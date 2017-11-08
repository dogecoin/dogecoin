// Copyright (c) 2011-2016 The Bitcoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "sync.h"

#include "util.h"
#include "utilstrencodings.h"

#include <stdio.h>

#include <thread>
#include <unordered_map>
#include <utility>
#include <vector>
#include <set>
#include <map>
#include <mutex>

#ifdef DEBUG_LOCKCONTENTION
void PrintLockContention(const char* pszName, const char* pszFile, int nLine)
{
    LogPrintf("LOCKCONTENTION: %s\n", pszName);
    LogPrintf("Locker: %s:%d\n", pszFile, nLine);
}
#endif /* DEBUG_LOCKCONTENTION */

#ifdef DEBUG_LOCKORDER
//
// Early deadlock detection.
// Problem being solved:
//    Thread 1 locks  A, then B, then C
//    Thread 2 locks  D, then C, then A
//     --> may result in deadlock between the two threads, depending on when they run.
// Solution implemented here:
// Keep track of pairs of locks: (A before B), (A before C), etc.
// Complain if any thread tries to lock in a different order.
//

struct CLockLocation {
    CLockLocation(const char* pszName, const char* pszFile, int nLine, bool fTryIn)
    {
        mutexName = pszName;
        sourceFile = pszFile;
        sourceLine = nLine;
        fTry = fTryIn;
    }

    std::string ToString() const
    {
        return mutexName + "  " + sourceFile + ":" + itostr(sourceLine) + (fTry ? " (TRY)" : "");
    }

    std::string MutexName() const { return mutexName; }

    bool fTry;
private:
    std::string mutexName;
    std::string sourceFile;
    int sourceLine;
};

typedef std::pair<void*, CLockLocation> LockStackItem;
typedef std::vector<LockStackItem> LockStack;
typedef std::unordered_map<std::thread::id, LockStack> LockStacks;

typedef std::pair<void*, void*> LockPair;
typedef std::map<LockPair, LockStack> LockOrders;
typedef std::set<LockPair> InvLockOrders;

struct LockData {
    LockStacks m_lock_stacks;
    LockOrders lockorders;
    InvLockOrders invlockorders;
    std::mutex dd_mutex;
};

LockData& GetLockData() {
    static LockData& lock_data = *new LockData();
    return lock_data;
}

static void potential_deadlock_detected(const LockPair& mismatch, const LockStack& s1, const LockStack& s2)
{
    LogPrintf("POTENTIAL DEADLOCK DETECTED\n");
    LogPrintf("Previous lock order was:\n");
    for (const LockStackItem& i : s2) {
        if (i.first == mismatch.first) {
            LogPrintf(" (1)");
        }
        if (i.first == mismatch.second) {
            LogPrintf(" (2)");
        }
        LogPrintf(" %s\n", i.second.ToString());
    }
    LogPrintf("Current lock order is:\n");
    for (const LockStackItem& i : s1) {
        if (i.first == mismatch.first) {
            LogPrintf(" (1)");
        }
        if (i.first == mismatch.second) {
            LogPrintf(" (2)");
        }
        LogPrintf(" %s\n", i.second.ToString());
    }
    if (g_debug_lockorder_abort) {
        fprintf(stderr, "Assertion failed: detected inconsistent lock order at %s:%i, details in debug log.\n", __FILE__, __LINE__);
        abort();
    }
    throw std::logic_error("potential deadlock detected");
}

static void push_lock(void* c, const CLockLocation& locklocation, bool fTry)
{
    LockData& lockdata = GetLockData();
    std::lock_guard<std::mutex> lock(lockdata.dd_mutex);

    LockStack& lock_stack = lockdata.m_lock_stacks[std::this_thread::get_id()];
    lock_stack.emplace_back(c, locklocation);

    for (const LockStackItem& i : lock_stack) {
        if (i.first == c)
            break;

        const LockPair p1 = std::make_pair(i.first, c);
        if (lockdata.lockorders.count(p1))
            continue;
        lockdata.lockorders[p1] = lock_stack;

        const LockPair p2 = std::make_pair(c, i.first);
        lockdata.invlockorders.insert(p2);
        if (lockdata.lockorders.count(p2))
            potential_deadlock_detected(p1, lockdata.lockorders[p2], lockdata.lockorders[p1]);
    }
}

static void pop_lock()
{
    LockData& lockdata = GetLockData();
    std::lock_guard<std::mutex> lock(lockdata.dd_mutex);

    LockStack& lock_stack = lockdata.m_lock_stacks[std::this_thread::get_id()];
    lock_stack.pop_back();
    if (lock_stack.empty()) {
        lockdata.m_lock_stacks.erase(std::this_thread::get_id());
    }
}

void EnterCritical(const char* pszName, const char* pszFile, int nLine, void* cs, bool fTry)
{
    push_lock(cs, CLockLocation(pszName, pszFile, nLine, fTry), fTry);
}

void LeaveCritical()
{
    pop_lock();
}

std::string LocksHeld()
{
    LockData& lockdata = GetLockData();
    std::lock_guard<std::mutex> lock(lockdata.dd_mutex);

    const LockStack& lock_stack = lockdata.m_lock_stacks[std::this_thread::get_id()];

    std::string result;
    for (const LockStackItem& i : lock_stack) {
        result += i.second.ToString() + std::string("\n");
    }
    return result;
}

static bool LockHeld(void* mutex)
{
    LockData& lockdata = GetLockData();
    std::lock_guard<std::mutex> lock(lockdata.dd_mutex);

    const LockStack& lock_stack = lockdata.m_lock_stacks[std::this_thread::get_id()];
    for (const LockStackItem& i : lock_stack) {
        if (i.first == mutex) return true;
    }

    return false;
}

void AssertLockHeldInternal(const char* pszName, const char* pszFile, int nLine, void* cs)
{
    if (LockHeld(cs)) return;
    fprintf(stderr, "Assertion failed: lock %s not held in %s:%i; locks held:\n%s", pszName, pszFile, nLine, LocksHeld().c_str());
    abort();
}

void DeleteLock(void* cs)
{
    LockData& lockdata = GetLockData();
    std::lock_guard<std::mutex> lock(lockdata.dd_mutex);

    const LockPair item = std::make_pair(cs, (void*)0);
    LockOrders::iterator it = lockdata.lockorders.lower_bound(item);
    while (it != lockdata.lockorders.end() && it->first.first == cs) {
        const LockPair invitem = std::make_pair(it->first.second, it->first.first);
        lockdata.invlockorders.erase(invitem);
        lockdata.lockorders.erase(it++);
    }
    InvLockOrders::iterator invit = lockdata.invlockorders.lower_bound(item);
    while (invit != lockdata.invlockorders.end() && invit->first == cs) {
        const LockPair invinvitem = std::make_pair(invit->second, invit->first);
        lockdata.lockorders.erase(invinvitem);
        lockdata.invlockorders.erase(invit++);
    }
}

bool g_debug_lockorder_abort = true;

#endif /* DEBUG_LOCKORDER */
