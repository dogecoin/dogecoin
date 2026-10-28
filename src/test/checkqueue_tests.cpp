// Copyright (c) 2012-2017 The Bitcoin Core developers
// Copyright (c) 2026 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "util.h"
#include "utiltime.h"
#include "validation.h"

#include "test/test_bitcoin.h"
#include "checkqueue.h"
#include <boost/test/unit_test.hpp>
#include <boost/thread.hpp>
#include <atomic>
#include <thread>
#include <vector>
#include <mutex>
#include <condition_variable>
#include <future>

#include <unordered_set>
#include <memory>
#include "random.h"

// BasicTestingSetup not sufficient because nScriptCheckThreads is not set
// otherwise.
BOOST_FIXTURE_TEST_SUITE(checkqueue_tests, TestingSetup)

static const int QUEUE_BATCH_SIZE = 128;

struct FakeCheck {
    bool operator()()
    {
        return true;
    }
    void swap(FakeCheck& x){};
};

struct FakeCheckCheckCompletion {
    static std::atomic<size_t> n_calls;
    bool operator()()
    {
        ++n_calls;
        return true;
    }
    void swap(FakeCheckCheckCompletion& x){};
};

struct FailingCheck {
    bool fails;
    FailingCheck(bool fails) : fails(fails){};
    FailingCheck() : fails(true){};
    bool operator()()
    {
        return !fails;
    }
    void swap(FailingCheck& x)
    {
        std::swap(fails, x.fails);
    };
};

struct UniqueCheck {
    static std::mutex m;
    static std::unordered_multiset<size_t> results;
    size_t check_id;
    UniqueCheck(size_t check_id_in) : check_id(check_id_in){};
    UniqueCheck() : check_id(0){};
    bool operator()()
    {
        std::lock_guard<std::mutex> l(m);
        results.insert(check_id);
        return true;
    }
    void swap(UniqueCheck& x) { std::swap(x.check_id, check_id); };
};


struct MemoryCheck {
    static std::atomic<size_t> fake_allocated_memory;
    bool b {false};
    bool operator()()
    {
        return true;
    }
    MemoryCheck(){};
    MemoryCheck(const MemoryCheck& x)
    {
        // We have to do this to make sure that destructor calls are paired
        //
        // Really, copy constructor should be deletable, but CCheckQueue breaks
        // if it is deleted because of internal push_back.
        fake_allocated_memory += b;
    };
    MemoryCheck(bool b_) : b(b_)
    {
        fake_allocated_memory += b;
    };
    ~MemoryCheck(){
        fake_allocated_memory -= b;

    };
    void swap(MemoryCheck& x) { std::swap(b, x.b); };
};

struct FrozenCleanupCheck {
    static std::atomic<uint64_t> nFrozen;
    static std::condition_variable cv;
    static std::mutex m;
    // Freezing can't be the default initialized behavior given how the queue
    // swaps in default initialized Checks.
    bool should_freeze {false};
    bool operator()()
    {
        return true;
    }
    FrozenCleanupCheck() {}
    ~FrozenCleanupCheck()
    {
        if (should_freeze) {
            std::unique_lock<std::mutex> l(m);
            nFrozen = 1;
            cv.notify_one();
            cv.wait(l, []{ return nFrozen == 0;});
        }
    }
    void swap(FrozenCleanupCheck& x){std::swap(should_freeze, x.should_freeze);};
};

/**
 * A check that, like CScriptCheck, only ever touches caller-owned data
 * through a raw, non-owning pointer. `pDestroyed` stands in for a pointer
 * into caller-owned data (e.g. ConnectBlock()'s `txdata`); the check
 * records whether that data had already been marked destroyed by the time
 * it got around to reading it.
 *
 * `pStarted`/`proceed` let the test synchronize deterministically with a
 * worker thread instead of relying on real-time races: the check signals
 * once it has been picked up by a worker, then blocks until the test tells
 * it to continue.
 */
struct DestructOrderCheck {
    std::atomic<bool>* pDestroyed {nullptr};
    std::atomic<bool>* pObservedAfterDestruction {nullptr};
    std::promise<void>* pStarted {nullptr};
    std::shared_future<void> proceed {};

    DestructOrderCheck() {}
    DestructOrderCheck(std::atomic<bool>* destroyed, std::atomic<bool>* observed,
                        std::promise<void>* started, std::shared_future<void> proceedIn)
        : pDestroyed(destroyed), pObservedAfterDestruction(observed), pStarted(started), proceed(proceedIn) {}

    bool operator()()
    {
        if (pStarted) {
            pStarted->set_value();
        }
        if (proceed.valid()) {
            proceed.wait();
        }
        // Mirrors CScriptCheck::operator() dereferencing its raw pointer.
        if (pDestroyed && pDestroyed->load()) {
            if (pObservedAfterDestruction) {
                pObservedAfterDestruction->store(true);
            }
        }
        return true;
    }
    void swap(DestructOrderCheck& x)
    {
        std::swap(pDestroyed, x.pDestroyed);
        std::swap(pObservedAfterDestruction, x.pObservedAfterDestruction);
        std::swap(pStarted, x.pStarted);
        std::swap(proceed, x.proceed);
    }
};

/**
 * Stands in for caller-owned data referenced by a raw pointer handed out to
 * worker checks (e.g. an element of ConnectBlock()'s `txdata`). Marks
 * `*pDestroyed` on destruction instead of actually freeing memory, so the
 * test can detect a check reading past the data's lifetime.
 */
struct DestructionSentinel {
    std::atomic<bool>* pDestroyed;
    explicit DestructionSentinel(std::atomic<bool>* d) : pDestroyed(d) {}
    ~DestructionSentinel() { pDestroyed->store(true); }
};


// Static Allocations
std::mutex FrozenCleanupCheck::m{};
std::atomic<uint64_t> FrozenCleanupCheck::nFrozen{0};
std::condition_variable FrozenCleanupCheck::cv{};
std::mutex UniqueCheck::m;
std::unordered_multiset<size_t> UniqueCheck::results;
std::atomic<size_t> FakeCheckCheckCompletion::n_calls{0};
std::atomic<size_t> MemoryCheck::fake_allocated_memory{0};

// Queue Typedefs
typedef CCheckQueue<FakeCheckCheckCompletion> Correct_Queue;
typedef CCheckQueue<FakeCheck> Standard_Queue;
typedef CCheckQueue<FailingCheck> Failing_Queue;
typedef CCheckQueue<UniqueCheck> Unique_Queue;
typedef CCheckQueue<MemoryCheck> Memory_Queue;
typedef CCheckQueue<FrozenCleanupCheck> FrozenCleanup_Queue;
typedef CCheckQueue<DestructOrderCheck> DestructOrder_Queue;


/** This test case checks that the CCheckQueue works properly
 * with each specified size_t Checks pushed.
 */
void Correct_Queue_range(std::vector<size_t> range)
{
    auto small_queue = std::unique_ptr<Correct_Queue>(new Correct_Queue {QUEUE_BATCH_SIZE});
    boost::thread_group tg;
    for (auto x = 0; x < nScriptCheckThreads; ++x) {
       tg.create_thread([&]{small_queue->Thread();});
    }
    // Make vChecks here to save on malloc (this test can be slow...)
    std::vector<FakeCheckCheckCompletion> vChecks;
    for (auto i : range) {
        size_t total = i;
        FakeCheckCheckCompletion::n_calls = 0;
        CCheckQueueControl<FakeCheckCheckCompletion> control(small_queue.get());
        while (total) {
            vChecks.resize(std::min(total, (size_t) InsecureRandRange(10)));
            total -= vChecks.size();
            control.Add(vChecks);
        }
        BOOST_REQUIRE(control.Wait());
        if (FakeCheckCheckCompletion::n_calls != i) {
            BOOST_REQUIRE_EQUAL(FakeCheckCheckCompletion::n_calls, i);
            BOOST_TEST_MESSAGE("Failure on trial " << i << " expected, got " << FakeCheckCheckCompletion::n_calls);
        }
    }
    tg.interrupt_all();
    tg.join_all();
}

/** Test that 0 checks is correct
 */
BOOST_AUTO_TEST_CASE(test_CheckQueue_Correct_Zero)
{
    std::vector<size_t> range;
    range.push_back((size_t)0);
    Correct_Queue_range(range);
}
/** Test that 1 check is correct
 */
BOOST_AUTO_TEST_CASE(test_CheckQueue_Correct_One)
{
    std::vector<size_t> range;
    range.push_back((size_t)1);
    Correct_Queue_range(range);
}
/** Test that MAX check is correct
 */
BOOST_AUTO_TEST_CASE(test_CheckQueue_Correct_Max)
{
    std::vector<size_t> range;
    range.push_back(100000);
    Correct_Queue_range(range);
}
/** Test that random numbers of checks are correct
 */
BOOST_AUTO_TEST_CASE(test_CheckQueue_Correct_Random)
{
    std::vector<size_t> range;
    range.reserve(100000/1000);
    for (size_t i = 2; i < 100000; i += std::max((size_t)1, (size_t)InsecureRandRange(std::min((size_t)1000, ((size_t)100000) - i))))
        range.push_back(i);
    Correct_Queue_range(range);
}


/** Test that failing checks are caught */
BOOST_AUTO_TEST_CASE(test_CheckQueue_Catches_Failure)
{
    auto fail_queue = std::unique_ptr<Failing_Queue>(new Failing_Queue {QUEUE_BATCH_SIZE});

    boost::thread_group tg;
    for (auto x = 0; x < nScriptCheckThreads; ++x) {
       tg.create_thread([&]{fail_queue->Thread();});
    }

    for (size_t i = 0; i < 1001; ++i) {
        CCheckQueueControl<FailingCheck> control(fail_queue.get());
        size_t remaining = i;
        while (remaining) {
            size_t r = InsecureRandRange(10);

            std::vector<FailingCheck> vChecks;
            vChecks.reserve(r);
            for (size_t k = 0; k < r && remaining; k++, remaining--)
                vChecks.emplace_back(remaining == 1);
            control.Add(vChecks);
        }
        bool success = control.Wait();
        if (i > 0) {
            BOOST_REQUIRE(!success);
        } else if (i == 0) {
            BOOST_REQUIRE(success);
        }
    }
    tg.interrupt_all();
    tg.join_all();
}
// Test that a block validation which fails does not interfere with
// future blocks, ie, the bad state is cleared.
BOOST_AUTO_TEST_CASE(test_CheckQueue_Recovers_From_Failure)
{
    auto fail_queue = std::unique_ptr<Failing_Queue>(new Failing_Queue {QUEUE_BATCH_SIZE});
    boost::thread_group tg;
    for (auto x = 0; x < nScriptCheckThreads; ++x) {
       tg.create_thread([&]{fail_queue->Thread();});
    }

    for (auto times = 0; times < 10; ++times) {
        for (bool end_fails : {true, false}) {
            CCheckQueueControl<FailingCheck> control(fail_queue.get());
            {
                std::vector<FailingCheck> vChecks;
                vChecks.resize(100, false);
                vChecks[99] = end_fails;
                control.Add(vChecks);
            }
            bool r =control.Wait();
            BOOST_REQUIRE(r || end_fails);
        }
    }
    tg.interrupt_all();
    tg.join_all();
}

// Test that unique checks are actually all called individually, rather than
// just one check being called repeatedly. Test that checks are not called
// more than once as well
BOOST_AUTO_TEST_CASE(test_CheckQueue_UniqueCheck)
{
    auto queue = std::unique_ptr<Unique_Queue>(new Unique_Queue {QUEUE_BATCH_SIZE});
    boost::thread_group tg;
    for (auto x = 0; x < nScriptCheckThreads; ++x) {
       tg.create_thread([&]{queue->Thread();});

    }

    size_t COUNT = 100000;
    size_t total = COUNT;
    {
        CCheckQueueControl<UniqueCheck> control(queue.get());
        while (total) {
            size_t r = InsecureRandRange(10);
            std::vector<UniqueCheck> vChecks;
            for (size_t k = 0; k < r && total; k++)
                vChecks.emplace_back(--total);
            control.Add(vChecks);
        }
    }
    bool r = true;
    BOOST_REQUIRE_EQUAL(UniqueCheck::results.size(), COUNT);
    for (size_t i = 0; i < COUNT; ++i)
        r = r && UniqueCheck::results.count(i) == 1;
    BOOST_REQUIRE(r);
    tg.interrupt_all();
    tg.join_all();
}


// Test that blocks which might allocate lots of memory free their memory aggressively.
//
// This test attempts to catch a pathological case where by lazily freeing
// checks might mean leaving a check un-swapped out, and decreasing by 1 each
// time could leave the data hanging across a sequence of blocks.
BOOST_AUTO_TEST_CASE(test_CheckQueue_Memory)
{
    auto queue = std::unique_ptr<Memory_Queue>(new Memory_Queue {QUEUE_BATCH_SIZE});
    boost::thread_group tg;
    for (auto x = 0; x < nScriptCheckThreads; ++x) {
       tg.create_thread([&]{queue->Thread();});
    }
    for (size_t i = 0; i < 1000; ++i) {
        size_t total = i;
        {
            CCheckQueueControl<MemoryCheck> control(queue.get());
            while (total) {
                size_t r = InsecureRandRange(10);
                std::vector<MemoryCheck> vChecks;
                for (size_t k = 0; k < r && total; k++) {
                    total--;
                    // Each iteration leaves data at the front, back, and middle
                    // to catch any sort of deallocation failure
                    vChecks.emplace_back(total == 0 || total == i || total == i/2);
                }
                control.Add(vChecks);
            }
        }
        BOOST_REQUIRE_EQUAL(MemoryCheck::fake_allocated_memory, 0);
    }
    tg.interrupt_all();
    tg.join_all();
}

// Test that a new verification cannot occur until all checks
// have been destructed
BOOST_AUTO_TEST_CASE(test_CheckQueue_FrozenCleanup)
{
    auto queue = std::unique_ptr<FrozenCleanup_Queue>(new FrozenCleanup_Queue {QUEUE_BATCH_SIZE});
    boost::thread_group tg;
    bool fails = false;
    for (auto x = 0; x < nScriptCheckThreads; ++x) {
        tg.create_thread([&]{queue->Thread();});
    }
    std::thread t0([&]() {
        CCheckQueueControl<FrozenCleanupCheck> control(queue.get());
        std::vector<FrozenCleanupCheck> vChecks(1);
        // Freezing can't be the default initialized behavior given how the queue
        // swaps in default initialized Checks (otherwise freezing destructor
        // would get called twice).
        vChecks[0].should_freeze = true;
        control.Add(vChecks);
        control.Wait(); // Hangs here
    });
    {
        std::unique_lock<std::mutex> l(FrozenCleanupCheck::m);
        // Wait until the queue has finished all jobs and frozen
        FrozenCleanupCheck::cv.wait(l, [](){return FrozenCleanupCheck::nFrozen == 1;});
        // Try to get control of the queue a bunch of times
        for (auto x = 0; x < 100 && !fails; ++x) {
            fails = queue->ControlMutex.try_lock();
        }
        // Unfreeze
        FrozenCleanupCheck::nFrozen = 0;
    }
    // Awaken frozen destructor
    FrozenCleanupCheck::cv.notify_one();
    // Wait for control to finish
    t0.join();
    tg.interrupt_all();
    tg.join_all();
    BOOST_REQUIRE(!fails);
}


BOOST_AUTO_TEST_CASE(test_CheckQueueControlWaiter_prevents_destruction_before_join)
{
    auto queue = std::unique_ptr<DestructOrder_Queue>(new DestructOrder_Queue{QUEUE_BATCH_SIZE});
    boost::thread_group tg;
    for (auto x = 0; x < nScriptCheckThreads; ++x) {
        tg.create_thread([&]{ queue->Thread(); });
    }

    // Vulnerable shape: control declared before the data, relying on
    // ~CCheckQueueControl() to Wait(). The data is destroyed (here,
    // simulated via DestructionSentinel) before that destructor runs.
    {
        std::atomic<bool> destroyed{false};
        std::atomic<bool> observedAfterDestruction{false};
        std::promise<void> started;
        std::promise<void> proceed;
        std::shared_future<void> proceedFuture(proceed.get_future());

        {
            CCheckQueueControl<DestructOrderCheck> control(queue.get());
            {
                DestructionSentinel sentinel(&destroyed);
                std::vector<DestructOrderCheck> vChecks;
                vChecks.emplace_back(&destroyed, &observedAfterDestruction, &started, proceedFuture);
                control.Add(vChecks);

                // Wait until the worker has picked up the check and is
                // blocked inside operator(), mirroring an in-flight
                // CScriptCheck.
                started.get_future().wait();

                // Scope ends here: `sentinel` is destroyed now, simulating
                // an early return in ConnectBlock() that destroys `txdata`
                // before the explicit control.Wait() is reached.
            }
            // Let the worker proceed to read; it will observe destroyed==true.
            proceed.set_value();
            // ~CCheckQueueControl() runs here and joins the worker, but only
            // after it already read past-destruction state.
        }
        BOOST_CHECK(observedAfterDestruction);
    }

    // Fixed shape: CCheckQueueControlWaiter declared after the data, so it
    // destructs (and joins the worker) before the data does -- no matter
    // how the scope is exited.
    {
        std::atomic<bool> destroyed{false};
        std::atomic<bool> observedAfterDestruction{false};
        std::promise<void> started;
        std::promise<void> proceed;
        std::shared_future<void> proceedFuture(proceed.get_future());

        {
            CCheckQueueControl<DestructOrderCheck> control(queue.get());
            DestructionSentinel sentinel(&destroyed);
            CCheckQueueControlWaiter<DestructOrderCheck> waiter(control);
            std::vector<DestructOrderCheck> vChecks;
            vChecks.emplace_back(&destroyed, &observedAfterDestruction, &started, proceedFuture);
            control.Add(vChecks);

            started.get_future().wait();
            // Tell the worker to proceed. Even though `destroyed` is still
            // false right now, the assertion below doesn't rely on timing:
            // `waiter`'s destructor (which joins the worker) is guaranteed
            // to run to completion before `sentinel`'s destructor (which
            // sets destroyed=true) can run, because `waiter` is declared
            // after `sentinel` and C++ destroys locals in reverse order.
            proceed.set_value();
            // Scope ends: waiter destructs first (joins the worker),
            // then sentinel destructs (destroyed=true) only afterward.
        }
        BOOST_CHECK(!observedAfterDestruction);
    }

    tg.interrupt_all();
    tg.join_all();
}


/** Test that CCheckQueueControl is threadsafe */
BOOST_AUTO_TEST_CASE(test_CheckQueueControl_Locks)
{
    auto queue = std::unique_ptr<Standard_Queue>(new Standard_Queue{QUEUE_BATCH_SIZE});
    {
        boost::thread_group tg;
        std::atomic<int> nThreads {0};
        std::atomic<int> fails {0};
        for (size_t i = 0; i < 3; ++i) {
            tg.create_thread(
                    [&]{
                    CCheckQueueControl<FakeCheck> control(queue.get());
                    // While sleeping, no other thread should execute to this point
                    auto observed = ++nThreads;
                    MilliSleep(10);
                    fails += observed  != nThreads;
                    });
        }
        tg.join_all();
        BOOST_REQUIRE_EQUAL(fails, 0);
    }
    {
        boost::thread_group tg;
        std::mutex m;
        bool has_lock {false};
        bool has_tried {false};
        bool done {false};
        bool done_ack {false};
        std::condition_variable cv;
        {
            std::unique_lock<std::mutex> l(m);
            tg.create_thread([&]{
                    CCheckQueueControl<FakeCheck> control(queue.get());
                    std::unique_lock<std::mutex> l(m);
                    has_lock = true;
                    cv.notify_one();
                    cv.wait(l, [&]{return has_tried;});
                    done = true;
                    cv.notify_one();
                    // Wait until the done is acknowledged
                    //
                    cv.wait(l, [&]{return done_ack;});
                    });
            // Wait for thread to get the lock
            cv.wait(l, [&](){return has_lock;});
            bool fails = false;
            for (auto x = 0; x < 100 && !fails; ++x) {
                fails = queue->ControlMutex.try_lock();
            }
            has_tried = true;
            cv.notify_one();
            cv.wait(l, [&](){return done;});
            // Acknowledge the done
            done_ack = true;
            cv.notify_one();
            BOOST_REQUIRE(!fails);
        }
        tg.join_all();
    }
}
BOOST_AUTO_TEST_SUITE_END()

