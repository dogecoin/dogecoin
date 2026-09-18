// Copyright (c) 2017 The Bitcoin Core developers
// Copyright (c) 2026 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "wallet/coinselection.h"

#include <algorithm>

namespace {

struct CompareCandidateDescending
{
    bool operator()(const CInputCandidate& a, const CInputCandidate& b) const
    {
        // Break ties on the index so that the traversal order, and with it the
        // combination that gets picked, does not depend on the order in which
        // the caller happened to collect equal valued outputs.
        if (a.nValue != b.nValue)
            return a.nValue > b.nValue;
        return a.nIndex < b.nIndex;
    }
};

} // anonymous namespace

bool SelectCoinsBnB(std::vector<CInputCandidate> vCandidates,
                    const CAmount& nTargetValue,
                    const CAmount& nCostOfChange,
                    std::vector<size_t>& vIndicesRet,
                    CAmount& nValueRet)
{
    vIndicesRet.clear();
    nValueRet = 0;

    if (nTargetValue <= 0 || nCostOfChange < 0)
        return false;

    // Outputs worth nothing cannot bring a branch closer to the target, but they
    // would still be explored, so drop them up front.
    vCandidates.erase(std::remove_if(vCandidates.begin(), vCandidates.end(),
                                     [](const CInputCandidate& candidate) { return candidate.nValue <= 0; }),
                      vCandidates.end());

    // Value of the candidates that the search has not descended into yet. It is
    // what lets the search abandon a branch that can no longer reach the target.
    CAmount nRemainingValue = 0;
    for (const CInputCandidate& candidate : vCandidates)
        nRemainingValue += candidate.nValue;

    if (nRemainingValue < nTargetValue)
        return false;

    // Descending order makes the depth first search reach near exact
    // combinations early, which keeps the pruned tree shallow.
    std::sort(vCandidates.begin(), vCandidates.end(), CompareCandidateDescending());

    // vfSelection is the path from the root to the current node: one entry per
    // candidate the search has descended into, true if that candidate is part of
    // the combination being evaluated.
    std::vector<char> vfSelection;
    vfSelection.reserve(vCandidates.size());
    CAmount nCurrentValue = 0;

    std::vector<char> vfBest;
    CAmount nBestExcess = 0;

    for (size_t nTries = 0; nTries < BNB_MAX_TRIES; ++nTries) {
        bool fBacktrack = false;

        if (nCurrentValue + nRemainingValue < nTargetValue) {
            // Taking every remaining candidate would still fall short.
            fBacktrack = true;
        } else if (nCurrentValue > nTargetValue + nCostOfChange) {
            // Overshot the window, so the excess would have to become change.
            fBacktrack = true;
        } else if (nCurrentValue >= nTargetValue) {
            // Inside the window: a combination that needs no change output.
            const CAmount nExcess = nCurrentValue - nTargetValue;
            if (vfBest.empty() || nExcess < nBestExcess) {
                vfBest = vfSelection;
                vfBest.resize(vCandidates.size(), false);
                nBestExcess = nExcess;
            }
            if (nBestExcess == 0)
                break; // An exact match leaves nothing to improve on.
            fBacktrack = true;
        }

        if (fBacktrack) {
            // Climb back to the deepest candidate that is still included,
            // returning the candidates left behind to the remaining value.
            while (!vfSelection.empty() && !vfSelection.back()) {
                vfSelection.pop_back();
                nRemainingValue += vCandidates[vfSelection.size()].nValue;
            }
            if (vfSelection.empty())
                break; // The whole tree has been explored.

            // Explore the sibling branch that leaves that candidate out.
            vfSelection.back() = false;
            nCurrentValue -= vCandidates[vfSelection.size() - 1].nValue;
        } else {
            // Descend, including the next candidate.
            const CInputCandidate& candidate = vCandidates[vfSelection.size()];
            nRemainingValue -= candidate.nValue;

            if (!vfSelection.empty() && !vfSelection.back() &&
                    candidate.nValue == vCandidates[vfSelection.size() - 1].nValue) {
                // Skipping a candidate and then including an equally valued one
                // only reproduces a combination that has already been evaluated.
                vfSelection.push_back(false);
            } else {
                vfSelection.push_back(true);
                nCurrentValue += candidate.nValue;
            }
        }
    }

    if (vfBest.empty())
        return false;

    for (size_t i = 0; i < vfBest.size(); ++i) {
        if (vfBest[i]) {
            vIndicesRet.push_back(vCandidates[i].nIndex);
            nValueRet += vCandidates[i].nValue;
        }
    }

    return true;
}
