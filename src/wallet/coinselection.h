// Copyright (c) 2017 The Bitcoin Core developers
// Copyright (c) 2026 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#ifndef BITCOIN_WALLET_COINSELECTION_H
#define BITCOIN_WALLET_COINSELECTION_H

#include "amount.h"

#include <stddef.h>
#include <vector>

/**
 * A spendable output offered to the branch and bound solver.
 *
 * nIndex is opaque to the solver and is handed back unchanged as part of the
 * solution, so that the solver needs to know nothing about wallet types and can
 * be exercised on its own in the unit tests.
 */
struct CInputCandidate
{
    CAmount nValue;
    size_t nIndex;

    CInputCandidate(const CAmount& nValueIn, size_t nIndexIn) : nValue(nValueIn), nIndex(nIndexIn) {}
};

/** Nodes the solver may visit before it gives up and lets the caller fall back. */
static const size_t BNB_MAX_TRIES = 100000;

/**
 * Search for a combination of candidates that pays nTargetValue while
 * overshooting it by no more than nCostOfChange, i.e. a combination that needs
 * no change output at all.
 *
 * The search is an exhaustive depth first traversal of the inclusion/exclusion
 * tree over the candidates sorted by descending value, pruned on three
 * conditions: the branch can no longer reach the target, the branch has
 * overshot the window, or an exact match has already been found. It is
 * deterministic and, unlike a stochastic approximation, it will find a
 * changeless combination whenever one exists within BNB_MAX_TRIES nodes.
 *
 * Of the changeless combinations found, the one overshooting nTargetValue by the
 * least is returned, as the overshoot is paid to the miner rather than kept.
 * Combinations that overshoot by the same amount are settled by traversal order,
 * which visits the candidates from largest to smallest and therefore reaches the
 * combination built from the fewest, largest outputs first: the one that makes
 * for the smallest transaction and so the smallest fee.
 *
 * Candidates with a value of zero or less are ignored.
 *
 * @param[in]  vCandidates    The outputs to choose from.
 * @param[in]  nTargetValue   The amount to pay, including the fee budget.
 * @param[in]  nCostOfChange  The largest acceptable overshoot of nTargetValue.
 * @param[out] vIndicesRet    nIndex of each chosen candidate, on success.
 * @param[out] nValueRet      Summed value of the chosen candidates, on success.
 * @return true if a changeless combination was found.
 */
bool SelectCoinsBnB(std::vector<CInputCandidate> vCandidates,
                    const CAmount& nTargetValue,
                    const CAmount& nCostOfChange,
                    std::vector<size_t>& vIndicesRet,
                    CAmount& nValueRet);

#endif // BITCOIN_WALLET_COINSELECTION_H
