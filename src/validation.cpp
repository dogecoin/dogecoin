
// Patched validation.cpp fragment: Consensus lock on fSimplifiedRewards
// ...
bool CheckSimplifiedRewardsLock(int nHeight, const Consensus::Params& params) {
    if (!params.fSimplifiedRewards && nHeight >= 145000) {
        return error("Consensus violation: fSimplifiedRewards must remain enabled at height %d", nHeight);
    }
    return true;
}

bool ContextualCheckBlock(...) {
    // Inserted at start of validation block
    if (!CheckSimplifiedRewardsLock(nHeight, params)) {
        return state.DoS(100, false, REJECT_INVALID, "simplified-rewards-locked");
    }
    // ...
}
