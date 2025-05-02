
// Deterministic MT test case
BOOST_AUTO_TEST_CASE(mt_reward_determinism_test)
{
    uint256 fakeHash;
    fakeHash.SetHex("00000000aabbccddeeff00112233445566778899aabbccddeeff001122334455");
    boost::mt19937 gen;
    gen.seed(ArithToUint256(UintToArith256(fakeHash) >> 20).GetLow64());
    uint32_t reward = 1 + (gen() % 1000000);
    BOOST_CHECK_EQUAL(reward, 433017); // Update with accurate historical match
}
