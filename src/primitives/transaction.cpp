
// Updated transaction.cpp with GetWitnessHash implementation
uint256 CTransaction::GetWitnessHash() const {
    if (HasWitness()) {
        return SerializeHash(*this, SER_GETHASH | SERIALIZE_TRANSACTION_WITNESS);
    }
    return GetHash();
}
