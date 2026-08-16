#ifndef DOGECOIN_ZKP_VERIFIER_H
#define DOGECOIN_ZKP_VERIFIER_H

#include <vector>

/**
 * Verify a Zero-Knowledge Proof.
 * 
 * @param proof The proof data.
 * @param public_inputs The public inputs for the circuit.
 * @return true if verification succeeds, false otherwise.
 */
bool VerifyZKP(const std::vector<unsigned char>& proof, const std::vector<unsigned char>& public_inputs);

#endif // DOGECOIN_ZKP_VERIFIER_H
