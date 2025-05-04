#ifndef DOGECOIN_AMOUNT_H
#define DOGECOIN_AMOUNT_H

#include <stdint.h>
#include <string>
#include <limits>

typedef int64_t CAmount;
static const CAmount COIN = 100000000;
static const CAmount CENT = 1000000;

static_assert(std::numeric_limits<CAmount>::is_integer, "CAmount must be integral");

// Format an amount to a string
std::string FormatMoney(const CAmount& n);

// Parse a money string into amount
bool ParseMoney(const std::string& str, CAmount& nRet);

#endif // DOGECOIN_AMOUNT_H
