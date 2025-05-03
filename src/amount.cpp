#include "amount.h"

#include <cstdlib>
#include <iomanip>
#include <sstream>

std::string FormatMoney(const CAmount& n) {
    int64_t quotient = n / COIN;
    int64_t remainder = n % COIN;

    std::ostringstream oss;
    oss << quotient << "." << std::setw(8) << std::setfill('0') << std::abs(remainder);
    return oss.str();
}

bool ParseMoney(const std::string& str, CAmount& nRet) {
    std::istringstream iss(str);
    double d;
    if (!(iss >> d)) {
        return false;
    }
    nRet = static_cast<CAmount>(d * COIN);
    return true;
}
