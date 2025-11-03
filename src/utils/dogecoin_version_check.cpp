// Copyright (c) 2025
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include <iostream>
#include <string>

namespace dogecoin {

/**
 * Simple utility to print the current Dogecoin Core version.
 * Intended for quick verification in development and CI scripts.
 */
void PrintVersionInfo(const std::string& version) {
    std::cout << "Dogecoin Core version: " << version << std::endl;
}

} // namespace dogecoin

#ifdef DOGE_VERSION_MAIN
int main() {
    dogecoin::PrintVersionInfo("v1.14.8-dev");
    return 0;
}
#endif
