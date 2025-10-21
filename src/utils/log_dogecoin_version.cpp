// Copyright (c) 2025
// Simple utility to print Dogecoin Core version info
// Author: Marta

#include <iostream>
#include "clientversion.h"

namespace dogeutils {

/**
 * Prints the current Dogecoin Core version in a friendly format.
 * Intended for debugging or verifying build information.
 */
void PrintCoreVersion()
{
    std::cout << "🐶 Dogecoin Core Version: " << CLIENT_VERSION_MAJOR << "."
              << CLIENT_VERSION_MINOR << "."
              << CLIENT_VERSION_REVISION << std::endl;
}

} // namespace dogeutils
