#ifndef BITCOIN_COMMON_PCP_H
#define BITCOIN_COMMON_PCP_H

#include <netaddress.h>
#include <string>
#include <vector>

namespace pcp {

struct PCPResult {
    bool success;
    std::string error;
};

PCPResult MapPort(const CService& internal, const CService& external, const std::string& description, int lifetime);
PCPResult UnmapPort(const CService& internal, const CService& external);

} // namespace pcp

#endif // BITCOIN_COMMON_PCP_H
