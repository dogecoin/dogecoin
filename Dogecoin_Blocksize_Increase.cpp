// File: dogecoin-DOIP-XXXX.cpp

#include <dogecoin/dogecoin.h>

using namespace dogecoin;

class DOIPXXXX {
public:
    void implementBlockSizeIncrease(Network& network) {
        // Increase the block size to improve scalability
        network.setBlockSize(8 * 1024 * 1024);
    }
};

// Create an instance of the proposal and apply the change in main
int main() {
    Network network = Network::init();
    
    DOIPXXXX doip;
    doip.implementBlockSizeIncrease(network);

    network.start();
    // Wait for network to stabilize
    // ...

    network.stop();
    return 0;
}
