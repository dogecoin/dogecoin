#include <iostream>
#include <chrono>
#include <thread>
#include <dogecoin/dogecoin.h>

using namespace std;
using namespace dogecoin;

int main() {
   // Initialize the Dogecoin network
   Network network = Network::init();

   // Reduce the block time to the lowest possible value to minimize latency
   network.setBlockTime(2);

   // Enable parallel block validation to speed up network propagation
   network.enableParallelValidation();

   // Use the fastest mining algorithm to reduce latency
   network.useFastestMiningAlgo();

   // Optimize network communication for low latency
   network.optimizeForLowLatency();

   // Start the network
   network.start();

   // Wait for network to stabilize
   this_thread::sleep_for(chrono::seconds(30));

   // Check network latency
   cout << "Network latency: " << network.getLatency() << "ms" << endl;

   // Stop the network
   network.stop();

   return 0;
}
