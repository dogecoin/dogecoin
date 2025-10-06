// Added log output for block synchronization progress
#include <logging.h>

void LogSyncProgress(double percentage) {
    LogPrintf("Synchronization progress: %.2f%% complete\n", percentage);
}
