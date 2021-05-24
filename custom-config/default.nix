self: {
  withHoogle = true;
  localCluster = {
    cacheDir    = "${self.localCluster.stateDir}/.cache";
    stateDir    = "state-cluster";
    profileName = "default-mary";
    basePort    = 30000;
    autoStartCluster = false;
    enableEKG        = true;
    workbenchDevMode = false;
    extraSupervisorConfig = {};
  };
}
