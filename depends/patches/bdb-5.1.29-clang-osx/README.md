These patches are to allow bdb-5.1.29 to compile using clang / c++11 under later MacOSX versions (Sierra+).
These patches must be applied to the bdb-5.1.29 source from Oracle, and then compiled, for the Mac client to build.

Note some of these exist in bdb.mk but we don't want to use the -stdlib=libstd++ flag that's indicated in there on OSX.
