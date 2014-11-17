#!/bin/bash

if [ -e boost_1_55_0.tar.bz2 ]
then
    echo bunzipping ... wait for it!
    bunzip2 boost_1_55_0.tar.bz2
    echo de-tarring ... moar waiting!
    tar -xf boost_1_55_0.tar
    mv boost_1_55_0/boost .
    echo cleaning up mess...
    rm -rf boost_1_55_0
    rm boost_1_55_0.tar
    echo patching boost...
    patch -u -p0 -R -i boostpatch.diff
    echo All done!
else
echo Go get boost from http://sourceforge.net/projects/boost/files/boost/1.55.0/boost_1_55_0.tar.bz2/download and put the zip file in this dir.
fi
