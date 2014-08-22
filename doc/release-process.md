Release Process
====================

* update translations (ping wumpus, Diapolo or tcatm on IRC)
* see https://github.com/bitcoin/bitcoin/blob/master/doc/translation_process.md#syncing-with-transifex

* * *

###update (commit) version in sources

	contrib/verifysfbinaries/verify.sh
	doc/README*
	share/setup.nsi
	src/clientversion.h (change CLIENT_VERSION_IS_RELEASE to true)

###tag version in git

	git tag -s v(new version, e.g. 0.8.0)

###write release notes. git shortlog helps a lot, for example:

	git shortlog --no-merges v(current version, e.g. 0.7.2)..v(new version, e.g. 0.8.0)

* * *

##perform gitian builds

 From a directory containing the dogecoin source, gitian-builder and gitian.sigs
  
	export SIGNER=(your gitian key, ie bluematt, sipa, etc)
	export VERSION=(new version, e.g. 0.8.0)
	pushd ./dogecoin
	git checkout v${VERSION}
	popd
	pushd ./gitian-builder

 Fetch and build inputs: (first time, or when dependency versions change)

	mkdir -p inputs; cd inputs/
	wget 'http://miniupnp.free.fr/files/download.php?file=miniupnpc-1.9.20140701.tar.gz' -O miniupnpc-1.9.20140701.tar.gz
	wget 'https://www.openssl.org/source/openssl-1.0.1i.tar.gz'
	wget 'http://download.oracle.com/berkeley-db/db-5.1.29.NC.tar.gz'
	wget 'http://zlib.net/zlib-1.2.8.tar.gz'
	wget 'https://downloads.sourceforge.net/project/libpng/libpng16/older-releases/1.6.8/libpng-1.6.8.tar.gz'
	wget 'https://fukuchi.org/works/qrencode/qrencode-3.4.3.tar.bz2'
	wget 'https://downloads.sourceforge.net/project/boost/boost/1.55.0/boost_1_55_0.tar.bz2'
	wget 'https://svn.boost.org/trac/boost/raw-attachment/ticket/7262/boost-mingw.patch' -O \
	     boost-mingw-gas-cross-compile-2013-03-03.patch
	wget 'https://download.qt-project.org/official_releases/qt/5.2/5.2.0/single/qt-everywhere-opensource-src-5.2.0.tar.gz'
	wget 'https://download.qt-project.org/archive/qt/4.6/qt-everywhere-opensource-src-4.6.4.tar.gz'
	wget 'https://protobuf.googlecode.com/files/protobuf-2.5.0.tar.bz2'
	cd ..
	./bin/gbuild ../dogecoin/contrib/gitian-descriptors/boost-linux.yml
	mv build/out/boost-*.zip inputs/
	./bin/gbuild ../dogecoin/contrib/gitian-descriptors/deps-linux.yml
	mv build/out/dogecoin-deps-*.zip inputs/
	./bin/gbuild ../dogecoin/contrib/gitian-descriptors/qt-linux.yml
	mv build/out/qt-*.tar.gz inputs/
	./bin/gbuild ../dogecoin/contrib/gitian-descriptors/boost-win.yml
	mv build/out/boost-*.zip inputs/
	./bin/gbuild ../dogecoin/contrib/gitian-descriptors/deps-win.yml
	mv build/out/dogecoin-deps-*.zip inputs/
	./bin/gbuild ../dogecoin/contrib/gitian-descriptors/qt-win.yml
	mv build/out/qt-*.zip inputs/
	./bin/gbuild ../dogecoin/contrib/gitian-descriptors/protobuf-win.yml
	mv build/out/protobuf-*.zip inputs/

 The expected SHA256 hashes of the intermediate inputs are:

    b66e8374031adf8d5309c046615fe4f561c3a7e3c1f6885675c13083db0c4d3b  dogecoin-deps-linux32-gitian-r8.zip
    ec83deb4e81bea5ac1fb5e3f1b88cd02ca665306f0c2290ef4f19b974525005e  dogecoin-deps-linux64-gitian-r8.zip
    f29b7d9577417333fb56e023c2977f5726a7c297f320b175a4108cf7cd4c2d29  boost-linux32-1.55.0-gitian-r1.zip
    88232451c4104f7eb16e469ac6474fd1231bd485687253f7b2bdf46c0781d535  boost-linux64-1.55.0-gitian-r1.zip
    57e57dbdadc818cd270e7e00500a5e1085b3bcbdef69a885f0fb7573a8d987e1  qt-linux32-4.6.4-gitian-r1.tar.gz
    60eb4b9c5779580b7d66529efa5b2836ba1a70edde2a0f3f696d647906a826be  qt-linux64-4.6.4-gitian-r1.tar.gz
    60dc2d3b61e9c7d5dbe2f90d5955772ad748a47918ff2d8b74e8db9b1b91c909  boost-win32-1.55.0-gitian-r6.zip
    f65fcaf346bc7b73bc8db3a8614f4f6bee2f61fcbe495e9881133a7c2612a167  boost-win64-1.55.0-gitian-r6.zip
    9c2572b021b3b50dc9441f2e96d672ac1da4cb6c9f88a1711aa0234882f353cf  dogecoin-deps-win32-gitian-r15.zip
    94e9f6d861140d9130a15830eba40eba4c8c830440506ac7cc0d1e3217293c25  dogecoin-deps-win64-gitian-r15.zip
    963e3e5e85879010a91143c90a711a5d1d5aba992e38672cdf7b54e42c56b2f1  qt-win32-5.2.0-gitian-r3.zip
    751c579830d173ef3e6f194e83d18b92ebef6df03289db13ab77a52b6bc86ef0  qt-win64-5.2.0-gitian-r3.zip
    e2e403e1a08869c7eed4d4293bce13d51ec6a63592918b90ae215a0eceb44cb4  protobuf-win32-2.5.0-gitian-r4.zip
    a0999037e8b0ef9ade13efd88fee261ba401f5ca910068b7e0cd3262ba667db0  protobuf-win64-2.5.0-gitian-r4.zip

 Build dogecoind and dogecoin-qt on Linux32, Linux64, and Win32:
  
	./bin/gbuild --commit dogecoin=v${VERSION} ../dogecoin/contrib/gitian-descriptors/gitian-linux.yml
	./bin/gsign --signer $SIGNER --release ${VERSION} --destination ../gitian.sigs/ ../dogecoin/contrib/gitian-descriptors/gitian-linux.yml
	pushd build/out
	zip -r dogecoin-${VERSION}-linux-gitian.zip *
	mv dogecoin-${VERSION}-linux-gitian.zip ../../../
	popd
	./bin/gbuild --commit dogecoin=v${VERSION} ../dogecoin/contrib/gitian-descriptors/gitian-win.yml
	./bin/gsign --signer $SIGNER --release ${VERSION}-win --destination ../gitian.sigs/ ../dogecoin/contrib/gitian-descriptors/gitian-win.yml
	pushd build/out
	zip -r dogecoin-${VERSION}-win-gitian.zip *
	mv dogecoin-${VERSION}-win-gitian.zip ../../../
	popd
	popd

  Build output expected:

  1. linux 32-bit and 64-bit binaries + source (dogecoin-${VERSION}-linux-gitian.zip)
  2. windows 32-bit and 64-bit binaries + installer + source (dogecoin-${VERSION}-win-gitian.zip)
  3. Gitian signatures (in gitian.sigs/${VERSION}[-win]/(your gitian key)/

repackage gitian builds for release as stand-alone zip/tar/installer exe

**Linux .tar.gz:**

	unzip dogecoin-${VERSION}-linux-gitian.zip -d dogecoin-${VERSION}-linux
	tar czvf dogecoin-${VERSION}-linux.tar.gz dogecoin-${VERSION}-linux
	rm -rf dogecoin-${VERSION}-linux

**Windows .zip and setup.exe:**

	unzip dogecoin-${VERSION}-win-gitian.zip -d dogecoin-${VERSION}-win
	mv dogecoin-${VERSION}-win/dogecoin-*-setup.exe .
	zip -r dogecoin-${VERSION}-win.zip dogecoin-${VERSION}-win
	rm -rf dogecoin-${VERSION}-win

**Perform Mac build:**

  OSX binaries are created by Gavin Andresen on a 64-bit, OSX 10.6 machine.

	./autogen.sh
        SDK=$(xcode-select --print-path)/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.6.sdk
        CXXFLAGS="-mmacosx-version-min=10.6 -isysroot $SDK" ./configure --enable-upnp-default
	make
	export QTDIR=/opt/local/share/qt4  # needed to find translations/qt_*.qm files
	T=$(contrib/qt_translations.py $QTDIR/translations src/qt/locale)
        export CODESIGNARGS='--keychain ...path_to_keychain --sign "Developer ID Application: DOGECOIN FOUNDATION, INC., THE"'
	python2.7 contrib/macdeploy/macdeployqtplus Dogecoin-Qt.app -sign -add-qt-tr $T -dmg -fancy contrib/macdeploy/fancy.plist

 Build output expected: Dogecoin-Qt.dmg

###Next steps:

* Code-sign Windows -setup.exe (in a Windows virtual machine using signtool)
 Note: only Gavin has the code-signing keys currently.

* upload builds to SourceForge

* create SHA256SUMS for builds, and PGP-sign it

* update dogecoin.com version
  make sure all OS download links go to the right versions
  
* update forum version

* update wiki download links

* update wiki changelog: [https://en.bitcoin.it/wiki/Changelog](https://en.bitcoin.it/wiki/Changelog)

Commit your signature to gitian.sigs:

	pushd gitian.sigs
	git add ${VERSION}-linux/${SIGNER}
	git add ${VERSION}-win/${SIGNER}
	git add ${VERSION}-osx/${SIGNER}
	git commit -a
	git push  # Assuming you can push to the gitian.sigs tree
	popd

-------------------------------------------------------------------------

### After 3 or more people have gitian-built and their results match:

From a directory containing dogecoin source, gitian.sigs and gitian zips

	export VERSION=(new version, e.g. 0.8.0)
	mkdir dogecoin-${VERSION}-linux-gitian
	pushd dogecoin-${VERSION}-linux-gitian
	unzip ../dogecoin-${VERSION}-linux-gitian.zip
	mkdir gitian
	cp ../dogecoin/contrib/gitian-downloader/*.pgp ./gitian/
	for signer in $(ls ../gitian.sigs/${VERSION}/); do
	 cp ../gitian.sigs/${VERSION}/${signer}/dogecoin-build.assert ./gitian/${signer}-build.assert
	 cp ../gitian.sigs/${VERSION}/${signer}/dogecoin-build.assert.sig ./gitian/${signer}-build.assert.sig
	done
	zip -r dogecoin-${VERSION}-linux-gitian.zip *
	cp dogecoin-${VERSION}-linux-gitian.zip ../
	popd
	mkdir dogecoin-${VERSION}-win-gitian
	pushd dogecoin-${VERSION}-win-gitian
	unzip ../dogecoin-${VERSION}-win-gitian.zip
	mkdir gitian
	cp ../dogecoin/contrib/gitian-downloader/*.pgp ./gitian/
	for signer in $(ls ../gitian.sigs/${VERSION}-win/); do
	 cp ../gitian.sigs/${VERSION}-win/${signer}/dogecoin-build.assert ./gitian/${signer}-build.assert
	 cp ../gitian.sigs/${VERSION}-win/${signer}/dogecoin-build.assert.sig ./gitian/${signer}-build.assert.sig
	done
	zip -r dogecoin-${VERSION}-win-gitian.zip *
	cp dogecoin-${VERSION}-win-gitian.zip ../
	popd

    - Code-sign MacOSX .dmg

  Note: only Gavin has the code-signing keys currently.

- Create `SHA256SUMS.asc` for builds, and PGP-sign it. This is done manually.
  Include all the files to be uploaded. The file has `sha256sum` format with a
  simple header at the top:

```
Hash: SHA256

0060f7d38b98113ab912d4c184000291d7f026eaf77ca5830deec15059678f54  bitcoin-x.y.z-linux.tar.gz
...
```

- Upload gitian zips to SourceForge

- Announce the release:

  - Add the release to dogecoin.com

  - Announce on reddit /r/dogecoin, /r/dogecoindev

  - Release sticky on discuss dogecoin: https://discuss.dogecoin.com/categories/announcements

- Celebrate 
