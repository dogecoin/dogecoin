### Setting up Gitian on a VM using LXC ###

If your main machine is a 64-bit Mac or PC with a few gigabytes of memory
and at least 10 gigabytes of free disk space, you can `gitian-build` using
LXC running inside a virtual machine (VirtualBox, VMware, probably others too).

Here's a description of Gavin's setup on OSX 10.6:

1. Download and install VirtualBox from [https://www.virtualbox.org/](https://www.virtualbox.org/)

2. Download the 64-bit Ubuntu Desktop 12.04 LTS .iso CD image from
   [http://www.ubuntu.com/](http://www.ubuntu.com/)

3. Run VirtualBox and create a new virtual machine, using the Ubuntu .iso (see the [VirtualBox documentation](https://www.virtualbox.org/wiki/Documentation) for details). Create it with at least 2 gigabytes of memory and a disk that is at least 50 gigabytes big.

4. Inside the running Ubuntu desktop, install:

	sudo apt-get install debootstrap lxc ruby apache2 git apt-cacher-ng python-vm-builder

5. Still inside Ubuntu, tell gitian-builder to use LXC, then follow the instructions below:

	export USE_LXC=1

-------------------------------------------------------------------------

Sanity check:

    sudo service apt-cacher-ng status  # Should return apt-cacher-ng is running

Enable bridging interface for networking:

    sudo brctl addbr br0
    sudo ifconfig br0 10.0.2.2/24 up

Once you've got the right hardware and software:

    git clone git://github.com/dogecoin/dogecoin.git
    git clone git://github.com/devrandom/gitian-builder.git

Prepeare the builder VMs:

    cd gitian-builder
    bin/make-base-vm --arch i386
    bin/make-base-vm --arch amd64
    cd ..

Now follow the build process under doc/release-process to first build the dependecies
(one time task, or when the dependencies change) and then the client packagaes.