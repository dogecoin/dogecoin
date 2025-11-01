# 🐶 Dogecoin Core — Local Build & Test Guide

This short guide helps new contributors set up, build, and test Dogecoin Core locally.

## 🧱 Requirements
- Ubuntu 22.04+ or macOS 13+
- Python 3.8+
- Git
- Build essentials (make, g++, automake)
- Dependencies installed via `contrib/install_db4.sh` if needed

## ⚙️ Build
```bash
./autogen.sh
./configure
make
