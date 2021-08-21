# 狗狗币核心 （Dogecoin Core） [DOGE, Ð]

![Dogecoin](https://static.tumblr.com/ppdj5y9/Ae9mxmxtp/300coin.png)

[![Build Status](https://travis-ci.com/dogecoin/dogecoin.svg?branch=master)](https://travis-ci.com/dogecoin/dogecoin)

选择文档语言: [英文](../README.md) | 简体中文 | [其他语言...](./README_zh_CN.md)

尽管狗狗币不使用SHA256作为它的工作量证明（POW），狗狗币也是一款类似Bitcoin的加密货币。受到Tenebrix和Litecoin的影响，狗狗币目前采用一种更简洁的加密方式。
- **网址:** [dogecoin.com.](https://dogecoin.com)

## 证书 – 炫酷的证书 ⚖️
狗狗币在MIT liscence的条款下发行。详情请参见
[COPYING](COPYING) 或者
[opensource.org](https://opensource.org/licenses/MIT)。

## 开发和贡献 – omg 开发者
目前的研发还在进行中，开发团队以及其他志愿者可以随意通过自己的代码库分支发布pull request。

#### 版本说明
版本号码遵循以下语法：```major.minor.patch```。

#### 代码库分支(branch)
本代码库有3个branch：

- **master（主代码库）:** 稳定。包含最新版本的release，以*major.minor*形式呈现。
- **maintenance（维护代码库）:** 稳定。包含正在维护中的上一个release的最新版本。格式： ```<version>-maint```
- **development（正在开发代码库）:** 不稳定。包含下一个release的最新代码。格式： ```<version>-dev```

*Master 和 maintenance 互不干扰。最新release永远包含一个development分支。新的 pull request 应该发布于此。Maintenance 分支**只能用于调试程序**。请将新开发的功能发布在 development 分支的最高版本。*

#### 贡献 ✍️

鼓励开发者为新代码来创建自己的[单元测试](src/test/README.md)，或者为旧代码发布新的单元测试。用如下指令编译并运行单元测试（仅限于单元测试在配置中没有被禁用）：`make check`。更多关于运行和开发单元测试的信息请查看：[/src/test/README.md](/src/test/README.md).

RPC界面上还有由Python编写的[回归和整合测试（regression and integration tests）](/qa) ，可以在build服务器上自动运行。这些测试还可以通过以下指令来运行：`qa/pull-tester/rpc-tests.py`（前提是已经安装了[测试依赖文件（test dependencies）](/qa)）。

代码的改变还应该由除开发者之外的其他人员进行测试。这一点对于大段改动和高危变化尤其重要。建议在pull request的描述中加入测试文档(test plan)，如果测试方式并不很直接。

## 灰常常见的问题 ❓

### 到底一共可以有多少狗狗币? – 狗狗多多! 🐕
截至2015年初（大概发行一年半的时间）会有100,000,000,000狗狗币。
之后的每个区块（block）将会带来10,000狗狗币来激励矿工们持续输出，增加挖矿的安全性并且对因丢失的钱包、硬盘、手机、密码等而丢失的狗狗币做出相应弥补。 

### 挖矿须知 ⛏

狗狗币采用一种简化版的密钥派生函数作为它的工作量证明。目标时间为每分钟产出一个区块，每产出一个区块后都会进行难度值调整。区块奖励是固定的，且每产出100,000个区块就会减半。从第600,000个区块开始，每产出一个区块的奖励会固定在10,000个狗狗币。

起初，我们设想了一种不同的支付方式，由Mersenne Twister伪随机数生成器在0到区块计划相应的最大区块奖励中生成一个随机数作为区块奖励。

然而在第145,000区块上，为了防止大矿池钻空子只挖高奖励的区块，这种方案就不适用了。与此同时，难度值调整也从每4小时变为每个区块产生（每分钟）更改一次，使用的是DigiByte Coin开发团队的一个算法，以减少狗狗币网络中哈希率突然增大或减少带来的影响。

**目前的区块奖励机制:**

1–99,999: 0–1,000,000 Dogecoin

100,000–144,999: 0–500,000 Dogecoin

145,000–199,999: 250,000 Dogecoin

200,000–299,999: 125,000 Dogecoin

300,000–399,999: 62,500 Dogecoin

400,000–499,999: 31,250 Dogecoin

500,000–599,999: 15,625 Dogecoin

600,000+: 10,000 Dogecoin

**原先的区块奖励机制（一分钟区块和四小时难度调整）:**

1–99,999: 0–1,000,000 Dogecoin

100,000–199,999: 0–500,000 Dogecoin

200,000–299,999: 0–250,000 Dogecoin

300,000–399,999: 0–125,000 Dogecoin

400,000–499,999: 0–62,500 Dogecoin

500,000–599,999: 0–31,250 Dogecoin

600,000+: 10,000 Dogecoin

### 请编译我吧 / Wow plz make dogecoind/dogecoin-cli/dogecoin-qt

  以下是开发者的笔记，教你如何在你自己的平台上搭建狗狗币。这些不是什么权威指南，但包含了必要的libary, 编译flag 等等。

  - [OSX Build Notes](doc/build-osx.md)
  - [Unix Build Notes](doc/build-unix.md)
  - [Windows Build Notes](doc/build-windows.md)

### 端口们

- RPC 22555
- P2P 22556

## 开发贴士与技巧

**调试编译**

运行 `configure`， 应用 `--enable-debug` 选项, 然后 `make`. 或者运行 `configure` 应用
`CXXFLAGS="-g -ggdb -O0"` 或者其他你需要的flag。

**debug.log**

如果代码表现得很诡异，看一眼data路径下的debug.log；错误信息和调试消息都在这里。

`-debug=...`这个选项代表调试模式；带着这个flag去运行会打开所有的类别（并输出一个非常大的debug.log文件）。

Qt代码把 `qDebug()` 放到qt类别下的debug.log文件里；使用方式`-debug=qt`。

**测试链（testnet）和私有网络（regtest）模式**

如果你在通过因特网测试跨机代码，在测试链上运行`-testnet`来运行“开始使用狗狗币”。

如果你在测试单机代码，用`-regtest`运行。
在回归测试模式下，区块可以随时被按需制造出来；参见qa/rpc-tests/。

**DEBUG_LOCKORDER**

狗狗币核心是一个多线程应用，碰到死锁和其他多线程问题都会很难找到原因。如果在代码中发现了不一致的地方，使用-DDEBUG_LOCKORDER编译 (configure CXXFLAGS="-DDEBUG_LOCKORDER -g")会在运行时插入检查语句来检测哪些进程被锁定，并把警告加入debug.log文件中。
