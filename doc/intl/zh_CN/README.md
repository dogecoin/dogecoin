<h1 align="center">
<img src="https://raw.githubusercontent.com/dogecoin/dogecoin/master/share/pixmaps/dogecoin256.svg" alt="Dogecoin" width="256"/>
<br/><br/>
Dogecoin Core [DOGE, Ð]  
</h1>

狗狗币是一款受柴犬表情包启发，由社区驱动的加密货币。通过狗狗币内核软件，任何人都可以在狗狗币区块链网络中建立一个节点。节点采用Scrypt哈希算法来实现工作量证明(Proof of Work)。狗狗币内核是从比特币内核和其它加密货币演化而来。

狗狗币网络默认交易费的相关信息请查看[收费建议](doc/fee-recommendation.md)

## 使用指南 💻

开始使用狗狗币内核软件，请参考[安装指南](INSTALL.md)和[入门](doc/getting-started.md)教程。

狗狗币内核提供基于自文档化的JSON-RPC API，可用`dogecoin-cli help`浏览。同时可用'dogecoin-cli help <command>`浏览每条命令的详细信息。

### 炫酷的端口

狗狗币内核的点对点通信默的默认端口为22556，用于与主网络（mainnet）区块链同步，并接受新交易和新区块的信息。此外，还可打开一个默认端口号为22555的JSONRPC端口供主网络节点使用。强烈建议不要将RPC端口暴露给公共网络。

| 功能 Function | 主网络 mainnet | 测试网络 testnet | 回归测试 regtest |
| :----- | ----------: | -------------: | ------: |
| P2P    |       22556 |          44556 |   18444 |
| RPC    |       22555 |          44555 |   18332 |

## 进行中的开发 - 月球计划 🌒

狗狗币内核是一个社区驱动的开源软件。其开发过程是开放的并公开可见的。任何人都可以查看，讨论和使用该软件。

主要开发资料：
* [Github Projects](https://github.com/dogecoin/dogecoin/projects)用于跟踪即将发布的计划和正在进行的工作。
* [Github Discussion](https://github.com/dogecoin/dogecoin/discussions)用于讨论与狗狗币内核软件开发、底层协议和狗狗币资产相关的计划内和计划外功能。

## 版本说明
版本号码遵循以下语法：```major.minor.patch```。

## 代码库分支(branch)
本代码库有3个branch：

- **master（主代码库）:** 稳定。包含最新版本的release，以*major.minor*形式呈现。
- **maintenance（维护代码库）:** 稳定。包含正在维护中的上一个release的最新版本。格式： ```<version>-maint```
- **development（正在开发代码库）:** 不稳定。包含下一个release的最新代码。格式： ```<version>-dev```

*Master 和 maintenance 互不干扰。最新release永远包含一个development分支。新的 pull request 应该发布于此。Maintenance 分支**只能用于调试程序**。请将新开发的功能发布在 development 分支的最高版本。*

## 炫酷的贡献 🤝

如果您发现错误或者遇到问题，请报告在[问题系统](https://github.com/dogecoin/dogecoin/issues/new?assignees=&labels=bug&template=bug_report.md&title=%5Bbug%5D+)

想了解如何参与狗狗币内核开发，请访问[贡献指引](CONTRIBUTING.md)。
那里常常有[主题需要帮助](https://github.com/dogecoin/dogecoin/labels/help%20wanted)。
您的贡献一定会产生很大的影响并赢得炫酷的称赞。哇哦(wow)。

## 灰常常见的问题 ❓

如果对狗狗币有问题，答案很可能已经在[常见问答](doc/FAQ.md)或者[问与答](https://github.com/dogecoin/dogecoin/discussions/categories/q-a)!里了。

## 许可证 -  灰常的许可 ⚖️
狗狗币内核是根据MIT许可条款发布的。
