<h1 align="center">
<img src="https://raw.githubusercontent.com/dogecoin/dogecoin/master/share/pixmaps/dogecoin256.svg" alt="Dogecoin" width="256"/>
<br/><br/>
Dogecoin Core [DOGE, Ð]
</h1>

**중요: 2024년 8월부터 `master` 브랜치가 기본 통합 브랜치가 되었고 불안정 브랜치가 되었습니다.
프로덕션 바이너리를 컴파일하기 전에 꼭 태그된 버전을 확인하세요.**

국제화된 문서는 [doc/intl](doc/intl/README.md) 에서 확인할 수 있습니다.

Dogecoin 은 시바견 밈에서 영감을 받아 만들어진 커뮤니티 기반 암호화폐입니다.
Dogecoin Core 소프트웨어는 누구나 Dogecoin 블록체인 네트워크에서 노드를 운영할 수 있도록 하며,
작업 증명(Proof of Work) 방식으로 Scrypt 해싱 알고리즘을 사용합니다.
이 소프트웨어는 Bitcoin Core 와 다른 암호화폐에서 파생되었습니다.

네트워크 기본 수수료에 대한 정보는 [권장 수수료](doc/fee-recommendation.md) 를 확인하세요.

## 사용법 💻

Dogecoin Core 와 함께 여정을 시작하려면 [설치 가이드](INSTALL.md) 및 [시작하기](doc/getting-started.md) 튜토리얼을 참조하세요.

Dogecoin Core 에서 제공하는 JSON-RPC API는 자체 문서화 기능을 갖추고 있으며, `dogecoin-cli help` 명령어를 통해 탐색할 수 있습니다.
특정 명령어에 대한 자세한 정보는 `dogecoin-cli help <command>`를 실행하여 확인할 수 있습니다.

### 포트

Dogecoin Core 는 기본적으로 P2P(peer-to-peer) 통신을 위해 `22556` 포트를 사용하며,
이는 "메인넷" 블록체인과 동기화하고 새로운 트랜잭션과 블록 정보를 업데이트합니다.
추가적으로 JSONRPC 포트를 열 수 있으며, 메인넷 노드의 기본 포트는 `22555` 입니다.
RPC 포트를 공용 인터넷에 노출하지 않는 것을 강력히 권장합니다.

| Function | mainnet | testnet | regtest |
| :------- | ------: | ------: | ------: |
| P2P      |   22556 |   44556 |   18444 |
| RPC      |   22555 |   44555 |   18332 |

## 지속적 개발 - Moon plan 🌒

Dogecoin Core 는 오픈 소스 및 커뮤니티 기반의 소프트웨어입니다.
개발 프로세스는 오픈되어 공개적으로 볼수있고, 누구나 보고, 논의하고, 작업할 수 있습니다.

메인 개발 리소스:

* [GitHub Projects](https://github.com/dogecoin/dogecoin/projects) 는
  향후 릴리즈를 위한 계획, 진행 중인 작업을 진행합니다.
* [GitHub Discussions](https://github.com/dogecoin/dogecoin/discussions) 는
  Dogecoin Core, 기본 프로토콜 및 DOGE 자산 개발에 관한 계획된 기능과 계획되지 않은 기능에 대해 논의합니다.

### 버전 전략
버전 번호는 ```major.minor.patch``` 체계를 따릅니다.

### 브랜치
4가지 종류의 브랜치가 있습니다:

- **master:** 불안정, 개발 중인 최신 코드 포함.
- **maintenance:** 안정적, 지속적으로 유지관리되고 있는 이전 릴리스의 최신 버전 포함. 포멧: ```<version>-maint```
- **development:** 불안정, 향후 릴리스를 위한 새 코드 포함. 포멧: ```<version>-dev```
- **archive:** 안정적, 더 이상 유지 관리되지 않는 변경되지 않는 구버전의 수정불가한 브랜치입니다.

***모든 pull request는 `master` 브랜치로 제출하세요.***

*Maintenance 브랜치는 릴리스별로 독립적인 변경이 가능합니다.*
*릴리스가 계획되면, 개발 브랜치가 생성되고*
*마스터 브랜치의 커밋이 maintainers 에 의해 cherry-pick 됩니다.*

## 기여 🤝

버그를 발견하거나 소프트웨어와 관련한 문제가 있다면
[이슈 시스템](https://github.com/dogecoin/dogecoin/issues/new?assignees=&labels=bug&template=bug_report.md&title=%5Bbug%5D+)
을 사용해 보고해 주세요.

[기여 가이드](CONTRIBUTING.md)를 참고하여 Dogecoin Core 개발에 참여하는 방법을 확인하세요.
[도움을 필요로 하는 주제들](https://github.com/dogecoin/dogecoin/labels/help%20wanted)이 종종 있으며,
여러분의 기여는 커뮤니티에 큰 영향을 주고, 큰 감사를 받을 것입니다. 와우.

## 자주 묻는 질문 ❓

Dogecoin 과 관련된 질문이 있나요?
답변은 아마 [FAQ](doc/FAQ.md)
또는 [Q&A section](https://github.com/dogecoin/dogecoin/discussions/categories/q-a)
에서 찾을 수 있을 것입니다!

## 라이센스 ⚖️
Dogecoin Core 는 MIT license 조건에 따라 배포됩니다.
자세한 정보는 [COPYING](COPYING)을 참조하세요.