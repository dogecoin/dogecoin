<h1 align="center">
<img src="https://raw.githubusercontent.com/dogecoin/dogecoin/master/share/pixmaps/dogecoin256.svg" alt="Dogecoin" width="256"/>
<br/><br/>
Dogecoin Core [DOGE, Ð]  
</h1>

**重要: 2024年8月以降、`master` ブランチが主要な統合ブランチとなり、不安定な状態に
なりました。本番環境で使うバイナリをコンパイルする場合は、タグの付いたバージョンを
チェックアウトしてください。**

多言語のドキュメントについては、[doc/intl](doc/intl/README.md) の索引を参照してください。

Dogecoin は、柴犬のミームから着想を得た、コミュニティ主導の暗号資産です。Dogecoin Core ソフトウェアを使えば、誰でも Dogecoin ブロックチェーンネットワークのノードを運用できます。プルーフ・オブ・ワークのハッシュ方式には Scrypt を採用しており、Bitcoin Core をはじめとする暗号資産の実装を基に開発されています。

Dogecoin ネットワークで使われる既定の手数料については、[手数料の推奨値](doc/fee-recommendation.md)を参照してください。

## 使い方 💻

Dogecoin Core を使い始めるには、[インストールガイド](INSTALL.md)と[入門ガイド](doc/getting-started.md)を参照してください。

Dogecoin Core が提供する JSON-RPC API は自己文書化されており、`dogecoin-cli help` で一覧を閲覧できます。各コマンドの詳細な情報は `dogecoin-cli help <command>` で確認できます。

### すごいポート

Dogecoin Core は、「mainnet」ブロックチェーンの同期や、新しいトランザクションと
ブロックの情報を受け取るために必要な P2P 通信に、既定でポート `22556` を使います。
これに加えて JSONRPC ポートを開くこともでき、mainnet のノードでは既定で
ポート `22555` を使います。RPC ポートをインターネットに公開しないことを強く推奨します。

| 機能     | mainnet | testnet | regtest |
| :------- | ------: | ------: | ------: |
| P2P      |   22556 |   44556 |   18444 |
| RPC      |   22555 |   44555 |   18332 |

## 継続的な開発 - ムーンプラン 🌒

Dogecoin Core はオープンソースかつコミュニティ主導のソフトウェアです。開発プロセスは
公開されており、誰でもソフトウェアを見て、議論し、開発に参加できます。

主な開発リソース:

* [GitHub Projects](https://github.com/dogecoin/dogecoin/projects) は、今後の
  リリースに向けて計画中および進行中の作業を追うために使われています。
* [GitHub Discussions](https://github.com/dogecoin/dogecoin/discussions) は、
  Dogecoin Core ソフトウェアの開発、その基盤となるプロトコル、および DOGE という
  資産に関わる機能について、計画済みかどうかを問わず議論するために使われています。

### バージョンの方針
バージョン番号は ```major.minor.patch``` のセマンティクスに従います。

### ブランチ
このリポジトリには4種類のブランチがあります:

- **master:** 不安定。開発中の最新のコードが含まれます。
- **maintenance:** 安定。過去のリリースのうち、現在もメンテナンスが継続されている
  ものの最新版が含まれます。形式: ```<version>-maint```
- **development:** 不安定。今後のリリースに向けた新しいコードが含まれます。形式: ```<version>-dev```
- **archive:** 安定。メンテナンスが終了し、もう変更されることのない過去の
  バージョンのための、不変のブランチです。

***プルリクエストは `master` に対して送ってください***

*メンテナンスブランチはリリース時にのみ変更されます。リリースが計画されると*
*development ブランチが作成され、master のコミットがメンテナによってそこへ*
*チェリーピックされます。*

## コントリビュート 🤝

バグを見つけた場合や、このソフトウェアで問題が起きた場合は、
[Issue システム](https://github.com/dogecoin/dogecoin/issues/new?assignees=&labels=bug&template=bug_report.md&title=%5Bbug%5D+)を使って報告してください。

Dogecoin Core の開発に参加する方法については、
[コントリビューションガイド](CONTRIBUTING.md)を参照してください。
[協力を求めているトピック](https://github.com/dogecoin/dogecoin/labels/help%20wanted)も
数多くあり、そこへのコントリビュートは大きな効果があり、とても感謝されます。wow.

## とてもよくある質問 ❓

Dogecoin について質問がありますか？ その答えは、すでに [FAQ](doc/FAQ.md) や、
ディスカッションボードの
[Q&A セクション](https://github.com/dogecoin/dogecoin/discussions/categories/q-a)
にあるかもしれません。

## ライセンス - たくさんライセンス ⚖️
Dogecoin Core は MIT ライセンスの条項の下で公開されています。詳しくは
[COPYING](COPYING) を参照してください。
