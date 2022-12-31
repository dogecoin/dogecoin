<h1 align="center">
<img src="https://static.tumblr.com/ppdj5y9/Ae9mxmxtp/300coin.png" alt="Dogecoin" width="300"/>
<br/><br/>
Dogecoin Core [DOGE, Ð]  
</h1>

<div align="center">

[![DogecoinBadge](https://img.shields.io/badge/Doge-Coin-yellow.svg)](https://dogecoin.com)
[![MuchWow](https://img.shields.io/badge/Much-Wow-yellow.svg)](https://dogecoin.com)

</div>

Sélectionner la langue: [EN](README.md) | [CN](./README_zh_CN.md) | [PT](./README_pt_BR.md) | [VN](./README_vi_VN.md) | [FA](./README_fa_IR.md) | FR

Dogecoin est une crypto-monnaie communautaire qui s'inspire d'un mème de Shiba Inu. Le logiciel Dogecoin Core permet à quiconque d'exploiter un nœud dans les réseaux de la blockchain Dogecoin et utilise la méthode de hachage Scrypt pour la preuve de travail. Il est adapté de Bitcoin Core et d'autres crypto-monnaies.

Pour plus d'informations sur les frais par défaut utilisés sur le réseau Dogecoin, veuillez consulter la [recommandation sur les frais](doc/fee-recommendation.md).

**Site web:** [dogecoin.com](https://dogecoin.com)

## Utilisation 💻

Pour commencer votre voyage avec Dogecoin Core, consultez le [guide d'installation](INSTALL.md) et le [tutoriel de démarrage](doc/getting-started.md).

L'API JSON-RPC fournie par Dogecoin Core est auto-documentée et peut être parcourue avec `dogecoin-cli help`, tandis que des informations détaillées pour chaque commande peuvent être visualisées avec `dogecoin-cli help <command>`. Vous pouvez également consulter la [documentation Bitcoin Core](https://developer.bitcoin.org/reference/rpc/) - qui implémente un protocole similaire - pour obtenir une version consultable.

### Les ports

Dogecoin Core utilise par défaut le port `22556` pour la communication pair-à-pair qui est nécessaire pour synchroniser la blockchain au "mainnet" et rester informé des nouvelles transactions et blocs. De plus, un port JSONRPC peut être ouvert, dont par défaut le port `22555` est utilisé pour les nœuds du réseau principal. Il est fortement recommandé de ne pas exposer les ports RPC à l'Internet public.

| Function | mainnet | testnet | regtest |
| :------- | ------: | ------: | ------: |
| P2P      |   22556 |   44556 |   18444 |
| RPC      |   22555 |   44555 |   18332 |

## Développement continu - Moon plan 🌒

Dogecoin Core est un logiciel libre et communautaire. Le processus de développement est ouvert et publiquement visible; tout le monde peut voir, discuter et travailler sur le logiciel.

Principales ressources de développement :

* [GitHub Projects](https://github.com/dogecoin/dogecoin/projects) est utilisé pour
  suivre les travaux prévus et en cours pour les prochaines versions.

* [GitHub Discussion](https://github.com/dogecoin/dogecoin/discussions) est utilisé pour
  discuter des caractéristiques, planifiées et non planifiées, liées à la fois au développement du logiciel Dogecoin Core, les protocoles sous-jacents et l'actif DOGE.
  
* [Dogecoindev subreddit](https://www.reddit.com/r/dogecoindev/)

### Stratégie de version
Les numéros de version suivent la sémantique ```major.minor.patch```.

### Branches
Il y a 3 types de branches dans ce référentiel :

- **master:** Stable, contient la dernière version de la dernière *major.minor* version.
- **maintenance:** Stable, contient la dernière version des versions précédentes, qui sont toujours en maintenance active. Format : ```<version>-maint```
- **development:** Instable, contient du nouveau code pour les versions prévues. Format : ```<version>-dev```

*Les branches master et de maintenance sont exclusivement mutables par version. Les versions planifiées auront toujours une branche de développement et les demandes de pull doivent être soumises dans cette branche. Les branches de maintenance ne sont là **seulement pour corriger les bugs**, veuillez soumettre les nouvelles fonctionnalités dans la branche de développement avec la version la plus récente.*

## Contribution🤝

Si vous trouvez un bug ou rencontrez des problèmes avec ce logiciel, veuillez le signaler en utilisant le [système de gestion des problèmes](https://github.com/dogecoin/dogecoin/issues/new?assignees=&labels=bug&template=bug_report.md&title=%5Bbug%5D+).

Veuillez consulter [le guide de contribution](CONTRIBUTING.md) pour voir comment vous pouvez
participer au développement de Dogecoin Core. Il y a souvent
[des sujets demandant de l'aide](https://github.com/dogecoin/dogecoin/labels/help%20wanted)
où vos contributions auront un impact considérable et seront très appréciées. wow.

## Communautés 🚀🍾

Vous pouvez rejoindre les communautés sur différents réseaux sociaux.
Pour voir ce qu'il se passe, rencontrer des gens et discuter, trouver le dernier mème, apprendre sur le Dogecoin, donner ou demander de l'aide, pour partager votre projet.

Voici quelques endroits à visiter :

* [Dogecoin subreddit](https://www.reddit.com/r/dogecoin/)
* [Dogeducation subreddit](https://www.reddit.com/r/dogeducation/)
* [Discord](https://discord.gg/dogecoin)
* [Dogecoin Twitter](https://twitter.com/dogecoin)

## Questions très fréquemment posées ❓

Vous avez une question concernant le Dogecoin ? Une réponse se trouve peut-être déjà dans la
[FAQ](doc/FAQ.md) ou dans la
[section Q&R](https://github.com/dogecoin/dogecoin/discussions/categories/q-a)
du forum de discussion !

## License - Much license ⚖️
Dogecoin Core est publié sous les termes de la licence MIT. Voir
[COPYING](COPYING) pour plus d'informations ou voir
[opensource.org](https://opensource.org/licenses/MIT)
