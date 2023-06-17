<h1 align="center">
Dogecoin Core [DOGE, Ð]  
<br/><br/>
<img src="https://static.tumblr.com/ppdj5y9/Ae9mxmxtp/300coin.png" alt="Dogecoin" width="300"/>
</h1>

<div align="center">

[![DogecoinBadge](https://img.shields.io/badge/Doge-Coin-yellow.svg)](https://dogecoin.com)
[![MuchWow](https://img.shields.io/badge/Much-Wow-yellow.svg)](https://dogecoin.com)

</div>

Séléction de la langue: [EN](./README.md) | [CN](./README_zh_CN.md) | [PT](./README_pt_BR.md) | [FA](./README_fa_IR.md)  | [VI](./README_vi_VN.md) | FR

Dogecoin est une crypto-monnaie centrée sur une communauté inspirée par un meme sur le Shiba Inu. Le programme Dogecoin Core permet à quiconque d'opérer un nœud sur le réseau blockchain de Dogecoin et utilise l'algorithme de hash Scrypt pour prouver le travail effectué (Proof of Work). Dogecoin Core est une adaptation sur Bitcoin Core et d'autres crypto-monnaies.

Pour plus d'information sur les frais de transaction sur le réseau Dogecoin, veuillez consulter l'addresse:
[frais conseillés](doc/fee-recommendation.md).

**Site web:** [dogecoin.com](https://dogecoin.com)

## Mode d'utilisation 💻

Pour commencer votre journée avec Dogecoin Core, veuillez lire le [manuel d'installation](INSTALL.md) et le [guide pour débutants](doc/getting-started.md).

L'API JSON-RPC offert par Dogecoin Core est auto-documenté et peut-être lu avec la commande `dogecoin-cli help`. Pour plus d'information sur une commande, `dogecoin-cli help <command>`. Sinon, voir la [documentation sur Bitcoin Core](https://developer.bitcoin.org/reference/rpc/) - qui implémente un protocole similaire - pour obtenir une version navigable.

### Trop ports

Dogecoin Core utilise le port `22556` par défaut pour la communication pair-à-pair synchnorisant la blockchain sur le réseau principal. Sinon, un port JSON-RPC peut être ouvert, qui par défaut est le port "22555" sur le réseau principal. Nous vous recommandons fortement de ne pas exposer les ports RPC à l'Internet public.

|  Fonction  | mainnet | testnet | regtest |
| :--------  | ------: | ------: | ------: |
| P2P        |   22556 |   44556 |   18444 |
| RPC        |   22555 |   44555 |   18332 |

## Développement en continu - Le plan lunaire 🌒

Dogecoin Core est un programme en logiciel libre géré par la communauté. Le processus de développement est ouvert et visible publiquement; tout le monde peut voir, discutter et travailler sur le programme.

Nos resources principales de développement :

* [La page projets sur Github](https://github.com/dogecoin/dogecoin/projects) est utilisée pour gérer le travail prévu ou cours de développement pour les mises à jour à venir.
* [La page discussion sur Github](https://github.com/dogecoin/dogecoin/discussions) est utilisée pour discuter des fonctionnalités, planifiées ou non, liées au développement du programme Dogecoin Core, des protocoles sous-jacents et de la crypto-monnaie DOGE.
* [La page Dogecoindev sur Reddit](https://www.reddit.com/r/dogecoindev/)

### Stratégie de versionnement
Les numéros de version suivent le format sémantique ```majeur.mineur.patch```.

### Branches de développement
Le projet a 3 branches principales:

- **master:** Stable, contient la dernière version de la dernière mise à jour *majeur.mineur*.
- **maintenance:** Stable, contient la dernière version des mises à jour précédentes, qui sont toujours en maintenance. Format : ```<version>-maint```
- **development** Instable, contient du code pour les mises à jour prévues. Format : ```<version>-dev```

*Les branches Master et Maintenance sont modifiables exclusivement lors d'une nouvelle version. Les mises à jour planifiées auront toujours une branche de développement, et les contributions doivent être faites sur cette branche. Les branches de maintenance existent uniquement pour la correction des bogues, veuillez donc transmettre les nouvelles fonctionnalités dans la branche de développement avec la version la plus élevée.*

## Contribuer 🤝

Si vous trouvez un bogue ou avez une expérience inhabituelle avec ce programme, veuillez nous le signaler en utilisant le [traqueur de bogue](https://github.com/dogecoin/dogecoin/issues/new?assignees=&labels=bug&template=bug_report.md&title=%5Bbug%5D+).

Ayez la gentilesse de lire [](https://github.com/dogecoin/dogecoin/blob/master/CONTRIBUTING.md) pour apprendre comment contribuer au développement de Dogecoin Core. Il y a souvent des [questions ouvertes](https://github.com/dogecoin/dogecoin/labels/help%20wanted) où vos contributions auront de l'importance et seront appréciées. Ouaou. Ouananiche.

## Communautés 🚀🍾

Vous pouvez rejoindre la communauté sur différents réseaux sociaux. Pour voir ce qui ce passe, rencontrer des gens et discuter, obtenir le meme ultime, apprendre sur Dogecoin, donner et recevoir de l'aide et partager votre projet.

Voici quelques endroits à visiter:

* [Dogecoin subreddit](https://www.reddit.com/r/dogecoin/)
* [Dogeducation subreddit](https://www.reddit.com/r/dogeducation/)
* [Discord](https://discord.gg/dogecoin)
* [Dogecoin Twitter](https://twitter.com/dogecoin)

## Questions fréquentes ❓

Avez-vous une question concernant Dogecoin? Peut-être qu'une réponse est déjà disponible dans la [FAQ](https://github.com/dogecoin/dogecoin/blob/master/doc/FAQ.md) ou dans la [section Q&A](https://github.com/dogecoin/dogecoin/discussions/categories/q-a) de notre forum de discussion !

## Licence ⚖️
Dogecoin Core est disponible sous les termes de la licence MIT. Veuillez-voir [COPYING](COPYING) pour plus d'information ou [opensource.org](https://opensource.org/licenses/MIT)
