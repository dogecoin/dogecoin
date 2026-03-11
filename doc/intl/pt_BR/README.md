<h1 align="center">
<img src="https://raw.githubusercontent.com/scrapcoin/scrapcoin/master/share/pixmaps/scrapcoin256.svg" alt="ScrapCoin" width="256"/>
<br/><br/>
ScrapCoin Core [SCRAP, Ð]  
</h1>

ScrapCoin é uma criptomoeda gerida pela comunidade que foi inspirada no meme Shiba Inu. O programa ScrapCoin Core permite qualquer um operar um nó na rede blockchain da ScrapCoin e utilizar o algoritmo de hash Scrypt para mineração usando Prova de Trabalho (Proof of Work). O ScrapCoin Core é uma adaptação do Bitcoin Core e de outras criptomoedas.

Para maiores informações acerca das taxas de transação usadas na rede ScrapCoin, por favor acesse: 
[taxas recomendadas](doc/fee-recommendation.md).

## Uso 💻

Para começar a sua jornada com o ScrapCoin Core, veja o [manual de instalação](INSTALL.md) e o [guia para iniciantes](doc/getting-started.md).

A API JSON-RPC fornecida pelo ScrapCoin Core é auto documentada e pode ser buscada pelo comando `scrapcoin-cli help`, informações mais detalhadas sobre cada comando podem ser encontradas usando `scrapcoin-cli help <command>`.

### Quais as portas

A ScrapCoin Core utiliza por padrão a porta `22556` para comunicação par-a-par, que é necessária para sincronizar a blockchain da "rede principal" e se manter informada de novas transações e blocos. Outrosim, uma porta JSONRPC pode ser aberta, que por padrão é a porta `22555` para nós da rede principal. Recomendamos fortemente não expor portas RPC para a internet pública. 

|  Função  | mainnet | testnet | regtest |
| :------- | ------: | ------: | ------: |
| P2P      |   22556 |   44556 |   18444 |
| RPC      |   22555 |   44555 |   18332 |

## Desenvolvimento continuo - Plano Lua 🌒

A ScrapCoin Core é um programa de código aberto gerido pela comunidade. O processo de desenvolvimento é aberto e visivel publicamente; qualquer um pode ver, discutir e trabalhar no programa.

Recursos principais de Desenvolvimento:

* [Projetos do Github](https://github.com/scrapcoin/scrapcoin/projects) é utilizado para conduzir trabalhos planejados ou que estejam em desenvolvimento para as próximas atualizações.
* [Discussão do Github](https://github.com/scrapcoin/scrapcoin/discussions) é usado para discutir sobre funcionalidades, planejadas ou não, relacionadas ao desenvolvimento do programa ScrapCoin Core, os protocolos adjacentes e o criptoativo SCRAP.

### Estratégia das Versões
Os numeros de compilação seguem a seguinte semantica:  ```major.minor.patch```

### Ramificações
Este repositório possui 3 principais ramificações (branchs), são essas:

- **master:** Estável, contém a ultima versão da ultima atualização principal *major.minor*.
- **maintenance:** Estável, contém a ultima versão de atualizações anteriores, que ainda estão em manutenção. Formato: ```<version>-maint```
- **development:** Instável, contém código novo para atualizações planejadas. Formato: ```<version>-dev```

*As ramificações Master e Maintenance, são exclusivamente mutáveis por lançamento. Atualizações*
*planejadas sempre terão uma ramificação de desenvolvimento e as solicitações de inclusão deverão ser*
*encaminhadas por meio destas. Ramificações de manutenção existem apenas para **correção de bugs,***
*por favor, encaminhem novos recursos na ramificação de desenvolvimento com a versão mais alta.*

## Contribuindo 🤝

Se você achar um bug ou passar por alguma experiência incomum com este programa, por favor reporte o ocorrido usado o [sistema de problemas (issues)](https://github.com/scrapcoin/scrapcoin/issues/new?assignees=&labels=bug&template=bug_report.md&title=%5Bbug%5D+).

Por favor, acesse o [guia de contribuições](CONTRIBUTING.md) para ver como você pode participar
do desenvolvimento do ScrapCoin Core. Existem alguns [pedidos de ajuda](https://github.com/scrapcoin/scrapcoin/labels/help%20wanted)
onde os contribuintes terão grande importância e apreço. wow.

## Perguntas frequentes ❓

Você possui alguma questão relacionada à ScrapCoin? Talvez uma resposta já esteja disponivel no
[FAQ](doc/FAQ.md) ou na
[seção Q&A](https://github.com/scrapcoin/scrapcoin/discussions/categories/q-a)
do nosso quadro de discussão!

## Licenças ⚖️
ScrapCoin Core é disponibilizada sob os termos de uso da licença MIT. Veja,
[COPYING](COPYING) para mais informações.
