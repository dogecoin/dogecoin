<h1 align="center">
<img src="https://raw.githubusercontent.com/dogecoin/dogecoin/master/share/pixmaps/dogecoin256.svg" alt="Dogecoin" width="256"/>
<br/><br/>
Dogecoin Core [DOGE, Ð]  
</h1>

**GIRÎNG: Ji Tebaxê 2024'an ve, liqê `master` bûye liqê bingehîn a entegrasyonê û
ne seqamgîr e. Ji kerema xwe berî ku hûn binariyên hilberînê amade bikin, versiyonên nîşankîrî kontrol bikin.**

Ji bo belgeyên navneteweyî, li fîhrîsta [doc/intl](doc/intl/README.md) binêrin.

Dogecoin pereyekî elektronîk e ku ji hêla civakê ve tê birêvebirin û ji mîmê Shiba Inu îlham girtiye. Nermalava Dogecoin Core dihêle ku her kes di torên zincîra blokê ya Dogecoin de gireyek saz bike û ji bo Îsbata Kar rêbaza hashing Scrypt bi kar tîne. Ev ji Bitcoin Core û kriptopereyên din hatiye adapte kirin.

Ji bo agahdariyên li ser xercên standart ên ku di tora Dogecoin de tên bikaranîn, ji kerema xwe li
[pêşniyarên xercê](doc/fee-recommendation.md) binêrin.

## Bikaranîn 💻

Ji bo destpêkirina rêwîtiya xwe bi Dogecoin Core, li [rêbera sazkirinê](INSTALL.md) û perwerdehiya [destpêkirinê](doc/getting-started.md) binêrin.

API ya JSON-RPC ku ji hêla Dogecoin Core ve tê pêşkêş kirin, xwe-belgedar e û dikare bi `dogecoin-cli help` were gerandin, dema ku agahdariyên berfireh ji bo her fermanê dikare bi `dogecoin-cli help <ferman>` were dîtin.

### Ev portên

Dogecoin Core bi awayê xweber porta `22556` ji bo peywendiya peer-to-peer bi kar tîne, ku
ji bo senkronîzekirina zincîra blokê ya "mainnet" pêwîst e û ji bo ku hûn ji
muameleyên nû û blokan agahdar bimînin. Bi zêdeyî, porteke JSONRPC dikare were vekirin, ku
ji bo gireyên mainnet porta wê ya xwerû `22555` ye. Bi xurtî tê pêşniyar kirin ku hûn
portên RPC'ê ji înternetê re nekin.

| Fonksiyon | tora sereke | tora testê | tora testê ya rêgezê |
| :------- | ------: | ------: | ------: |
| P2P      |   22556 |   44556 |   18444 |
| RPC      |   22555 |   44555 |   18332 |

## Pêşveçûna berdewam - Plana Heyvê 🌒

Dogecoin Core nermalavek çavkaniya vekirî û ji hêla civakê ve tê birêvebirin. Pêvajoya
pêşveçûnê vekirî ye û bi eşkereyî tê dîtin; her kes dikare li ser nermalavê bibîne, nîqaş bike û bixebite.

Çavkaniyên bingehîn ên pêşveçûnê:

* [Projeyên GitHub](https://github.com/dogecoin/dogecoin/projects) ji bo
  şopandina kar ên plankîrî û di pêşveçûnê de ji bo weşanên pêş de tê bikaranîn.
* [Nîqaşên GitHub](https://github.com/dogecoin/dogecoin/discussions) ji bo
  nîqaşkirina taybetmendiyan, yên plankîrî û neplankîrî, yên ku bi pêşveçûna
  nermalava Dogecoin Core, protokolên bingehîn û heyberê DOGE ve girêdayî ne, tê bikaranîn.

### Stratejiya Versiyonê
Hejmarên versiyonê semantîka ```girîng.biçûk.patch``` dişopînin.

### Liq
Di vê depoyê de 4 cureyên liqan hene:

- **master:** Ne seqamgîr, koda herî dawî ya di bin pêşveçûnê de dihewîne.
- **maintenance:** Seqamgîr, versiyona herî dawî ya weşanên berê dihewîne,
  ku hîn jî di bin lênihêrîna çalak de ne. Format: ```<versiyon>-maint```
- **development:** Ne seqamgîr, koda nû ji bo weşanên pêş de dihewîne. Format: ```<versiyon>-dev```
- **archive:** Seqamgîr, liqên neguhêrbar ji bo versiyonên kevn ku êdî naguherin
  ji ber ku êdî nayên lênihêrîn.

***Daxwazên xwe yên pull li dijî `master` pêşkêş bikin***

*Liqên lênihêrînê bi tenê bi weşanê guherbar in. Dema ku weşanek*
*tê plankirin, liqekî pêşveçûnê dê were afirandin û pêkanîn ji master*
*dê ji hêla lênihêrkerên ve werin hilbijartin.*

## Beşdarbûn 🤝

Heke hûn çewtiyek dibînin an bi vê nermalavê re pirsgirêkan tecrûbe dikin, ji kerema xwe
bi karanîna [pergala pirsgirêkê](https://github.com/dogecoin/dogecoin/issues/new?assignees=&labels=bug&template=bug_report.md&title=%5Bbug%5D+) rapor bikin.

Ji kerema xwe binêrin [rêbera beşdarbûnê](CONTRIBUTING.md) da ku hûn bibînin hûn çawa dikarin
beşdarî pêşveçûna Dogecoin Core bibin. Gelek caran [mijarên ku alîkariyê dixwazin](https://github.com/dogecoin/dogecoin/labels/help%20wanted) hene
ku beşdariyên we dê bandoreke bilind hebin û gelek pejirandin werbigirin. wow.

## Pirsên Pir Caran Tên Pirsîn ❓

Ma pirseke we derbarê Dogecoin heye? Bersivek belkî jixwe di
[FAQ](doc/FAQ.md) an
[beşa Pirs û Bersiv](https://github.com/dogecoin/dogecoin/discussions/categories/q-a)
a panela nîqaşê de ye!

## Lîsans - Gelek lîsans ⚖️
Dogecoin Core li gorî şertên lîsansa MIT tê weşandin. Ji bo
bêtir agahdarî binêrin [COPYING](COPYING).