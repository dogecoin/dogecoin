Dogecoin Core [DOGE, Ð]
==========================

Dogecoin

Estado de construcción consejo para el próximo compromiso

¿Qué es Dogecoin? - Esa moneda
Dogecoin es una criptomoneda como Bitcoin, aunque no usa SHA256 como prueba de trabajo (POW). Tomando señales de desarrollo de Tenebrix y Litecoin, Dogecoin actualmente emplea una variante simplificada de scrypt.

http://dogecoin.com/

Licencia - Mucha licencia
Dogecoin Core se lanza bajo los términos de la licencia MIT. Consulte COPIA para obtener más información o consulte https://opensource.org/licenses/MIT .

Desarrollo y contribuciones - omg developers
El desarrollo está en curso y el equipo de desarrollo, así como otros voluntarios, pueden trabajar libremente en sus propios árboles y enviar solicitudes de extracción cuando las funciones o las correcciones de errores estén listas.

Estrategia de versión
Los números de versión siguen la major.minor.patchsemántica.

Ramas
Hay 3 tipos de ramas en este repositorio:

master: estable, contiene la última versión de la última versión major.minor .
mantenimiento: Estable, contiene la última versión de las versiones anteriores, que aún se encuentran en mantenimiento activo. Formato:<version>-maint
desarrollo: Inestable, contiene nuevo código para lanzamientos planificados. Formato:<version>-dev
Las ramas maestra y de mantenimiento son exclusivamente mutables por liberación. Los lanzamientos planificados siempre tendrán una rama de desarrollo y las solicitudes de extracción deben enviarse contra ellos. Las ramas de mantenimiento están disponibles solo para corregir errores, envíe nuevas funciones contra la rama de desarrollo con la versión más alta.

Contribuciones
Se recomienda encarecidamente a los desarrolladores que escriban pruebas unitarias para el nuevo código y que envíen nuevas pruebas unitarias para el código antiguo. Las pruebas unitarias se pueden compilar y ejecutar (suponiendo que no fueron desactivadas en configure) con: make check. Se pueden encontrar más detalles sobre la ejecución y extensión de pruebas unitarias en /src/test/README.md .

También hay pruebas de regresión e integración de la interfaz RPC, escritas en Python, que se ejecutan automáticamente en el servidor de compilación. Estas pruebas se pueden ejecutar (si las dependencias de prueba están instaladas) con:qa/pull-tester/rpc-tests.py

Los cambios deben ser probados por otra persona que no sea el desarrollador que escribió el código. Esto es especialmente importante para cambios importantes o de alto riesgo. Es útil agregar un plan de prueba a la descripción de la solicitud de extracción si probar los cambios no es sencillo.

Preguntas muy frecuentes
¿Cuánto dogo puede existir? - ¡Tantos cachorros!
A principios de 2015 (aproximadamente un año y medio después del lanzamiento) habrá aproximadamente 100,000,000,000 de monedas. Cada bloque subsiguiente otorgará 10,000 monedas para alentar a los mineros a continuar asegurando la red y compensar las billeteras perdidas en discos duros / teléfonos / contraseñas de cifrado perdidas / etc.

¿Cómo conseguir Doge? - ¡A la Luna!
Dogecoin utiliza una variante simplificada de la función de derivación de clave scrypt como prueba de trabajo con un tiempo objetivo de un minuto por bloque y reajuste de dificultad después de cada bloque. Las recompensas de bloque se fijan y se reducen a la mitad cada 100.000 bloques. A partir del bloque 600,000, se pagará una recompensa permanente de 10,000 Dogecoin por bloque.

Originalmente, se concibió un esquema de pago diferente con recompensas en bloque que se determinaban tomando la recompensa máxima según el programa de bloque y aplicando el resultado de un generador de números pseudoaleatorios Mersenne Twister para llegar a un número entre 0 y la recompensa máxima. Esto se cambió, comenzando con el bloque 145,000, para evitar que los grupos grandes jueguen con el sistema y extraigan solo bloques de alta recompensa. Al mismo tiempo, la dificultad de retargeting también se cambió de cuatro horas a una por bloque (cada minuto), implementando un algoritmo cortesía del equipo de desarrollo de DigiByte Coin, para disminuir el impacto de aumentos repentinos y disminuciones de la tasa de hash de la red.

El programa de recompensas de bloque actual:

1–99,999: 0–1,000,000 Dogecoin

100.000-144.999: 0-500.000 Dogecoin

145.000-199.999: 250.000 Dogecoin

200.000–299.999: 125.000 Dogecoin

300.000–399.999: 62.500 Dogecoin

400.000–499.999: 31.250 Dogecoin

500.000–599.999: 15.625 Dogecoin

600,000+: 10,000 Dogecoin

El programa de recompensas de bloque original, con objetivos de bloque de un minuto y reajuste de dificultad de cuatro horas:

1–99,999: 0–1,000,000 Dogecoin

100,000–199,999: 0–500,000 Dogecoin

200.000–299.999: 0–250.000 Dogecoin

300.000–399.999: 0–125.000 Dogecoin

400.000–499.999: 0–62.500 Dogecoin

500.000–599.999: 0–31.250 Dogecoin

600,000+: 10,000 Dogecoin

Wow por favor haga dogecoind / dogecoin-cli / dogecoin-qt
Las siguientes son notas para desarrolladores sobre cómo construir Dogecoin en su plataforma nativa. No son guías completas, pero incluyen notas sobre las bibliotecas necesarias, indicadores de compilación, etc.

Notas de compilación de OSX
Notas de compilación de Unix
Notas de compilación de Windows
Tales puertos
RPC 22555 P2P 22556



Trucos y consejos de desarrollo
compilar para depurar

Ejecute configure con la opción --enable-debug, luego haga. O ejecute configure con CXXFLAGS = "- g -ggdb -O0" o cualquier indicador de depuración que necesite.

registro de depuración

Si el código se comporta de manera extraña, eche un vistazo al archivo debug.log en el directorio de datos; Los mensajes de error y depuración se escriben allí.

La opción de línea de comandos -debug = ... controla la depuración; ejecutar con solo -debug activará todas las categorías (y le dará un archivo debug.log muy grande).

El código Qt enruta la salida de qDebug () a debug.log en la categoría "qt": ejecute con -debug = qt para verlo.

modos testnet y regtest

Ejecute con la opción -testnet para ejecutar "jugar dogecoins" en la red de prueba, si está probando código de varias máquinas que necesita operar a través de Internet.

Si está probando algo que se puede ejecutar en una máquina, hágalo con la opción -regtest. En el modo de prueba de regresión, los bloques se pueden crear bajo demanda; consulte qa / rpc-tests / para ver las pruebas que se ejecutan en modo -regtest.

DEBUG_LOCKORDER

Dogecoin Core es una aplicación multiproceso, y los puntos muertos u otros errores de multiproceso pueden ser muy difíciles de rastrear. Al compilar con -DDEBUG_LOCKORDER (configure CXXFLAGS = "- DDEBUG_LOCKORDER -g") inserta comprobaciones en tiempo de ejecución para realizar un seguimiento de los bloqueos que se mantienen, y agrega advertencias al archivo debug.log si se detectan inconsistencias.
