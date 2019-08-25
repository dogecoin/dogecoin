<?php

echo `cat diamomds-60858.1`;
echo `echo "# $(cat diamomds-60858.1 | sum)"`;

$results=explode("\n",`grep $(basename $(pwd)) ../2*.txt | cut -c 16-`);


pretty_print($results);
?>
