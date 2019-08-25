<?php

$self = $_SERVER['PHP_SELF'];
$pos = strrpos($self,'/');
echo substr($self,0,$pos);
 
include 'dir.php';


?>
