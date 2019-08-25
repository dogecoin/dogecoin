<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html lang="en" xml:lang="en">
<head>
 <meta http-equiv="content-type" content="text/html;charset=UTF-8">
 <meta http-equiv="refresh" content="30">

<?php

include "lib.php";

#### BEGIN HERE

?>
</head>
<?php
$dirs = array();
?>
	<div class=body>


<table border=1><?php 
$array = file("data.txt");
$targ = array();
foreach ( $array as $line)
{
	list($block,$date,$satoshi_code) = explode(" ",$line);
	list($minor,$major) = explode(".",$satoshi_code);
	if($minor < 10)$minor = $minor * 10;
  echo "<tr><td>$block<td>";
	echo gmdate("m-d-Y", $date);
	$targ[$minor] = $major;
  echo "<td>$major</tr>";
} ?>
</table>

<?php include "1_01.php"; ?>
<pre>
<?php include "2_01.php";
`python ../qr-create.py $(basename $(pwd))`;
?>
</pre>

</div>
<img src="image.png">
</html>
