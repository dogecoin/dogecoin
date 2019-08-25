<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html lang="en" xml:lang="en">
<head>
 <meta http-equiv="content-type" content="text/html;charset=UTF-8">

<?php

$dirs = array();
include "lib.php";

#### BEGIN HERE

?>
</head>
	<div class=body>
<table border=1><tr>
<?php include "1_01.php" ?>
<td>
<table><tr><td>
<iframe src = "chainview.php" width=480 height=540>
         Sorry your browser does not support inline frames.
      </iframe>
<td>
<?php 

sort($dirs);

foreach( $dirs as $dir)
{   
	if( strcmp(substr($dir,0,3),"DCx") == 0 )
        { 
	$dir2 = str_replace("x"," ",$dir);
	$dir2 = str_replace("y","-",$dir2);
	$dir2 = str_replace("i","I",$dir2);
	$dir2 = str_replace("o","O",$dir2);
	list($first,$rest) = explode("z",$dir2);
	$title=substr($first,3,25);
	echo "<br><a href=$dir>";
	if(! include "$dir/3_01.php") echo "$title</a>";
        }
}

?>
</table>
<tr><td>

<iframe src = "chainview2.php?search=DCBTCPN" width=600 height=540>
         Sorry your browser does not support inline frames.
	 </iframe>


</table>
</div>
</html>
