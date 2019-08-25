<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html lang="en" xml:lang="en">
<head>
 <meta http-equiv="content-type" content="text/html;charset=UTF-8">
 <meta http-equiv="refresh" content="15">

<pre>
<?php


$blocks = array();
$feed=(explode("\n",`grep " DC" 2*.txt | egrep 'zzz|00039692' | cut -c 13- | grep -v BRANi`)); 



foreach(array_reverse($feed) as $line)
{
	list($block,$date,$addr,$code) = explode(" ",$line);
	$blocks[$block][$date][$addr] = $code;
}


foreach($blocks as $height => $values) 
{
	foreach($values as $timestamp => $values)
	{
		foreach($values as $addr => $code)
	#	{  if(substr($addr,0,3) == "DCU")
		        echo "$height $addr $code\n";
	#	}
	}

}



?>
</pre>

