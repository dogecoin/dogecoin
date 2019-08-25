<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html lang="en" xml:lang="en">
<head>
 <meta http-equiv="content-type" content="text/html;charset=UTF-8">
 <meta http-equiv="refresh" content="15">

<pre>
<div>
This section displays DCBTCPN records, which come as a set:
DCBTCPNa
DCBTCPNb
Together they describe a coin_address address and associate 
it with the given Satoshi Code.  Since anyone with a 
Dogecoin account can create these, they must have an 
off-chain reference to be trusted.
<hr>
<?php
$search=$_GET[search];

echo $search;


$blocks = array();
$feed=(explode("\n",`grep " $search" 2*.txt | cut -c 13-`)); 


foreach($feed as $line)
{
	list($block,$date,$addr,$code) = explode(" ",$line);
	$blocks[$block][$date][$addr] = $code;
	echo "<pre>$block $date $addr $code</pre>";
}

$coin_address = "";

foreach($blocks as $height => $values) 
{
	foreach($values as $timestamp => $values)
	{ 
		foreach($values as $addr => $code)
		{
			if(substr($addr,0,8) == $search . "a")
				$coin_address = substr($addr,8,17);

			if(substr($addr,0,8) == $search . "b")
				$coin_address = $coin_address . substr($addr,8,17);

		}
	}

}
echo "<a href=https://chain.so/address/BTC/$coin_address> $coin_address </a>\n";



?>
</pre>

