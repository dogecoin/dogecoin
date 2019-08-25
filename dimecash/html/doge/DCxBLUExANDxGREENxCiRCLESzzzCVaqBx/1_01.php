<table border=1>
<?php 
$array = file("data.txt");
$targ = array();
foreach ( $array as $line)
{
	list($block,$date,$satoshi_code) = explode(" ",$line);
	list($minor,$major) = explode(".",$satoshi_code);
	if($minor < 10)$minor = $minor * 10;
	$targ[$minor] = $major;
} ?>
<table>
<?php 
	ksort($targ); 
foreach ($targ as $set)
{  echo "<tr>";
for ($x = 0 ; $x < 8; $x++)
{
	if($set[$x] == 1) echo "<td> ";
	if($set[$x] == 2) echo "<td style='color:blue'>2";
	if($set[$x] == 3) echo "<td style='color:green'>3";
}
   echo "</tr>";
}

?>
</table>
