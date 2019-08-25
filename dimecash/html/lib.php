<?php

function pretty_print($array) {

echo "<pre>\n";
print_r($array);
echo "</pre>\n";

}

function menu() {

$cwd = getcwd();
$self = $_SERVER['PHP_SELF']; 
$pos = strrpos($self,'/');
$path = substr($self,0,$pos);

$bc = explode('/',$path);

array_shift($bc);


return $bc;

}

$breadcrumbs = menu();
$compound = "";

echo "<table border=1><tr>";
foreach ( $breadcrumbs as $item )
{
	$compound = $compound . "/" . $item;
	echo "<td><a href=$compound>$item</a>";

}

$this_dir = $compound;

echo "</tr></table>";
$dh = opendir('.');

$exclude = array(".","..","images","cgi-bin");
while (($tmp = readdir($dh)) !== false) 
{
  if(in_array($tmp,$exclude))
           continue;
  if(is_dir($tmp))
	$dirs []= $tmp;
  else
	$files []= $tmp;

}
closedir($dh);

function subdirs($dirs) {
echo "<div class=menu>";
foreach ($dirs as $dir) 
{

			echo "<br><a href=$this_dir/$dir> $dir</a>\n";
}

echo "</div>";

}

?>
