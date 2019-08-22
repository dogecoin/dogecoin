<?php

# This may not make sense to stay here

function pretty_print($array) {

echo "<pre>";
print_r($array);
echo "</pre>";

}

$cwd = getcwd();
$this_dir = substr($cwd,strlen($_SERVER[DOCUMENT_ROOT]));
$dh = opendir('.');

$breadcrumbs = explode("/", $this_dir);

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

echo "<div class=breadcrumb>";

$menupath = "https://$_SERVER[SERVER_NAME]";

foreach($breadcrumbs as $crumb)
{
	if ($crumb == "")
	$crumb = "home";
	else
	$menupath = $menupath . "/" . $crumb;

	echo "<a href=$menupath/index.php> $crumb</a> |";
}

echo "<table>";
echo "$thisdir";
echo "</table>";

echo "</div>";

echo "<div class=menu>";
foreach ($dirs as $dir) 
{

			echo "<br><a href=$this_dir/$dir> $dir</a>\n";
}

echo "</div>";

?>
