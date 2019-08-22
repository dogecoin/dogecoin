<? 
$dh = opendir("/home/dime/secrets");
echo "<div><pre>";
while (($tmp = readdir($dh)) !== false)
{
  if(strncmp("DCx",$tmp,3) == 0)echo "$tmp\n";

}
closedir($dh);

echo "</pre></div>";

?>
