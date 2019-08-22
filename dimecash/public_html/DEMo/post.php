<?php

# Change this to point to the directory where 
# dimecash is installed
#
#$funct="Fun.Times() { :; }";
$repo_dir = "/home/dime/dimecash";
$unspendable = "/home/dime/unspendable/unspendable.py";
#set_include_path(".:$repo_dir/php_library");
#include "lib.php";

echo `date`;
if(isset($_POST['funct']))file_put_contents("myfile.txt",$_POST[funct]);

$fun=`./decf.bash`;

?>
<form action="post.php" method="post">
   <p><textarea rows="20" cols="60" name="funct">
<? echo $fun; ?> 
</textarea> 
   <input type="submit" name="submit" value="Submit" />
</form>
