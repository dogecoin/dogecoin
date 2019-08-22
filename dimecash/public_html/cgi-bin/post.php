<?php

# Change this to point to the directory where 
# dimecash is installed
#
$function="";
$repo_dir = "/home/dime/dimecash";
$unspendable = "/home/dime/unspendable/unspendable.py";
set_include_path(".:$repo_dir/php_library");
include "lib.php";

if(isset($_POST['name']))$name = "value='" . "$_POST[name]" . "'";
if(isset($_POST['email']))$email = "value=" . "$_POST[email]";
#if(isset($_POST['fname']))$fname = "value=" . "$_POST[fname]";
if(isset($_POST['function']))$function = "$_POST[function]";


   if(isset($_POST['submit'])) 
   { 

      file_put_contents("xxxx",$function); 
      echo "xxxxxxxxxxxxxxxxxyyy";
      echo "<hr>";
      echo `. xxxx; declare -f x`; 
      echo "<hr>";
      $file=`echo "xx() { :; }" | ./proc.bash $repo_dir`; 
echo "xxxxx $file xxxxx";
      $funfile =`cat $repo_dir/secrets/$file` .  "# $file# $_POST[name] $_POST[email]"; 
  }

?>
<form action="post.php" method="post">
   <p>Name:  <input type="text" name="name" size=60  <?="$name" ?>  /> 
   <p>Email: <input type="text" name="email" size=60 <?="$email" ?> /> </p>
   <p><textarea rows="20" cols="60" name="function"><?=$funfile ?></textarea> 
   <input type="submit" name="submit" value="Submit" />
</form>
<div>
<? echo `echo "$funfile" | sum`; ?>

<? echo `$unspendable D $name 30`; ?>
</div>
