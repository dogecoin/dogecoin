<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
 <meta http-equiv="content-type" content="text/html;charset=UTF-8">
 <meta http-equiv="refresh" content="10">
 <title> homepage for ps4 users </title>

<?php

set_include_path(".:/home/dime/dimecash/php_library");
include "lib.php";
include "cookie_update.php";
$fruit = array("apple","orange","cherry");


$info = explode(" ",$_SERVER[HTTP_USER_AGENT]);

if ($info[1] == '(PlayStation')
 echo '<link rel="stylesheet" type="text/css" href="style.css">';
else
 echo '<link rel="stylesheet" type="text/css" href="style-ps.css">';

?> 

</head>
<?php
include "dir.php";
?>
<div class=body>
<?php 
include "ps4.php";

 ?></div>

<?php echo "$info[1]" ; ?>
</html>
