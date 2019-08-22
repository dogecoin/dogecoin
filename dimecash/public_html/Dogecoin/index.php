<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
 <meta http-equiv="content-type" content="text/html;charset=UTF-8">

<?php

set_include_path(".:/home/dime/dimecash/php_library");
include "lib.php";
$fruit = array("apple","orange","cherry");

#### BEGIN HERE

$info = explode(" ",$_SERVER[HTTP_USER_AGENT]);

if ($info[1] == '(PlayStation')
 echo '<link rel="stylesheet" type="text/css" href="style.css">';
else
 echo '<link rel="stylesheet" type="text/css" href="style-ps.css">';

?>

</head>
<?php
$dirs = array();
include "dir.php";
?>
	<div class=body>
<?php

include "welcome.php";

?>
</div>
</html>
