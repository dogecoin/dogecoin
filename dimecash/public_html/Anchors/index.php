<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<?php

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

include "welcome.php";

?>
</div>
</html>
