<?php

// create a 200*600 image
$img = imagecreatetruecolor(120, 80);

// allocate some colors
$white = imagecolorallocate($img, 255, 255, 255);
$red   = imagecolorallocate($img, 255,   0,   0);
$green = imagecolorallocate($img,   0, 255,   0);
$blue  = imagecolorallocate($img,   0,   0, 255);

// draw the head
imagefilledrectangle($img, 0,0,150,100, $white);
imagestring($img, 30, 20, 20, "DiMECASH", $blue);
imagefilledrectangle($img, 10,40,100,50,$green);
imagefilledrectangle($img, 10,60,100,70,$green);


// output image in the browser
header("Content-type: image/png");
imagepng($img);

// free memory
imagedestroy($img);

?>
