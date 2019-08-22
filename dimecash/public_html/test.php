<?php

require_once 'vendor/.composer/autoload.php';
// searches for the class allowing autoloaders to be called
var_dump(class_exists('Doctrine\ORM\EntityManager', true)); // dumps bool(true)
$generator = new \Picqer\Barcode\BarcodeGeneratorPNG();
echo '<img src="data:image/png;base64,' . base64_encode($generator->getBarcode('081231723897', $generator::TYPE_CODE_128)) . '">';
?>
