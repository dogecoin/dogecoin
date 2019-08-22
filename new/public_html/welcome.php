<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">

<h1>Welcome to Dimecash! </h1>
<div>

<?
echo `date`;

if(isset($_POST['funct']))
      {
      file_put_contents("$_SERVER[UNIQUE_ID]",$_POST[funct]);
      $fun=`./decf.bash $_SERVER[UNIQUE_ID]`;
      }
      else
      $fun=`./decf.bash Hello-27909.1`;
?>

<form action="index.php" method="post">
   <p><textarea rows="20" cols="86" name="funct">
<? echo $fun; ?>
</textarea>
   <input type="submit" name="submit" value="Submit" />
</form>

<div>
If you naturally read the contents of the box above, then you have experiences one of the basic hacks of Dimecash.  When you first visit this page, the message should end with the line: # Hello-27909.1.  If you change even one small part of the message the number will change.  The number is a very small hash of the message.  Feel free to change the text and you will see that the message changes.  If you change the message to say "Goodbye () { : again; }", then you will see some reformating and a number will appear at the end.  The number should change to 60272.  Change whatever you like and the number should always uniquely represent the text.  This process can also be done on any computer or smart phone that can run a bash shell. 
<p>
You should be very careful with what you type into the HTML form above.  This site makes public all entries immediately in <a href="secrets"> this <a> directory.
<p>
Congratulations, you now understand a fundamental part of this system.  The text above can independently resolved into the number by using a small hash.  The number can then be sent on as what is called a Satoshi Code by simply including it in a transaction.  The hash file, or secret, can be shared 
in various ways so that only a stub is left for the public to see.
<p>
The second piece of this system uses a similar trick.

<form action="https://dime.cash/cgi-bin/dogechain.cgi?name=name" target="doge">
  Enter Desired Public Anchor/Unspendable Address(opens in a new tab):<br>
  <input type="text" name="name" size=40><br>
  <input type="submit">
</form>


</div>


</html>
