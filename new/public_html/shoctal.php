<html>
<title>Shoctal - Shifted Octal Notation</title>
<body>
<h1>Shoctal - Shifted Octal Notation</h1>

Although a cryptocurrency transaction is almost universally displayed as a decimal number, issues arise because expressing a value of zero is not allowed.  Also, a message could change length based on its size.  These problems are solved by switching to an octal representation.  The normal octal numeral set begins with zero, however.  By shifting all of the numerals up, zero can be removed.  A second shift frees up the numeral one to be a special character in the system.
<p>
Consider these numbers:

<pre>
00002222 <--- the value zero, in a system of four digits 
00002223 <--- the value one, in a system of four digits
00000002 <--- the value zero, in a system of one digits
23212232 <--- the value 010.0010 in a system of seven digits
23221232 <--- the value 0100.010 in a system of seven digits
</pre>

<p>
Always padding to a certain message size means that a signal can be send to different places based on its composition.  A seven digit number with no value one would be distinct from a seven digit number with a value one.  
<p>
Shoctal represents an added level of complexity to DimeCash because it requires a bit of work to calculate.  Because the previous "sum" solution generates numbers up to five digits, a seven digit system would immediately be distinct.  The number range is also much greater.




</body>

</html>
