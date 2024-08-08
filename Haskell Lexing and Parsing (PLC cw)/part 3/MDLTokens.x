{ 
module MDLTokens where 
}

-- start with writing a grammar

-- E := Forward n | Rotate D | Check [1..9] | if E then E else E | E ; E | ( E )
-- D := L | R

%wrapper "posn"
$digit = 0-9

tokens :-
	$white+    ;
	"--".*     ;
	Forward    {\p s -> TokenForward p}  
	Rotate     {\p s -> TokenRotate p}
	Check      {\p s -> TokenCheck p}
	if         {\p s -> TokenIf p}
	then       {\p s -> TokenThen p}
	else       {\p s -> TokenElse p}
	L          {\p s -> TokenLeft p}
	R          {\p s -> TokenRight p}
	\;
	\(
	\)
	[1..9]     {\p s -> TokenDigit p (read s)}
	$digit $digit+ {\p s -> TokenInt p (read s)}

{
	data MDLToken = 
}