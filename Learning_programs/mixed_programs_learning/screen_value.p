DEFINE VARIABLE name1 AS INT INITIAL 2.
//NAME1 = "hello".
//ENABLE name1.
//DISPLAY name1.

PROMPT-FOR name1
name1 = INTEGER(name1:SCREEN-VALUE).          //screen value is by default character data type.
DISP name1.  

//SET NAME1.           // it asks for user input and after user presses enter it displays the value entered by user.
//DISPLAY name1.

//UPDATE name1.       //display previous value of var name1 i.e. 2, then user will
                       //enter some value which will go to screen buffer then in record buffer.
                       //then when user presses enter it displays the value entered by user.
