/*******DEFINING VARIABLES**************************************/ 
DEFINE VARIABLE inputStr    AS CHARACTER NO-UNDO FORMAT "x(30)".
DEFINE VARIABLE removeStr   AS CHARACTER NO-UNDO FORMAT "x(30)".
DEFINE VARIABLE i           AS INTEGER   NO-UNDO.
DEFINE VARIABLE flag        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE resultStr   AS CHARACTER NO-UNDO.
DEFINE VARIABLE replacedStr AS CHARACTER NO-UNDO.

/******ASK USER THE INPUT STRING AND STRING TO BE REMOVED**********/ 
SET inputStr LABEL "Enter a String".
SET removeStr LABEL "Enter the String to Remove".

/*******REPLACING STRINGTOBEREMOVED TO "" **************/
DO i = 1 TO LENGTH(inputStr):
    IF SUBSTRING(inputStr,i,LENGTH(removeStr)) = removeStr THEN
    DO:
        flag = TRUE.
        replacedStr = REPLACE(inputStr,removeStr,"").
    END.
END.

/*****REVERSING THE LEFT STRING AFTER REPACING*****************************/
IF flag = FALSE THEN
     DISP "String to be removed not found" WITH FRAME f.
ELSE DO:
    DO i = LENGTH(replacedStr) TO 1 BY -1:
        resultStr = resultStr + SUBSTRING(replacedStr,i,1).
    END.
    DISP resultStr WITH FRAME f1.
END. 
