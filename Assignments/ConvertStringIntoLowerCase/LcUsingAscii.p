/*****************Defining Variables****************/
DEFINE VARIABLE upperStr AS CHARACTER FORMAT "x(16)" NO-UNDO.
DEFINE VARIABLE lowerStr AS CHARACTER FORMAT "x(16)" NO-UNDO.
DEFINE VARIABLE i        AS INTEGER                  NO-UNDO.
DEFINE VARIABLE ch       AS CHARACTER                NO-UNDO.

/*********Input String from user**********/
SET upperStr.

/*******Finds Ascii values of characters, compares values then convert into lowercase****/

DO i = 1 TO LENGTH(upperStr):
    ch = SUBSTRING(upperStr,i,1).
    IF Asc(ch) GE 65 AND ASC(ch) LE 90 THEN
         lowerStr = lowerStr +  CHR(ASC(ch)+ 32).
    ELSE
        lowerStr = lowerStr + ch.     
END.
DISP lowerStr.  
