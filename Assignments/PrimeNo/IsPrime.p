/*********DEFINING VARIABLES**************/
DEFINE VARIABLE num  AS INTEGER NO-UNDO.
DEFINE VARIABLE i    AS INTEGER NO-UNDO.
DEFINE VARIABLE flag AS LOGICAL NO-UNDO.

/****ASK USER TO ENTER A NUMBER********/
SET num.

/******CHECK IF NUMBER IS PRIME OR NOT******/
IF num = 1 THEN
   flag = TRUE. 

DO i = 2 TO SQRT(num):
    IF num MOD i = 0 THEN
         flag = TRUE.
END.

IF flag THEN
    DISP "Number is not prime".
ELSE
    DISP "Number is prime".
