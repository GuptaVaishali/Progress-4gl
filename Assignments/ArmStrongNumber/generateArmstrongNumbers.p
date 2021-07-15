/* 
    PURPOSE - GENERATION OF ARMSTRONG NUMBERS
    AUTHOR NAME - VAISHALI GUPTA
    CREATION DATE - 16-JUN-2021
*/

/********** DEFINING VARIABLES *******************/
DEFINE VARIABLE viLimit     AS INTEGER     NO-UNDO.
DEFINE VARIABLE viNumber    AS INTEGER     NO-UNDO.
DEFINE VARIABLE viRemainder AS INTEGER     NO-UNDO.
DEFINE VARIABLE viQuotient  AS INTEGER     NO-UNDO.
DEFINE VARIABLE viTemp      AS INTEGER     NO-UNDO.
DEFINE VARIABLE viSum       AS INTEGER     NO-UNDO.
DEFINE VARIABLE viOrder     AS INTEGER     NO-UNDO.
DEFINE VARIABLE viCount     AS INTEGER     NO-UNDO.

/******* CALCULATES NO OF DIGITS IN NUMBER **************/
FUNCTION findOrder RETURNS INTEGER(INPUT inum AS INTEGER).
    viCount = 0.
    DO WHILE inum <> 0:
       inum = TRUNCATE (inum / 10 , 0).
       viCount = viCount + 1.
    END.
    RETURN viCount.
END FUNCTION.


/***** ASK USER A NUMBER *******/
SET viLimit.

DO viNumber = 1 TO viLimit:
    viOrder = findOrder(INPUT viNumber).

    /***** ASSIGN NUMBER TO A TEMP VARIABLE ********/
    viTemp = viNumber.
    viSum = 0.
    
    /***** CALCULATE SUM OF INDIVIDUAL DIGITS OF NUMBER **********/
    DO WHILE viTemp <> 0:
        viRemainder = viTemp MOD 10.
        viSum       = viSum + EXP(viRemainder , viOrder).
        
        /**** TRUNCATE DECIMAL VALUES, AS DIVISION OPERATOR RETURNS DECIMAL VALUES ****/
        viTemp      = TRUNCATE (viTemp / 10, 0).     
    END. 
     
    /***** CHECK IF SUMMATION IS EQUAL TO NUMBER ENTERED BY USER *************/
    IF viSum = viNumber THEN
    DO:
        MESSAGE viNumber "IS ARMSTRONG NUMBER" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    END.
END.   

