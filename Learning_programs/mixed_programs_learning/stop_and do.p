DEFINE VAR i AS INTEGER NO-UNDO.
/*DO i=1 TO 5:
    DISP i.
END.*/

/*REPEAT i=1 TO 5:
    DISP i.
END.   */



//ON F9 STOP.

/*REPEAT:
    SET i.
    DISP i.
END.  */

/*REPEAT i=1 TO 10:
    DISP i.
    IF i=6 THEN
    DO:
          STOP.
    END.  
END.  */


/*REPEAT:
    SET i.
    DISP i.
    IF i=5 THEN
    DO:
          STOP.
    END.
END.  */   

FOR EACH Customer ON ERROR UNDO, LEAVE
ON STOP UNDO, RETRY:
IF RETRY
THEN DO:
MESSAGE "The STOP condition has occurred." SKIP
"Do you wish to continue?" VIEW-AS ALERT-BOX QUESTION
BUTTONS yes-no UPDATE continue-ok AS LOGICAL.
IF NOT continue-ok
THEN undo,LEAVE.
END.
UPDATE Customer.
END.   

//ON F8 QUIT.

FOR EACH Customer ON QUIT undo,RETRY:
IF RETRY
THEN DO:
MESSAGE "The Quit condition has occurred." SKIP
"Do you wish to continue?" VIEW-AS ALERT-BOX QUESTION
BUTTONS yes-no UPDATE continue-ok AS LOGICAL.
IF NOT continue-ok
THEN UNDO, LEAVE.
END.
UPDATE Customer.
END. 

/*FOR EACH customer:
    DISP customer.
END.*/
