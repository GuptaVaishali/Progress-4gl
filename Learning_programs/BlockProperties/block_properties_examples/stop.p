ON F9 STOP.
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

//stop condition
REPEAT:
    SET i.
    DISP i.
    IF i=5 THEN
    DO:
          STOP.
    END.
END.  
