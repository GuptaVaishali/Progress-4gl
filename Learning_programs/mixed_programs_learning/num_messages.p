DEFINE VARIABLE i AS INTEGER NO-UNDO.
FORM WITH FRAME bbb.
REPEAT WITH FRAME bbb:
    PROMPT-FOR customer.cust-num WITH FRAME aaa.
    FIND customer USING cust-num NO-LOCK NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE ERROR-STATUS:NUM-MESSAGES
        " errors occurred during conversion." SKIP
        "Do you want to view them?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
        UPDATE view-errs AS LOGICAL.
        IF view-errs THEN
            DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
            MESSAGE ERROR-STATUS:GET-NUMBER(i)
            ERROR-STATUS:GET-MESSAGE(i).
        END.
    END.
    ELSE
        DISPLAY customer WITH 2 COLUMNS.
END.
