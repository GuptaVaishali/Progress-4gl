DEF VAR i AS INT NO-UNDO.
DISP TRANSACTION.                         //not a transaction block 
DO i=1 TO 10 ON ERROR UNDO,RETRY:
    DISP TRANSACTION.                    //not a transaction block as no direct updating statement
    DISP i.
    FIND customer WHERE cust-num = 999.  //error, customer not found, it will do undo and next rather than undo,retry,beacuse progress detects infinite loop would occur.
END.
