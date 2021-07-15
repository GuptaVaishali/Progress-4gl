FIND FIRST customer NO-LOCK NO-ERROR.                        
PAUSE.

DO TRANSACTION:
    FIND FIRST customer EXCLUSIVE-LOCK NO-ERROR.
    UPDATE NAME.
END.

MESSAGE "Transaction Ended" VIEW-AS ALERT-BOX.
PAUSE.
DISP SKIP TRANSACTION SKIP(1).    

RELEASE customer.
MESSAGE "Record Released" VIEW-AS ALERT-BOX. 
PAUSE. 





    
