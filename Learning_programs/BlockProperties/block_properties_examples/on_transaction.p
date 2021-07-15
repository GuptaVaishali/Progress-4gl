REPEAT:
    FIND FIRST customer WHERE cust-num=1.
    UPDATE cust-num NAME.
    LEAVE.
END.  

REPEAT TRANSACTION:
    FIND FIRST customer WHERE cust-num=1.
    DISP cust-num NAME.
    LEAVE.
END. 

//Not a transaction block
DO:
    FIND customer WHERE cust-num=2.
    DISP customer.
    DISP TRANSACTION WITH FRAME aaa.    
END.   

DO TRANSACTION:
    FIND customer WHERE cust-num=2.
    DISP customer.
END.  


  
