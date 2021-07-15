/*REPEAT:
 FOR EACH customer:                          //weak scope
    DISPLAY cust-num name WITH FRAME a.
 END.
 FOR EACH customer:
    DISPLAY cust-num name WITH FRAME b.       //weak scope
 END.
 
 FIND FIRST customer.       //free reference - scope raised to procedure block
 DISP customer.cust-num NAME WITH FRAME c.
 LEAVE.
END.    
DISP customer.NAME.   */



DEFINE BUFFER custbuff FOR customer.
DEFINE VAR salesrep LIKE customer.sales-rep.
DO:
 FOR EACH customer:
    DISPLAY cust-num name WITH FRAME a.
    salesrep = customer.sales-rep.
    FOR EACH custbuff:                         //Nesting weak scope not allowed
        IF customer.sales-rep = salesrep THEN 
        DO:
            DISPLAY name WITH FRAME a.
            DOWN WITH FRAME a.
        END.
     END.
  END.    
END.    
