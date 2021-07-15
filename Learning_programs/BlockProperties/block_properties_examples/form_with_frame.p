FORM 
    customer.cust-num customer.NAME
    WITH FRAME a 15 DOWN USE-TEXT.   
  
FOR EACH customer WITH FRAME a:
    DISPLAY cust-num NAME.
    PAUSE.
END.

DISP "display" WITH FRAME a.
