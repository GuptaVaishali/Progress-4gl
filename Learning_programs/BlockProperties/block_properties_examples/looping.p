//This is just a block..not a loop
DEFINE VARIABLE i AS INTEGER LABEL "value" NO-UNDO.
i=1.
DO:                    
    DISPLAY i.
    i = i + 1.  
END.      
       
       
/*DEFINE VARIABLE i AS INTEGER LABEL "value" NO-UNDO.
i=1.
DO i=1 TO 5 BY 2 WITH FRAME f:                    
    DISPLAY i. 
END.     
     

DEFINE VARIABLE i AS INTEGER NO-UNDO.
i=1.                          
DO WHILE(i<=10) WITH FRAME f:
    DISPLAY i MODULO 2.
    i = i + 1. 
END.     


DEFINE VARIABLE i AS INTEGER NO-UNDO.
REPEAT i=1 TO 5:
            FIND NEXT customer.          
             DISPLAY cust-num NAME.
END.     


FOR EACH customer:                 //implicit looping
    DISPLAY customer WITH 2 COLUMNS.
END.       */
