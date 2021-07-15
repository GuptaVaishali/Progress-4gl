//when error key presses,it does undo and retry.
/*ON F9 ERROR.
REPEAT WITH 1 COLUMN 1 DOWN:         
    PROMPT-FOR customer.cust-num.
    FIND customer USING cust-num.
    DISP NAME address city state postal-code.
END. */ 

//on error phrase changes the default behaviour
ON F9 ERROR.
REPEAT WITH 1 COLUMN 1 DOWN ON ERROR UNDO,LEAVE:   
    PROMPT-FOR customer.cust-num.                         
    FIND customer USING cust-num.
    DISP NAME address city state postal-code.
END.  
     
