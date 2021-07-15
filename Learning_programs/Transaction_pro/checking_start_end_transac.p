//to check which records saved in database or not

PROMPT-FOR order.order-num.
FIND order USING order-num.   //if order is not found, find statement fails, error occurs, that is ,undo,retry
DISPLAY order WITH 2 COLUMNS.
FOR EACH order-line OF order:
    DISPLAY order-line.
END.   

      
/*PROMPT-FOR order-line.order-num line-num.
FIND order-line USING order-line.order-num AND line-num.
DISP order-line. */
