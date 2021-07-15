FORM WITH FRAME a.
DEF VAR totalprice AS DECIMAL NO-UNDO.
DEF VAR totaloprice AS DECIMAL NO-UNDO.
DEF VAR ordercount AS INT NO-UNDO.
FOR EACH customer:
    DISP cust-num WITH FRAME b.
    FOR EACH order OF customer BREAK BY order.cust-num:
         IF FIRST-OF(order.cust-num) THEN
         DO:
            ordercount = 0.
            totaloprice = 0.
         END.
         ordercount = ordercount + 1.            
         FOR EACH order-line OF order BREAK BY order-line.order-num:
            IF FIRST-OF(order-line.order-num) THEN
            DO:                            
                totalprice = 0.
            END.
            totalprice  = totalprice + extended-price.
            
         END. 
         totaloprice = totaloprice + totalprice.
         IF LAST-OF(order.cust-num) THEN
         DO:
            DISP ordercount WITH FRAME a. 
         END.
    END.   
    DISP totaloprice WITH FRAME a.
END.

