/*DEF VAR countvar AS INT NO-UNDO.
FOR EACH customer NO-LOCK BREAK BY customer.state:
    IF FIRST-OF(customer.state) THEN
    DO:
         countvar=0.
    END.
    countvar = countvar + 1.
    IF LAST-OF(customer.state) THEN
    DO:
       DISP cust-num NAME state countvar.
    END.
  //  DISP cust-num NAME state.
END.        */

FORM WITH FRAME a.
DEF VAR totalprice AS DECIMAL NO-UNDO.
DEF VAR ordercount AS INT NO-UNDO.
FOR EACH customer:
    DISP cust-num WITH FRAME a1.
    FOR EACH order OF customer BREAK BY order.cust-num:
         IF FIRST-OF(order.cust-num) THEN
         DO:
            ordercount = 0.
         END.
         ordercount = ordercount + 1.
         IF LAST-OF(order.cust-num) THEN
         DO:
            DISP order-num ordercount WITH FRAME a. 
         END.
         FOR EACH order-line OF order BREAK BY order-line.order-num:
            IF FIRST-OF(order-line.order-num) THEN
            DO:                            
                totalprice = 0.
            END.
            totalprice  = totalprice + extended-price.
            IF LAST-OF(order-line.order-num) THEN
            DO:
               DISP totalprice WITH FRAME a.
            END.         
         END.    
    END.   
END.     


/*DEF VAR countvar AS INT NO-UNDO.
FOR EACH customer:
    DISP cust-num.
    FOR EACH order WHERE order.cust-num = 1:
        FOR EACH order-line OF order:
            DISP order-line.order-num order-line.line-num order-line.item-num order-line.price order-line.qty  order-line.extended-price  .
        END.
    END.    
END.    */ 


/*FOR EACH order-line WHERE order-num = 171:
    DISPLAY order-line.
END.    */


