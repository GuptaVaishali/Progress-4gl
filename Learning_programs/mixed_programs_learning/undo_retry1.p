/*REPEAT WITH 1 COLUMN 1 DOWN:
 PROMPT-FOR customer.cust-num.
 FIND customer USING cust-num.
 DISPLAY name address city state postal-code credit-limit.
END. */

/*ON F9 ERROR.
REPEAT:
     INSERT order WITH 2 COLUMNS.
     FIND customer OF order.
     REPEAT:
         CREATE order-line.
         order-line.order-num = order.order-num.
         DISPLAY order-line.order-num.
         UPDATE line-num order-line.item-num qty.
         FIND item OF order-line.
         order-line.price = item.price.
         UPDATE order-line.price.
     END.
END.   */

/* DEFINE VARIABLE do-lookup AS LOGICAL.
 REPEAT WITH 1 COLUMN DOWN:
     UPDATE do-lookup LABEL "Do you want to look up a customer?"
     WITH FRAME ask-frame.
     IF do-lookup
         THEN DO:
             DO ON ERROR UNDO, RETRY:
                 PROMPT-FOR customer.cust-num.
                 FIND customer USING cust-num.
                 DISPLAY name address city state postal-code credit-limit.
         END.
     END.
     ELSE LEAVE.
 END. */
 
/*ON F9 ERROR.
o-block:
    REPEAT:
        INSERT order WITH 2 COLUMNS.
        FIND customer OF order.
        o-l-block:
        REPEAT ON ERROR UNDO o-block, RETRY o-block:
            CREATE order-line.
            order-line.order-num = order.order-num.
            DISPLAY line-num order-line.item-num qty.
            SET line-num order-line.item-num qty.
            FIND item OF order-line.
            order-line.price = item.price.
            UPDATE order-line.price.
        END.
    END.   */
 
/*PROMPT-FOR order.order-num.
FIND order USING order-num.
DISP order. */

