//ON F9 ENDKEY.
DEFINE VAR a AS CHARACTER NO-UNDO.
DEF VAR b AS CHAR NO-UNDO.
o-block:
REPEAT:
    UPDATE a.
    UPDATE b.
 INSERT order WITH 2 COLUMNS.
 FIND customer OF order.
 DISP customer.cust-num.
END.  

/*PROMPT-FOR order.order-num.
FIND order USING order-num.
DISP order.   */

/*ON F9 ENDKEY.
o-block:
REPEAT:
INSERT order WITH 2 COLUMNS.
FIND customer OF order.
0-1-block:
REPEAT ON ENDKEY UNDO o-block,LEAVE o-block:
CREATE order-line.
order-line.order-num = order.order-num.
DISPLAY order-line.order-num.
UPDATE line-num order-line.item-num qty.
FIND item OF order-line.
order-line.price = item.price.
UPDATE order-line.price.
END.
END. */

/*REPEAT WITH 1 COLUMN 1 DOWN:
PROMPT-FOR customer.cust-num.
FIND customer USING cust-num.
UPDATE name address city state postal-code credit-limit.
END. */
