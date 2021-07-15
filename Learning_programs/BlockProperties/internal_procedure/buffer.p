DEFINE BUFFER cust-buf FOR customer.
DEFINE BUFFER order-buf FOR order.

DEFINE VAR c-num LIKE customer.cust-num.
UPDATE c-num.
RUN getcust.

PROCEDURE getcust:
    FIND FIRST cust-buf WHERE cust-buf.cust-num = c-num NO-ERROR.
    IF NOT AVAILABLE cust-buf THEN
    DO:
         MESSAGE "customer not found".
         RETURN ERROR.                                       
    END.   
    ELSE DO:
        FIND FIRST order-buf OF cust-buf NO-LOCK NO-ERROR.
        IF AVAILABLE order-buf THEN
        DO:
            DISPLAY order-buf.order-num WITH FRAME f WITH 6 DOWN.
            DOWN 1 WITH FRAME f.
            FOR EACH order-line OF order-buf:
                   DISPLAY order-line WITH FRAME f WITH 6 DOWN.
                   DOWN 1 WITH FRAME f.
            END.
        END.
        ELSE
            MESSAGE "order not available".
    END. 
END.
