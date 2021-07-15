DEFINE BUFFER cust_buf FOR customer.
DEFINE BUFFER order_buf FOR order.

DEFINE VARIABLE custnum LIKE customer.cust-num.
DEFINE VARIABLE itemnum LIKE ITEM.item-num.

REPEAT:
    UPDATE custnum.
    RUN getcust.
END.

PROCEDURE getcust:
    FIND FIRST cust_buf WHERE cust_buf.cust-num = custnum NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cust_buf THEN
    DO:
        MESSAGE "Customer not found".
        RETURN ERROR.
    END.
    ELSE DO:
        FIND FIRST order_buf OF cust_buf NO-LOCK NO-ERROR.
        IF AVAILABLE order_buf THEN
        DO:
              RUN getord.
        END.
        ELSE MESSAGE "No order of customer".
    END.
 END PROCEDURE.
 
PROCEDURE getord:
    FOR EACH order-line OF order_buf:
        DISPLAY order-line WITH FRAME foo.
        itemnum = order-line.item-num.
        RUN getitem.
    END.
END PROCEDURE.

PROCEDURE getitem:
    FIND FIRST ITEM WHERE ITEM.item-num = itemnum.
    DISPLAY cust_buf.cust-num cust_buf.NAME ITEM.item-num ITEM.item-name WITH FRAME foo1.
END PROCEDURE.
