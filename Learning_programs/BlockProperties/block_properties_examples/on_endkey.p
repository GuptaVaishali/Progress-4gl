/*ON F9 END-ERROR.

o-block:
REPEAT:
    INSERT order WITH 2 COLUMNS.   
    FIND customer OF order.
    o-1-block:
    REPEAT:
        CREATE order-line.
        order-line.order-num = order.order-num.
        DISPLAY order-line.order-num.
        UPDATE line-num order-line.item-num qty.
    END.
END.  */


DEF VAR a AS INT NO-UNDO.
DEF VAR b AS INT NO-UNDO.
DEF VAR c AS INT NO-UNDO.
REPEAT:
    UPDATE a.
    UPDATE b.
    UPDATE c.
END.
