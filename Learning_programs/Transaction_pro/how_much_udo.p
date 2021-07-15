//ON F9 STOP.
DEFINE VARIABLE extension LIKE order-line.price.
DEFINE VARIABLE tot-order LIKE order-line.price.
REPEAT:                                     //starts transaction
    INSERT order WITH 2 COLUMNS.
    tot-order = 0.
    i-block:
    REPEAT:                               //starts subtransaction
        CREATE order-line.
        order-line.order-num = order.order-num.
        DISPLAY order-line.order-num.
        UPDATE line-num order-line.item-num qty price.
        extension = qty * price.
        tot-order = tot-order + extension.
        IF tot-order > 500 THEN DO:
            MESSAGE "Order has exceeded $500".
            MESSAGE "No more order-lines allowed".
            UNDO i-block.
        END.
    END.
END.
