ON F9 STOP.                 //progress backs out the transaction on pressing stop key or system failure
REPEAT:                   //Transaction starts
    INSERT order WITH 2 COLUMNS.
    REPEAT:                         //subtransaction starts
        CREATE order-line.
        order-line.order-num = order.order-num.
        DISPLAY order-line.order-num.
        UPDATE line-num order-line.item-num qty price.
    END.                             //subtransaaction end
END.                                 //Transaction ends

FOR EACH salesrep:
    DISPLAY sales-rep rep-name.
    UPDATE region.
END.   


