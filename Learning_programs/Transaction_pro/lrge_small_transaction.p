/*REPEAT:
    INSERT order WITH 2 COLUMNS.
    REPEAT:
        CREATE order-line.
        order-line.order-num = order.order-num.
        DISPLAY order-line.order-num.
        UPDATE line-num order-line.item-num qty price.
        IF qty = 0 THEN
        DO:
            
        END.
    END.
END.    */


//Reject all orders and orderline records if condition qty = 0 becomes true

/*outer-block:
DO TRANSACTION:
    REPEAT:
        INSERT order WITH 2 COLUMNS.
        inner-block:
        REPEAT:
            CREATE order-line.
            order-line.order-num = order.order-num.
            DISPLAY order-line.order-num.
            UPDATE line-num order-line.item-num qty price.
            IF qty = 0 THEN
            DO:
               UNDO outer-block,LEAVE outer-block. 
            END.
        END.
    END.
END. */     


// Reject current orderline record only if condition qty = 0 becomes true

/*    outer-block:
    REPEAT:
        INSERT order WITH 2 COLUMNS.
        inner-block:
        REPEAT:                                    //repeat transaction
            CREATE order-line.
            order-line.order-num = order.order-num.
            DISPLAY order-line.order-num.
            UPDATE line-num order-line.item-num qty price.
            IF qty = 0 THEN
            DO:
               UNDO inner-block,LEAVE inner-block. 
            END.
        END.
    END.                    */


// Reject all orderline records of the order if condition qty = 0 becomes true

/*outer-block:
REPEAT:
    INSERT order WITH 2 COLUMNS.
    inner-block:
    DO TRANSACTION:
        REPEAT:
            CREATE order-line.
            order-line.order-num = order.order-num.
            DISPLAY order-line.order-num.
            UPDATE line-num order-line.item-num qty price.
            IF qty = 0 THEN
            DO:
               UNDO inner-block,RETRY inner-block. 
            END.
        END.
    END.
END.     */ 

//Reject the current order and all its orderline records if condition qty = 0 becomes true

    outer-block:
    REPEAT:
        INSERT order WITH 2 COLUMNS.
        inner-block:
        REPEAT:
            CREATE order-line.
            order-line.order-num = order.order-num.
            DISPLAY order-line.order-num.
            UPDATE line-num order-line.item-num qty price.
            IF qty = 0 THEN
            DO:
               UNDO outer-block,LEAVE outer-block. 
            END.
        END.
    END.       
