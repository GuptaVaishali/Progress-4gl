/* How much to undo transaction */

REPEAT:                             //starts Transaction block
    INSERT order WITH 2 COLUMNS.
    FIND customer OF order.
    REPEAT:                            //starts subtransaction
        CREATE order-line.
        order-line.order-num = order.order-num.
        DISPLAY order-line.order-num.
        UPDATE line-num order-line.item-num qty price.
    END.
END.


//to check
/*PROMPT-FOR order.order-num.
FIND order USING order.order-num.
DISP order.  */

/*PROMPT-FOR order-line.order-num line-num.
FIND order-line USING order-num AND line-num.
DISP order-line.  */



/*Suppose that you wanted to restrict the maximum amount of an order to $500. In the event that
the value of an order exceeded that amount, you want to undo the current order–line entry and
display a message to the user. */

DEFINE VARIABLE extension LIKE order-line.price.
DEFINE VARIABLE tot-order LIKE order-line.price.
REPEAT:                                     //starts transaction
    INSERT order WITH 2 COLUMNS.
    tot-order = 0.
    o-l-block:
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
            UNDO o-l-block.
        END.
    END.
END.



//Making Transaction Larger

/* if error occurs, Only the current order is undone, while all other orders are safely
stored in the database */

REPEAT:
    INSERT order WITH 2 COLUMNS.
    REPEAT:
        CREATE order-line.
        order-line.order-num = order.order-num.
        DISPLAY order-line.order-num.
        UPDATE line-num order-line.item-num qty price.
    END.
END.

//when you want to undo all the orders
DO TRANSACTION:                 //Transaction starts here
    REPEAT:
        INSERT order WITH 2 COLUMNS.
        REPEAT:
            CREATE order-line.
            order-line.order-num = order.order-num.
            DISPLAY order-line.order-num.
            UPDATE line-num order-line.item-num qty price.
        END.
    END.
END.                 //Transaction ends here


//check all the orders you entered save in the database or not.
PROMPT-FOR order.order-num.
FIND order USING order-num.
DISPLAY order WITH 2 COLUMNS.
FOR EACH order-line OF order:
    DISPLAY order-line.
END.


//when you want to undo only the current order–line

REPEAT:
    DO TRANSACTION:
        INSERT order WITH 2 COLUMNS.
    END.
    REPEAT TRANSACTION:
        CREATE order-line.
        order-line.order-num = order.order-num.
        DISPLAY order-line.order-num.
        UPDATE line-num order-line.item-num qty price.
        FIND item OF order-line.
    END.
END.

//To check that only the last order-line enetered is undone,rest are saved in database.
PROMPT-FOR order.order-num.
FIND order USING order-num.
DISPLAY order WITH 2 COLUMNS.
FOR EACH order-line OF order:
    DISPLAY order-line.
END.

//Transaction and variables
DEFINE VARIABLE i AS INTEGER.
DEFINE VARIABLE j AS INTEGER.
culoop:
REPEAT:
    i = i + 1.                              //varaible out of transaction..so undo does not occur for this
    PROMPT-FOR customer.cust-num.
    FIND customer USING cust-num.
    DISPLAY name.
    FOR EACH order OF customer:              //Transaction starts here
        j = j + 1.                               //undoes the value of variable-returns to previous value
        DISPLAY order-num.
        UPDATE odate.
        IF odate > TODAY THEN UNDO cusloop, RETRY cusloop.
    END.                                       //Transaction ends here
END.
