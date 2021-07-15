ON WRITE OF customer OLD BUFFER ocust DO:
    MESSAGE "write session trigger"
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    IF customer.sales-rep <> ocust.sales-rep THEN 
    DO:
        FIND salesrep OF customer NO-LOCK.
        customer.comments = customer.comments + " Salesrep changed to " +
        salesrep.rep-name + " on " + STRING(TODAY).
    END.
END.

FIND FIRST customer NO-ERROR.
UPDATE customer.     

//to check if customer comment changed or not.
/*FIND customer WHERE customer.cust-num = 15.
DISP customer. */

/*FIND customer WHERE customer.cust-num = 2000.  //find trigger does not execute 
                                                //beacause it does not satisfy the condition
DISP customer.  */


