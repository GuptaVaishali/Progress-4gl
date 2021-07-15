FIND FIRST customer EXCLUSIVE-LOCK NO-ERROR.   //find trigger executed
UPDATE customer.cust-num.           //Assign trigger executed
//RELEASE customer.               //write trigger will execute here
MESSAGE "After customer update"
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

                                               
    
/*FIND customer WHERE customer.cust-num = 1191 NO-LOCK NO-ERROR.
DISP customer. */

/*FOR EACH order WHERE order.cust-num = 513:
       DISP order.
END.   */
   

