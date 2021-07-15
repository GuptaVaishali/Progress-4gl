/*find customer where cust-num = 1 no-lock.

run getcustomer.

disp cust-num.

Procedure getcustomer:

    find customer where cust-num = 2 no-lock.

end procedure.   */

/*FOR EACH customer NO-LOCK:
END.
DISP cust-num.  */

/*for each customer:
    update customer.
end.  */  

/*Repeat:
    Find next customer.
    Assign NAME = "vaishali".
    DISP customer.
End.   */

/*FOR FIRST customer NO-LOCK:
    DISP customer.
END. */

/*FOR FIRST customer NO-LOCK:
END. 
DISP customer. */

FOR FIRST Customer NO-LOCK BY Customer.state:
  DISPLAY Customer.
END
