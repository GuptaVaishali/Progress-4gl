//DEFINE BUFFER cust-buf FOR customer.
//DEFINE BUFFER order-buf FOR order.

//FOR EACH customer:
  //  FOR EACH order OF customer:
    //    FIND FIRST order NO-LOCK NO-ERROR.
    //    DISPLAY order.cust-num.
  //  END.
//END.
//FIND FIRST order-buf OF cust-buf.
//DISPLAY order-buf.cust-num.

//FOR EACH order OF customer:
 //   DISPLAY order.
//END.

DO FOR order:
    DO FOR customer:
        FIND FIRST order.
    END.
END.
