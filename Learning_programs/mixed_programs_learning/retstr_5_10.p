DEFINE PARAMETER BUFFER cust-buf FOR customer.
DEFINE INPUT PARAMETER cnum AS INTEGER.
FIND cust-buf WHERE cust-num = cnum NO-ERROR.
IF NOT AVAILABLE cust-buf
THEN DO:
IF LOCKED cust-buf
THEN RETURN "Record is locked.".
ELSE RETURN "Record not found.".
END.
RETURN cust-buf.name.
