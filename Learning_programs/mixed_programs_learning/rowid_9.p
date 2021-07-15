DEFINE VARIABLE custrid AS ROWID.
FIND FIRST customer NO-LOCK.
custrid = ROWID(customer).

IF balance>0 THEN
DO:
    FIND customer WHERE ROWID(customer) = custrid EXCLUSIVE-LOCK.
    UPDATE customer.
END.

