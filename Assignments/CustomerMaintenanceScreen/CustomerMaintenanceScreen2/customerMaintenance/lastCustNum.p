 /********AUTO-GENERATED VALUE OF CUSTOMER*************************/
DEFINE OUTPUT PARAMETER piCustNum AS INTEGER NO-UNDO.
FIND LAST customer NO-LOCK NO-ERROR.
IF AVAILABLE customer THEN
    piCustNum = customer.cust-num + 1.
ELSE
    piCustNum = 1.  

