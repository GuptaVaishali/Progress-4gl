DISABLE TRIGGERS FOR LOAD OF customer ALLOW-REPLICATION.
ON CREATE OF customer DO:
    MESSAGE "create session trigger"
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
END.

ON ASSIGN OF customer.cust-num OLD VALUE oldcustnum DO:
    MESSAGE "Assign Session Trigger for cust-num"
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    IF customer.cust-num <> oldcustnum THEN
    DO:
         MESSAGE "custnum changed in session assign trigger"
             VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    END.
END.

ON WRITE OF customer OLD BUFFER oldcust DO:
    MESSAGE "write session trigger"
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
  /*  IF customer.NAME <> oldcust.NAME THEN
    DO:
         MESSAGE "customer name changed in session trigger"
             VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    END.     */
END.

ON FIND OF customer REVERT.
/*DO:
    MESSAGE "Find session trigger"
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
END.    */

ON DELETE OF customer DO:
    MESSAGE "Delete Session Trigger"
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
END.

DO TRANSACTION:
    INSERT customer.
END. 

DO TRANSACTION:
    FIND customer WHERE customer.cust-num = 70 NO-ERROR.
    DELETE customer.
END.


