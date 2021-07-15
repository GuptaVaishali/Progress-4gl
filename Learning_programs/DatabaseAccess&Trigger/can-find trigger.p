ON FIND OF customer DO: MESSAGE "Find Session TRIGGER" VIEW-AS ALERT-BOX. END.

DEFINE VARIABLE rId AS ROWID NO-UNDO.

FIND FIRST customer. /* first fire of trigger */
rId = ROWID(customer).
RELEASE customer.

//IF CAN-FIND(FIRST customer WHERE ROWID(customer) = rId ) THEN DO: END. /* second fire of trigger */
IF CAN-FIND(FIRST customer WHERE cust-num = 1 ) THEN DO: END. /* No second fire of trigger  */
