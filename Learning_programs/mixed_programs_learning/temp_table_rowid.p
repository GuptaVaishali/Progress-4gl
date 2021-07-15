DEFINE TEMP-TABLE ttrid FIELD ridfld AS CHARACTER.

FOR EACH customer FIELDS(balance) WHERE balance = 0 NO-LOCK:
    CREATE ttrid.
    ASSIGN ttrid.ridfld = STRING(ROWID(customer)).
END.
DO TRANSACTION:
    FOR EACH ttrid:
        FIND customer WHERE ROWID(customer) = TO-ROWID(ttrid.ridfld).
        DELETE customer.
    END.
END.
