THIS-PROCEDURE:PRIVATE-DATA = "DATABASE".

PROCEDURE get-cust-name:
    DEFINE INPUT PARAMETER pcust-num LIKE customer.cust-num.
    DEFINE OUTPUT PARAMETER pname LIKE customer.name.
    FIND customer WHERE customer.cust-num = pcust-num NO-ERROR.
    IF NOT AVAILABLE(customer) THEN
        pname = ?.
    ELSE
        pname = customer.name.
END PROCEDURE.

PROCEDURE destroy-context:
    DEFINE OUTPUT PARAMETER out-message AS CHARACTER FORMAT "x(60)".
    DELETE PROCEDURE THIS-PROCEDURE.
    out-message = THIS-PROCEDURE:PRIVATE-DATA + " module deleted.".
END PROCEDURE.
