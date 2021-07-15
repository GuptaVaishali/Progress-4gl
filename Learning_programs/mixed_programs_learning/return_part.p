DEFINE INPUT PARAMETER pcust-num LIKE customer.cust-num.
DEFINE OUTPUT PARAMETER pname LIKE customer.NAME INITIAL ?.

FOR EACH customer:
    IF customer.cust-num = pcust-num THEN
    DO:
         pname = customer.NAME.
         RETURN.
    END.
END.
