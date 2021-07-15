DEFINE VARIABLE vcust-num LIKE customer.cust-num INITIAL 0.
DEFINE VARIABLE vname LIKE customer.NAME.

DO WHILE NOT vcust-num = ?:
    SET vcust-num LABEL "customer number" WITH SIDE-LABELS.
    IF NOT vcust-num = ? THEN
    DO:
       RUN return_part.p (INPUT vcust-num , OUTPUT vNAME).
       MESSAGE "Customer" vcust-num "is" vname + ".".
    END.
END.
