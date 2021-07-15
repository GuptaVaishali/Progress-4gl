DEFINE TEMP-TABLE TTcustomer NO-UNDO LIKE customer.

DEFINE INPUT-OUTPUT PARAMETER TABLE FOR TTcustomer.

FOR EACH customer:
    CREATE TTcustomer.
    BUFFER-COPY customer TO TTcustomer.
END.   
