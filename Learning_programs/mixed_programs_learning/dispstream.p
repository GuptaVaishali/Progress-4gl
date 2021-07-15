DEFINE SHARED BUFFER xrep FOR salesrep.
DEFINE SHARED STREAM phonelist.
FOR EACH customer OF xrep BY state:
DISPLAY STREAM phonelist cust-num name city state phone
WITH NO-LABELS STREAM-IO.
END.
