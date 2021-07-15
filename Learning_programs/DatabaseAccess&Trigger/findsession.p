ON FIND OF customer DO:
    MESSAGE "session find trigger"
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
END.

FIND FIRST customer NO-LOCK NO-ERROR.   //first schema find trigger then session find trigger
DISPLAY customer.


