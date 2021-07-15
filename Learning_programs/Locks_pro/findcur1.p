FIND FIRST customer NO-LOCK NO-ERROR.
DISP customer EXCEPT comments.
PAUSE.

FIND CURRENT customer EXCLUSIVE-LOCK NO-ERROR.

IF CURRENT-CHANGED customer THEN
DO:
    MESSAGE "customer has been changed by another user"
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    DISP customer.
END.
UPDATE customer.NAME.
DISP customer.

