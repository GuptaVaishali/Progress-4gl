DEFINE BUTTON btn-exclusive LABEL "Check Exclusive".
DEFINE BUTTON btn-share LABEL "Check Share".

ENABLE  btn-exclusive  btn-share.

ON CHOOSE OF btn-exclusive
DO:
    FIND FIRST customer EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

    IF NOT AVAILABLE customer THEN
        IF LOCKED customer THEN
            MESSAGE "The Record maybe Exclusively locked"
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        ELSE
            MESSAGE "No Record" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK. 
END.

ON CHOOSE OF btn-share
DO:
    FIND FIRST customer SHARE-LOCK NO-ERROR.
    DISP customer.
END.

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.

