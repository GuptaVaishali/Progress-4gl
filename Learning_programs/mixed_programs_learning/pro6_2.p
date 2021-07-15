DISPLAY "You may update each customer" SKIP
"After making your changes, press one of:" SKIP(1)
KBLABEL("Go") + " - Make the change permanent " FORMAT "x(40)"
SKIP
KBLABEL("end-error") + " - undo changes and exit" FORMAT "x(40)"
SKIP
"f8 - undo changes and try again" SKIP
"f10 - Find next customer" SKIP
"f12 - find previous customer"
WITH CENTERED FRAME ins.

FIND FIRST Customer.
REPEAT:
    UPDATE cust-num NAME address address2 city st
    GO-ON(f8 f10 f12) WITH 1 DOWN CENTERED.
    
    CASE LASTKEY:
        WHEN KEYCODE("f8") THEN
            UNDO, RETRY.
        WHEN KEYCODE("f10") THEN
            FIND NEXT customer.
        WHEN KEYCODE("f12") THEN
            FIND PREV customer.
    END CASE.
END.
