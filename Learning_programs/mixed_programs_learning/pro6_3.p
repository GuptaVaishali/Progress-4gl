REPEAT:
    DISPLAY "press any key".
    READKEY.
    DISPLAY LASTKEY LABEL "key code"
    KEYLABEL(LASTKEY) LABEL "key label"
    KEYFUNCTION(LASTKEY) LABEL "key function" FORMAT "x(12)".
    
    IF KEYFUNCTION(LASTKEY) = "end-error" THEN LEAVE.
END.
