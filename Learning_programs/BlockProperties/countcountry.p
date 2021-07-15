DEF VAR icount AS INT NO-UNDO.
DEFINE BUFFER custbuf FOR customer.
DEF TEMP-TABLE tCountryCount NO-UNDO FIELD country AS CHAR 
                     FIELD icount AS INT.
                    
FOR EACH customer NO-LOCK:
    icount = 0.
    FOR EACH custbuf WHERE custbuf.country = customer.country NO-LOCK:
              icount = icount + 1.
    END.
    
    FIND tCountryCount WHERE tCountryCount.country = customer.country NO-LOCK NO-ERROR.
    IF NOT AVAILABLE tCountryCount THEN
    DO:
        CREATE tCountryCount.
        tCountryCount.country  = customer.country.
        tCountryCount.icount = icount.
    END.
 //   DISP customer.country icount.
END.

FOR EACH tCountryCount:
    DISPLAY tCountryCount.
END.
