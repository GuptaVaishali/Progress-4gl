DEF VAR i AS INTEGER NO-UNDO.
REPEAT i=1 TO 10:
    DISP i.
    IF i=6 THEN
    DO:
          QUIT.
    END.  
END.  


DEF VAR i AS INTEGER NO-UNDO.
DEF VAR num AS INTEGER NO-UNDO.
REPEAT i=1 TO 6 ON QUIT UNDO,RETRY:
    SET num.
    DISP num.
    IF num=6 THEN
    DO:
          QUIT.
    END.  
END. 
