DEFINE VARIABLE i AS INTEGER NO-UNDO LABEL " variablei " INITIAL "1".
DEFINE VARIABLE j AS INTEGER NO-UNDO LABEL " variablej " INITIAL "?".
DO:
    DO WHILE(i<=10) WITH FRAME f:
        DISPLAY i.  
        j = 1.
        DO WHILE(j<=i) WITH FRAME f1:
            DISPLAY j.
            ASSIGN j = j + 1.
        END.
        ASSIGN i = i + 1.  
    END.
END.      


/*REPEAT WHILE(i<=10):        // in while loop..there is a need of increment.
    DISPLAY i.
    j = 1.
    REPEAT WHILE(j<=5):
        DISPLAY j.
        ASSIGN j = j + 1.
    END.
    ASSIGN i = i + 1.
END.    */

/*REPEAT i=1 TO 10:         // internal looping here in repeat loop . no need of increment.
    DISP i.
    REPEAT j=1 TO 5:
        DISP j.
    END.
END.    */
