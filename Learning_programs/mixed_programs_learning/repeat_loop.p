/*DEFINE VARIABLE i AS INTEGER INITIAL "1".
REPEAT i=1 TO 10:  //repeat block uses unnamed frame by default.
    DISPLAY i.
END. */

/*DEFINE VARIABLE i AS INTEGER INITIAL "1".
REPEAT i=1 TO 10 WITH FRAME f:  //repeat block uses unnamed frame by default.
    DISPLAY i.
END.  */

/*DEFINE VARIABLE i AS INTEGER INITIAL "1" LABEL "value".
REPEAT i=1 TO 10:  //repeat block uses unnamed frame by default.
    DISPLAY i.
END. */

DEFINE VARIABLE i AS DECIMAL INITIAL "1" NO-UNDO.
TryingRepeat:                            //block name
REPEAT while(i<=10):  //repeat block uses unnamed frame by default.
    DISPLAY i.
    i = i + 1.
END.



