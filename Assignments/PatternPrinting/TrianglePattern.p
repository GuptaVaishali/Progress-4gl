/*********DEFINING VARIABLES***********************************/
DEFINE VARIABLE n   AS INTEGER   NO-UNDO LABEL "Enter No. of lines".
DEFINE VARIABLE i   AS INTEGER   NO-UNDO.
DEFINE VARIABLE j   AS INTEGER   NO-UNDO.
DEFINE VARIABLE str AS CHARACTER NO-UNDO.

/********ASK USER THE NO OF LINES HE WANTS TO PRINT*************/
SET n.

/*********PATTERN PRINTING*******************/
DO i = 1 TO n WITH FRAME f:
    str = "".
    DO j = 1 TO n + i - 1:
        IF j <= n - i THEN
            str = str + " ".
        ELSE
            str = str + "*".
    END.
    DISP str FORMAT "x(40)".
END.
