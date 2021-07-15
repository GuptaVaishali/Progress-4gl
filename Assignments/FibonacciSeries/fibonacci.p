/********Defining Variables*********************/
DEFINE VARIABLE a   AS INTEGER INITIAL 0 NO-UNDO.
DEFINE VARIABLE b   AS INTEGER INITIAL 1 NO-UNDO.
DEFINE VARIABLE c   AS INTEGER           NO-UNDO.
DEFINE VARIABLE i   AS INTEGER INITIAL 0 NO-UNDO.
DEFINE VARIABLE num AS INTEGER           NO-UNDO.

/********ASK USER UPTO WHICH NUMBER HE WANTS TO PRINT SERIES*********/
SET num.

/*******DISPLAYING FIRST TWO NUMBERS**********************/
DISP a NO-LABEL SKIP b NO-LABEL SKIP WITH FRAME f1.

/*******DISPLAYING FIBONACCI SERIES**********/
DO WHILE i < num - 2 WITH FRAME f:
    c = a + b.
    DISP c NO-LABEL.
    a = b.
    b = c.
    i = i + 1.
END.
