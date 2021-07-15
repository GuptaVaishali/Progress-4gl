DEF VAR ch AS CHARACTER INITIAL "A String".
DEF VAR Inf AS INTEGER INITIAL 200.

DEFINE BUTTON btn-save LABEL "save".
DEFINE BUTTON btn-reset LABEL "reset".
DEFINE BUTTON btn-exit LABEL "Exit".

DEFINE FRAME f1 
    SKIP(2) ch COLON 10 VALIDATE(ch BEGINS "a" , "Entry must begin with letter A") SKIP
    inf COLON 10 VALIDATE (inf >100 ,"inf value should be greater than 100") SKIP(1)
    btn-save btn-reset btn-exit WITH NO-BOX CENTERED  SIDE-LABELS.
    
ON CHOOSE OF btn-save
DO:
    IF ch:MODIFIED THEN
    DO:
         ASSIGN ch.
    END.
    
    IF inf:MODIFIED THEN
    DO:
         ASSIGN inf.
    END.
END.

ON CHOOSE OF btn-reset
DO:
    DISPLAY ch inf WITH FRAME f1 USE-TEXT.
END.

DISPLAY ch inf WITH FRAME f1.
ENABLE ALL WITH FRAME f1.
WAIT-FOR CHOOSE OF btn-exit.

