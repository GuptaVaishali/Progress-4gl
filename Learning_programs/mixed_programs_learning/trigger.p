DEFINE VAR months AS INTEGER EXTENT 12
LABEL "january","feb","march","apr","may","june","july","aug","sept","oct","nov","dec" 
INITIAL [31,28,31,30,31,30,31,31,30,31,30,31].

DEFINE BUTTON btn-exit LABEL "exit".

DEFINE FRAME f1
    months COLON 11 SKIP(1)
    btn-exit
    WITH SIDE-LABELS NO-BOX CENTERED.
    
ON ENTRY OF months
DO:
    MESSAGE SELF:LABEL "has" SELF:SCREEN-VALUE "days."
        "the cursor is in array element number" self:index.
END.

DISPLAY months WITH FRAME f1.
ENABLE ALL WITH FRAME f1.
WAIT-FOR CHOOSE OF btn-exit. 
