DEFINE VARIABLE viNum1   AS INTEGER    NO-UNDO.
DEFINE VARIABLE viNum2   AS INTEGER    NO-UNDO.
DEFINE VARIABLE calc     AS Calculator NO-UNDO.
DEFINE VARIABLE vlAns    AS LOGICAL    NO-UNDO INITIAL TRUE.
DEFINE VARIABLE viChoice AS INTEGER    NO-UNDO.
calc = NEW Calculator().

DO WHILE vlAns:
    SET viNum1 LABEL "Please Enter 1st Number" WITH SIDE-LABEL.
    SET viNum2 LABEL "Please Enter 2nd Number" WITH SIDE-LABEL.

    DISPLAY SKIP "1. Addition" SKIP
                 "2. Subtraction" SKIP
                 "3. Multiplication" SKIP
                 "4. Division" SKIP.
                 
    SET viChoice LABEL "Please Choose a Option" WITH SIDE-LABEL.
    CASE viChoice:
        WHEN 1 THEN
        DO:
           calc:addition(INPUT viNum1,INPUT viNum2).     
        END.
        WHEN 2 THEN
        DO:
           calc:subtraction(INPUT viNum1,INPUT viNum2). 
        END.
        WHEN 3 THEN
        DO:
           calc:multiplication(INPUT viNum1,INPUT viNum2). 
        END.
        WHEN 4 THEN
        DO:
           calc:division(INPUT viNum1,INPUT viNum2). 
        END.
    END CASE.
    MESSAGE "DO You Want To Continue(Yes/No)" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO SET vlAns.
END.





