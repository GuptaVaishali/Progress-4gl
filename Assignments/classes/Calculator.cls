CLASS Calculator:

    DEFINE PRIVATE VARIABLE tot AS DECIMAL NO-UNDO.
 
    CONSTRUCTOR PUBLIC Calculator():
        tot = 0.
    END CONSTRUCTOR.
 
    METHOD PUBLIC VOID addition(INPUT piNum1 AS INTEGER,INPUT piNum2 AS INTEGER):
        tot = piNum1 + piNum2.
        MESSAGE tot VIEW-AS ALERT-BOX INFORMATION BUTTONS OK TITLE "Addition".
    END METHOD.
 
    METHOD PUBLIC VOID subtraction(INPUT piNum1 AS INTEGER,INPUT piNum2 AS INTEGER):
        tot = piNum1 - piNum2.
        MESSAGE tot VIEW-AS ALERT-BOX INFORMATION BUTTONS OK TITLE "Subtraction".
    END METHOD.
 
    METHOD PUBLIC VOID multiplication(INPUT piNum1 AS INTEGER,INPUT piNum2 AS INTEGER):
        tot = piNum1 * piNum2.
        MESSAGE tot VIEW-AS ALERT-BOX INFORMATION BUTTONS OK TITLE "Multiplication".
    END METHOD.
 
    METHOD PUBLIC VOID division(INPUT piNum1 AS INTEGER,INPUT piNum2 AS INTEGER):
        tot = piNum1 / piNum2.
        MESSAGE tot VIEW-AS ALERT-BOX INFORMATION BUTTONS OK TITLE "Division".
    END METHOD.
 
END CLASS.

 

