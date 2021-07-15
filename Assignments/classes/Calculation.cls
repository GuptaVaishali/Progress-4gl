CLASS Calculation:
    DEFINE PROTECTED VARIABLE viTotal AS INTEGER NO-UNDO.
    
    CONSTRUCTOR Calculation():
        viTotal = 0.
        MESSAGE "inside base class constructor" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    END CONSTRUCTOR.
    
    CONSTRUCTOR Calculation(INPUT pcName AS CHARACTER):
        MESSAGE "inside base parameter constructor"
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.   
    END CONSTRUCTOR.
    
    METHOD PUBLIC VOID addition(INPUT piNum1 AS INTEGER, INPUT piNum2 AS INTEGER):
        viTotal =  piNum1 + piNum2.
        MESSAGE viTotal VIEW-AS ALERT-BOX INFORMATION BUTTONS OK TITLE "Addition".
    END METHOD. 
    
    METHOD PUBLIC VOID show():
        MESSAGE "inside base class show method" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    END METHOD.
    
END CLASS.
