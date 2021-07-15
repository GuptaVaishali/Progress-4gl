CLASS MyCalculation INHERITS Calculation:
    
    CONSTRUCTOR MyCalculation():
        MESSAGE "inside derived class constructor" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    END CONSTRUCTOR.
    
    CONSTRUCTOR MyCalculation(INPUT pcName AS CHARACTER):
        SUPER(INPUT pcName).
        MESSAGE "inside derived parameter constructor"
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    END CONSTRUCTOR.
    
    METHOD PUBLIC VOID subtraction(INPUT piNum1 AS INTEGER, INPUT piNum2 AS INTEGER):
        viTotal = piNum1 - piNum2.
        MESSAGE viTotal VIEW-AS ALERT-BOX INFORMATION BUTTONS OK TITLE "Subtraction". 
    END METHOD.
    
    METHOD PUBLIC OVERRIDE VOID show():
        SUPER:show().
        MESSAGE "inside derived class show method" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    END METHOD.
    
END CLASS.
