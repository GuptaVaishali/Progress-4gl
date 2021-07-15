CLASS Triangle INHERITS Shape:
    DEFINE PRIVATE VARIABLE viHeight AS INTEGER NO-UNDO.
    DEFINE PRIVATE VARIABLE viBase AS INTEGER NO-UNDO.
    
    CONSTRUCTOR PUBLIC Triangle(INPUT piHeight AS INTEGER, INPUT piBase AS INTEGER):
        viHeight = piHeight.
        viBase = piBase.
    END CONSTRUCTOR.
    
    METHOD PUBLIC OVERRIDE VOID draw():
        MESSAGE "Triangle Shape" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    END METHOD.
    
    METHOD PUBLIC OVERRIDE DOUBLE area():
        RETURN (viHeight * viBase) * 0.5.
    END METHOD.
    
END CLASS.
