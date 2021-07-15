CLASS RECTANGLE INHERITS Shape:
    DEFINE PRIVATE VARIABLE viLength  AS INTEGER NO-UNDO.
    DEFINE PRIVATE VARIABLE viBreadth AS INTEGER NO-UNDO.
    
    CONSTRUCTOR PUBLIC RECTANGLE(INPUT piLength AS INTEGER , INPUT piBreadth AS INTEGER):
       viLength = piLength.
       viBreadth = piBreadth.
    END CONSTRUCTOR.
    
    METHOD PUBLIC OVERRIDE VOID draw():
        MESSAGE "Rectangle Shape" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    END METHOD.  
    
    METHOD PUBLIC OVERRIDE DOUBLE area():
         RETURN viLength * viBreadth.
    END METHOD.
    
END CLASS.
