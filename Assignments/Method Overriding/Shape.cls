CLASS Shape:
    DEFINE PROTECTED VARIABLE viLENGTH  AS  DECIMAL NO-UNDO.
    DEFINE PROTECTED VARIABLE viBreadth AS  DECIMAL NO-UNDO.
    
    CONSTRUCTOR Shape(INPUT l AS DECIMAL, INPUT b AS DECIMAL):
        viLength  = l.
        viBreadth = b.    
    END CONSTRUCTOR.
    
    METHOD PUBLIC VOID findArea():
        MESSAGE "area is not defined" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    END METHOD.
END CLASS.
    
