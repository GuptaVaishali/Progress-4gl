CLASS Shape:
    DEFINE PRIVATE VARIABLE area AS DECIMAL NO-UNDO.
    
    METHOD PUBLIC DECIMAL calculateArea(INPUT viLength AS DECIMAL, INPUT viBreadth AS DECIMAL,OUTPUT area AS DECIMAL):
        area = viLength * viBreadth.
      //  MESSAGE area VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    END METHOD.
    
    METHOD PUBLIC DECIMAL calculateArea(INPUT viLength AS DECIMAL, OUTPUT area AS DECIMAL):
         area = viLength * viLength.
      //   MESSAGE area VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    END METHOD.
    
END CLASS.
