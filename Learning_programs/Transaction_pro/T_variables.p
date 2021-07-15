DEFINE VARIABLE a AS INTEGER INITIAL 5.
DEFINE VARIABLE b AS INTEGER INITIAL 5 NO-UNDO.


a = a + 2.                    //7

DO TRANSACTION ON ENDKEY UNDO,LEAVE:

a = a + 3.                     //10
b = b + 3.                      //8
  
DISPLAY a b WITH FRAME aaa.

PAUSE.

END.

DISPLAY a b WITH FRAME bbb.
