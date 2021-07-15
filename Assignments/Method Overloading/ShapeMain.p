DEFINE VARIABLE shapeObj   AS Shape   NO-UNDO.
DEFINE VARIABLE recArea    AS DECIMAL NO-UNDO.
DEFINE VARIABLE squareArea AS DECIMAL NO-UNDO.

shapeObj = NEW Shape().

shapeObj:calculateArea(3,2,recArea).
MESSAGE recArea VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.


shapeObj:calculateArea(4, squareArea).
MESSAGE squareArea  VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
