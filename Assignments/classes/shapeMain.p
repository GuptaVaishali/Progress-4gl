DEFINE VARIABLE shapeObj AS shape NO-UNDO.
shapeObj = NEW RECTANGLE(2,5).
shapeObj:draw().
MESSAGE shapeObj:area() VIEW-AS ALERT-BOX INFORMATION BUTTONS OK TITLE "Rectangle Area".

shapeObj = NEW Triangle(3,5).
shapeObj:draw().
MESSAGE shapeObj:area() VIEW-AS ALERT-BOX INFORMATION BUTTONS OK TITLE "Triangle Area".
