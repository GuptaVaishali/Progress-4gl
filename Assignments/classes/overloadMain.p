DEFINE VARIABLE overloadObj AS overload NO-UNDO.
overloadObj = NEW overload().
DEFINE VARIABLE viNum1 AS INTEGER NO-UNDO.
DEFINE VARIABLE viNum2 AS INTEGER NO-UNDO.
DEFINE VARIABLE viNum3 AS INTEGER NO-UNDO.
SET viNum1 viNum2 viNum3.
overloadObj:addition(viNum1,viNum2,viNum3).
overloadObj:addition(viNum1,viNum2).
