DEFINE VARIABLE myCalcObj AS MyCalculation NO-UNDO.
myCalcObj = NEW MyCalculation("vaishali").
myCalcObj:addition(5,6).
myCalcObj:subtraction(6,2).
myCalcObj:show().  



/*DEFINE VARIABLE CalcObj AS Calculation NO-UNDO.
CalcObj = NEW MyCalculation().
CalcObj:addition(5,6).
//CalcObj:subtraction(6,2).
CalcObj:show(). */
