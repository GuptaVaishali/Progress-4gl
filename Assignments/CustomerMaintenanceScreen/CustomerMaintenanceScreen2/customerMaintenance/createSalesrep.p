/*********IF SALESREP VALUE DOES NOT EXIST IN SALESREP TABLE, THEN CREATE A RECORD IN SALESREP TABLE***************/

DEFINE INPUT PARAMETER pcSalesRep AS CHARACTER NO-UNDO.

FIND FIRST salesrep WHERE salesRep.sales-rep = pcSalesRep NO-LOCK NO-ERROR.
IF NOT AVAILABLE salesrep THEN
DO:
    CREATE salesrep.
    salesrep.sales-rep = pcSalesRep.
END.
