/* 
    PURPOSE - REVERSE OF ARRAY.
    AUTHOR NAME - VAISHALI GUPTA
    CREATION DATE - 16-JUN-2021
*/

/********* VARIABLES DEFINITIONS *********************************/
DEFINE VARIABLE iArr  AS INTEGER NO-UNDO EXTENT 4 INITIAL [2,4,8,9].
DEFINE VARIABLE iTemp AS INTEGER NO-UNDO.
DEFINE VARIABLE i     AS INTEGER NO-UNDO.
DEFINE VARIABLE j     AS INTEGER NO-UNDO.

ASSIGN i = 1
       j = EXTENT(iArr).

/******REVERSING ELEMENTS OF ARRAY*******/       
DO WHILE i < j:
    iTemp = iArr[i].
    iArr[i] = iArr[j].
    iArr[j] = iTemp.
    i = i + 1.
    j = j - 1.
END.

/***** DISPLAY ELEMENTS OF REVERSED ARRAY **********/
DO i = 1 TO EXTENT(iArr) WITH FRAME f:
    DISPLAY iArr[i].
END.
            
