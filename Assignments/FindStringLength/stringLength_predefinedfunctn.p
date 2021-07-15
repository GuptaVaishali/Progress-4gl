/* 
    PURPOSE - FIND LENGTH OF A STRING USING PREDEFINED FUNCTION.
    AUTHOR NAME - VAISHALI GUPTA
    CREATION DATE - 16-JUN-2021
*/

/************* VARIABLE DEFINITIONS ********************/
DEFINE VARIABLE vcStr AS CHARACTER NO-UNDO FORMAT "x(16)".
DEFINE VARIABLE vlAns AS LOGICAL   NO-UNDO INITIAL TRUE.

/******* DISPLAY LENGTH OF STRING ENTERED BY USER ******/
DO WHILE(vlAns):
    SET vcStr.
    DISPLAY LENGTH(vcStr).
    MESSAGE "DO YOU WANT TO CONTINUE(YES/NO)" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE vlAns.
END.