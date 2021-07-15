/* 
    PURPOSE - FIND LENGTH OF A STRING WITHOUT USING PREDEFINED FUNCTION.
    AUTHOR NAME - VAISHALI GUPTA
    CREATION DATE - 16-JUN-2021
*/

/************* VARIABLE DEFINITIONS ********************/

DEFINE VARIABLE vcStr   AS CHARACTER NO-UNDO FORMAT "x(16)".
DEFINE VARIABLE vlAns   AS LOGICAL   NO-UNDO INITIAL TRUE.
DEFINE VARIABLE ch      AS CHARACTER NO-UNDO.
DEFINE VARIABLE i       AS INTEGER   NO-UNDO.
DEFINE VARIABLE viCount AS INTEGER   NO-UNDO.

DO WHILE(vlAns):
   SET vcStr.
    ch = SUBSTRING(vcStr,1,1).
    i = 2. 
    viCount = 0.
    DO WHILE(ASC(ch) <> -1):
        ch = SUBSTRING(vcStr,i,1).
        i = i + 1.
        viCount = viCount + 1.
    END.
 
    DISPLAY viCount LABEL "String Length". 
    MESSAGE "DO YOU WANT TO CONTINUE(YES/NO)" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE vlAns.
END.       


/*DO WHILE(vlAns):
    SET vcStr.
    i = 1. 
    DO WHILE TRUE:
        ch = SUBSTRING(vcStr,i,1).
        IF ch = "" THEN
           LEAVE. 
        i = i + 1.
    END.
    DISPLAY i - 1. 
    MESSAGE "DO YOU WANT TO CONTINUE(YES/NO)" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE vlAns.
END.      */
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  //  MESSAGE "DO YOU WANT TO CONTINUE(YES/NO)" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE vlAns.
//END.
