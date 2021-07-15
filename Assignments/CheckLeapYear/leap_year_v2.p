/*********Defining Variables********************/
DEFINE VARIABLE viYear AS INT                  NO-UNDO.
DEFINE VARIABLE vlAns  AS LOGICAL INITIAL TRUE NO-UNDO.

/*********Check if year input by user is leap year or not****************/
DO WHILE vlAns:
    SET viYear.
    IF (viYear MOD 4 = 0 AND viYear MOD 100 <> 0) OR (viYear MOD 400 = 0) THEN
            MESSAGE viYear " IS A LEAP YEAR" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    ELSE
        MESSAGE viYear " IS  NOT A LEAP YEAR" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        
    MESSAGE "DO YOU WANT TO CONTINUE (YES/NO)" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO SET vlAns.
END.
