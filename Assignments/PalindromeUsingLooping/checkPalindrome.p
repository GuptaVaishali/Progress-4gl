/********Defining Variables********************************/
DEFINE VARIABLE inputStr AS CHARACTER              NO-UNDO.
DEFINE VARIABLE i        AS INTEGER                NO-UNDO.
DEFINE VARIABLE j        AS INTEGER                NO-UNDO.
DEFINE VARIABLE isPalin  AS LOGICAL                NO-UNDO.
DEFINE VARIABLE ans      AS LOGICAL INITIAL "True" NO-UNDO.

/*********Check if String is palindome or not********************/
DO WHILE ans:
    isPalin = TRUE.
    SET inputStr.
    
    i = 1.
    j = LENGTH(inputStr).
    
    DO WHILE i < j:
       IF SUBSTRING(inputStr,i,1) =  SUBSTRING(inputStr,j,1) THEN
       DO:
          i = i + 1.
          j = j - 1.
       END.
       ELSE DO:
           isPalin = FALSE. 
           LEAVE.
       END.
    END.
    
    IF isPalin THEN
       MESSAGE "String is palindrome" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    ELSE
       MESSAGE "String is not palindrome" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    
    MESSAGE "Do you want to Continue (yes/no)" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO SET ans.
END.
