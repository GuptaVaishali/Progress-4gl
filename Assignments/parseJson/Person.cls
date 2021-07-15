USING Progress.Lang.*.
USING Progress.Json.ObjectModel.*.
BLOCK-LEVEL ON ERROR UNDO, THROW.
CLASS Person:
    
    DEFINE STATIC VARIABLE oJsonObject AS JsonObject NO-UNDO.
    DEFINE STATIC VARIABLE oJsonArray  AS JsonArray  NO-UNDO.
    DEFINE STATIC VARIABLE vcError     AS CHARACTER  NO-UNDO. 
                               
    METHOD PUBLIC STATIC LONGCHAR aboutPerson(INPUT pcName    AS CHARACTER, 
                                    INPUT piAge     AS INTEGER, 
                                    INPUT pdBalance AS DECIMAL,
                                    INPUT paSalary  AS INTEGER EXTENT):
                                    
         oJsonObject = NEW JsonObject().
         oJsonArray  = NEW JsonArray().
          
         DEFINE VARIABLE i           AS INTEGER   NO-UNDO.
         DEFINE VARIABLE ch          AS CHARACTER NO-UNDO.
         DEFINE VARIABLE isValid     AS LOGICAL   NO-UNDO INITIAL TRUE.
         DEFINE VARIABLE cFile       AS LONGCHAR  NO-UNDO. 
         DEFINE VARIABLE lFormatted  AS LOGICAL   NO-UNDO. 
         
         lFormatted  = TRUE. 
         
         DO i = 1 TO LENGTH(pcName):
            ch = SUBSTRING(pcName,i,1).
            IF ASC(ch) >= 48 AND ASC(ch) <= 57 THEN
            DO:
                isValid = FALSE. 
                LEAVE.
            END.    
         END. 
         
         IF isValid = FALSE THEN
         DO:
            vcError = "INTEGER SHOULD NOT EXIST IN NAME".
            oJsonObject:ADD("ERROR",vcError).
         END.
         
         ELSE IF pcName = "" THEN
         DO:
            vcError = "PLEASE ENTER A VALID NAME".
            oJsonObject:ADD("ERROR",vcError).
         END.
         
         ELSE IF piAge < 0 THEN
         DO:
            vcError = "AGE SHOULD BE GREATER THAN 0".
            oJsonObject:ADD("ERROR",vcError).
         END.
         
         ELSE IF pdBalance < 0 THEN                
         DO:
            vcError = "BALANCE SHOULD BE GREATER THAN 0".
            oJsonObject:ADD("ERROR",vcError).
         END.
         
         ELSE
         DO:
             oJsonObject:ADD("NAME",pcName).
             oJsonObject:ADD("AGE",piAge).
             oJsonObject:ADD("Balance",pdBalance).
             oJsonArray:ADD(paSalary).
             oJsonObject:ADD("SALARY",oJsonArray).                     
         END.
         
         oJsonObject:WRITE(cFile,lFormatted).
         RETURN cFile.
         
      // MESSAGE SUBSTITUTE("&1" , cFile) VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    END METHOD.
END CLASS.
