USING Progress.Json.ObjectModel.*.

DEFINE VARIABLE salary          AS INT               NO-UNDO EXTENT 3 INITIAL [3,7,6].
DEFINE VARIABLE vlcResponse     AS LONGCHAR          NO-UNDO.
DEFINE VARIABLE oObjModelParser AS ObjectModelParser NO-UNDO.
DEFINE VARIABLE oJsonObj        AS JsonObject        NO-UNDO.
DEFINE VARIABLE propNames       AS CHARACTER         NO-UNDO EXTENT.
DEFINE VARIABLE i               AS INT               NO-UNDO.

vlcResponse = Person:aboutPerson("vaishali",9, 89.00, salary).

MESSAGE SUBSTITUTE("&1" , vlcResponse) VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.    

oObjModelParser = NEW Progress.Json.ObjectModel.ObjectModelParser().
oJsonObj        = CAST(oObjModelParser:Parse(vlcResponse), 
                        Progress.Json.ObjectModel.JsonObject) NO-ERROR.
                        
IF ERROR-STATUS:ERROR THEN
DO:
    MESSAGE "ERROR WHILE PARSING"
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    RETURN.
END.

IF oJsonObj:has("ERROR") THEN
DO:
    MESSAGE SUBSTITUTE("&1" , oJsonObj:GetJsonText("ERROR"))
       VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
END.
ELSE
DO:
    propNames = oJsonObj:GetNames().
    DO i = 1 TO EXTENT(propNames):
        MESSAGE propNames[i] VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        MESSAGE  SUBSTITUTE("&1" , oJsonObj:GetJsonText(propNames[i])) 
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    END.
END.
    



