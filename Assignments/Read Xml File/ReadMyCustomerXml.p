DEFINE VARIABLE cSourceType             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReadMode               AS CHARACTER NO-UNDO.
DEFINE VARIABLE lOverrideDefaultMapping AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cFile                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSchemaLocation         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldTypeMapping       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVerifySchemaMode       AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttCustomer NO-UNDO
    FIELD custNum AS INTEGER
    FIELD NAME    AS CHARACTER
    FIELD country AS CHARACTER
    FIELD city    AS CHARACTER
    FIELD state   AS CHARACTER
    FIELD phoneNo AS CHARACTER.
    
DEFINE VARIABLE lReturn AS LOGICAL NO-UNDO.

ASSIGN
    cSourceType             = "File"
    cFile                   = "d:\XMLRead\customer.xml"
    cReadMode               = "Empty"
    cSchemaLocation         = ?
    lOverrideDefaultMapping = ?
    cFieldTypeMapping       = ?
    cVerifySchemaMode       = ?.
    
lReturn = TEMP-TABLE ttCustomer:READ-XML(cSourceType, cFile, cReadMode, 
                                         cSchemaLocation, lOverrideDefaultMapping,
                                         cVerifySchemaMode).
                                                            
IF lReturn THEN
    FOR EACH ttCustomer NO-LOCK:
        DISPLAY custnum NAME country city FORMAT "x(20)" state phoneNo FORMAT "x(30)".
    END.
