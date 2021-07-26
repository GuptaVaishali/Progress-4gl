/*
    PURPOSE - SEARCHING STRING IN DATABASE TABLES USING DYNAMIC QUERY
    CREATION-DATE - 11-06-21
    AUTHOR-NAME - VAISHALI GUPTA
*/

/***********DEFINING VARIABLES******************************/
DEFINE VARIABLE cTableName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldName AS CHARACTER NO-UNDO.
DEFINE VARIABLE hQuery     AS HANDLE    NO-UNDO.
DEFINE VARIABLE hBuffer    AS HANDLE    NO-UNDO.

DEFINE TEMP-TABLE ttSchema NO-UNDO
    FIELD tableName AS CHARACTER 
    FIELD fieldName AS CHARACTER.

/*****POPULATING TEMP TABLE WITH TABLE NAMES AND FIELDS NAMES**********/
FOR EACH _file WHERE _file-name BEGINS "F":
     FOR EACH _field OF _file:
        IF _field._data-type = "character" THEN
        DO:
            CREATE ttSchema.
            ttSchema.tableName = _file-name.
            ttSchema.fieldName = _field._field-name.
        END.
     END.
END.

FOR EACH ttSchema:
    cTableName = ttSchema.tableName.
    OUTPUT TO VALUE("D:\files\" + cTableName + ".csv") APPEND.
    CREATE QUERY hQuery.
    CREATE BUFFER hBuffer FOR TABLE cTableName.
    
    hQuery:SET-BUFFERS(hBuffer).
    hQuery:QUERY-PREPARE("For Each " + cTableName).
    hQuery:QUERY-OPEN().
    
    REPEAT: 
        hQuery:GET-NEXT().
        IF hQuery:QUERY-OFF-END THEN LEAVE.
        
        cFieldName = ttSchema.fieldName.
                
        /************CHECK IF FIELD CONTAINS THE STRING "VAISHALI" *********************/
        IF INDEX((hBuffer:BUFFER-FIELD(cFieldName):BUFFER-VALUE),"vaishali") > 0 THEN
            /********************EXPORTING DATA INTO CSV FILES************************************/
            EXPORT DELIMITER "," cTableName cFieldName hBuffer:BUFFER-FIELD(cFieldName):BUFFER-VALUE.           
    END.
    
    hQuery:QUERY-CLOSE().
    DELETE OBJECT hQuery.
    DELETE OBJECT hBuffer.
    OUTPUT CLOSE.
END.


