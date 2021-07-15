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

/*************LOOP FOR EVERY TABLE STARTING WITH "F"**************************/
FOR EACH _file WHERE _file-name BEGINS "F":
    cTableName =  _file-name.
    
    OUTPUT TO VALUE("D:\SearchingStringUsingDynamicQuery\files\" + cTableName + ".csv").
    
    CREATE QUERY hQuery.
    CREATE BUFFER hBuffer FOR TABLE cTableName.
    
    hQuery:SET-BUFFERS(hBuffer).
    hQuery:QUERY-PREPARE("For Each " + cTableName).
    hQuery:QUERY-OPEN().
    
    /********* EXPORT HEADER ROW IN CSV FILE **************/
    EXPORT DELIMITER "," "TableName" "FieldName" "FieldValue".
    
    REPEAT: 
        hQuery:GET-NEXT().
        IF hQuery:QUERY-OFF-END THEN LEAVE.
        
        FOR EACH _field OF _file:
            IF _field._data-type = "character" THEN
            DO:
                cFieldName =  _field._field-name.
                
                /************CHECK IF FIELD CONTAINS THE STRING "VAISHALI" *********************/
                IF INDEX((hBuffer:BUFFER-FIELD(cFieldName):BUFFER-VALUE),"vaishali") > 0 THEN
                    /********************EXPORTING DATA INTO CSV FILES************************************/
                    EXPORT DELIMITER "," cTableName cFieldName hBuffer:BUFFER-FIELD(cFieldName):BUFFER-VALUE.           
            END. 
        END.
    END.   
    
    hQuery:QUERY-CLOSE().
    DELETE OBJECT hQuery.
    DELETE OBJECT hBuffer.
    OUTPUT CLOSE.
END.  
