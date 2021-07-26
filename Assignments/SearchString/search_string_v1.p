/*
    PURPOSE - SEARCHING STRING IN DATABASE TABLES USING DYNAMIC QUERY
    CREATION-DATE - 10-06-21
    AUTHOR-NAME - VAISHALI GUPTA
*/

/***********DEFINING VARIABLES******************************/
DEFINE VARIABLE cTableName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldName AS CHARACTER NO-UNDO.
DEFINE VARIABLE hQuery     AS HANDLE    NO-UNDO.
DEFINE VARIABLE hBuffer    AS HANDLE    NO-UNDO.
DEFINE VARIABLE cFileName  AS CHARACTER NO-UNDO FORMAT "x(20)".
DEFINE VARIABLE ix         AS INTEGER   NO-UNDO.
DEFINE VARIABLE iloop      AS INT       NO-UNDO.


FOR EACH _file WHERE _file-name BEGINS "F" NO-LOCK:
    /*******FIND TABLE NAME***********/
    cTableName =  _file-name.
    
    CREATE QUERY hQuery.
    CREATE BUFFER hBuffer FOR TABLE cTableName.
    
    hQuery:SET-BUFFERS(hBuffer).
    
    /************ASK FILENAME FROM USER TO EXPORT THE RECORDS************************************************/
    getfile:
    DO ON ERROR UNDO,RETRY:
         SET cFileName.
         /******************EXTRACTING DIRECTORY PATH BY EXCLUDING FILENAME FROM THE VALUE ENTERED BY USER**************************/
         FILE-INFO:FILE-NAME = SUBSTRING(cfilename,1,LENGTH(cFileName) - LENGTH(SUBSTRING(cFilename,(R-INDEX(cfilename,"\")) + 1))).
         IF FILE-INFO:FULL-PATHNAME EQ ? THEN
         DO:
            MESSAGE "The path specified does not exist!" VIEW-AS ALERT-BOX.
            UNDO getfile, RETRY getfile.
         END.      
         ELSE
         DO:     
            IF SEARCH(cFileName) = cFileName THEN
            DO:
                MESSAGE "A file named" cFileName "already exists, Please use another name" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
                UNDO  getfile, RETRY getfile.
            END.
         END.
    END.
   
    OUTPUT TO VALUE(cFileName).
     
    /********* EXPORT HEADER ROW IN CSV FILE **************/
    DO iLoop = 1 TO hBuffer:NUM-FIELDS:
        PUT UNFORMATTED hBuffer:BUFFER-FIELD(iLoop):NAME.
        IF iLoop < hBuffer:NUM-FIELDS THEN
            PUT UNFORMATTED ",".
    END.
    PUT SKIP.
    
    FOR EACH _field OF _file:
        IF _field._data-type = "character" THEN
        DO:  
            /***********FIND FIELD NAME**************/
            cFieldName =  _field._field-name.
            
            hQuery:QUERY-PREPARE("For Each " + cTableName).
            hQuery:QUERY-OPEN().
            
            REPEAT:
                hQuery:GET-NEXT().
                IF hQuery:QUERY-OFF-END THEN LEAVE.
                
                /************CHECK IF FIELD CONTAINS THE STRING "VAISHALI" *********************/
                IF INDEX((hBuffer:BUFFER-FIELD(cFieldName):BUFFER-VALUE),"vaishali") > 0 THEN
                DO:
                    /***************EXPORTING DATA INTO CSV FILES******************/
                    REPEAT ix = 1 TO hBuffer:NUM-FIELDS:
                        PUT UNFORMATTED hBuffer:BUFFER-FIELD(ix):BUFFER-VALUE.
                        IF ix < hBuffer:NUM-FIELDS THEN
                            PUT UNFORMATTED ",".
                    END.  
                    PUT SKIP.
                END.
            END.      
        END.     
    END.
    
    hQuery:QUERY-CLOSE().
    DELETE OBJECT hQuery.
    DELETE OBJECT hBuffer.
    OUTPUT CLOSE.
END.      
