DEFINE INPUT PARAMETER given-custNum AS INT.
DEFINE INPUT PARAMETER Fill_In_Contract_Path AS CHARACTER  FORMAT "x(256)".


//DEFINE VARIABLE given-custNum AS INT.
//DEFINE VARIABLE Fill_In_Contract_Path AS CHARACTER  FORMAT "x(256)".
//ASSIGN fill_In_contract_Path="D:\data.csv".
DEFINE TEMP-TABLE temp-file NO-UNDO LIKE fileData.
DEFINE VARIABLE v-datetime    AS DATETIME    NO-UNDO.
ASSIGN
  v-datetime = NOW.


IF CAN-FIND(FIRST fileData WHERE fileData.custNum = given-custNum AND fileData.FILE-NAME = substring( Fill_In_Contract_Path, r-index( Fill_In_Contract_Path, "\" ) + 1 ) ) THEN
DO:
     MESSAGE "Customer already existed with that file" VIEW-AS ALERT-BOX.
END.
ELSE
DO :

    CREATE fileData.
    ASSIGN fileData.custNum=given-custNum
        fileData.createdDateTime = v-datetime
        fileData.FILE-NAME = substring( Fill_In_Contract_Path, r-index( Fill_In_Contract_Path, "\" ) + 1 ).
    COPY-LOB FROM FILE Fill_In_Contract_Path TO fileData.blobData.
    
    MESSAGE "File Added " VIEW-AS ALERT-BOX.

 
END .
