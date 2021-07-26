DEFINE INPUT PARAMETER custNum AS INTEGER.
DEFINE INPUT PARAMETER FILE-NAME AS CHARACTER.

FIND FIRST fileData WHERE fileData.custNum= custNum AND fileData .FILE-NAME = FILE-NAME NO-LOCK NO-ERROR.
IF AVAILABLE fileData THEN
DO:
COPY-LOB FROM fileData.blobData TO FILE "D:\Final_Project\CustMaint\LoadContract\BlobData\" + FILE-NAME.
MESSAGE "Data is saved into blobdata folder" .    
END.
ELSE
    MESSAGE "tmse na ho paega"
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.


