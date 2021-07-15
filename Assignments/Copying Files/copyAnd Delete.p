DEFINE VARIABLE sourcefilename  AS CHARACTER NO-UNDO.
DEFINE VARIABLE destinationfile AS CHARACTER NO-UNDO FORMAT "x(20)" VIEW-AS FILL-IN.


SYSTEM-DIALOG GET-FILE sourcefilename
TITLE  "Choose File to Copy"
MUST-EXIST
USE-FILENAME.


getfile:
DO ON ERROR UNDO,RETRY:
     SET destinationfile.
     /******************EXTRACTING DIRECTORY PATH BY EXCLUDING FILENAME FROM THE VALUE ENTERED BY USER**************************/
     FILE-INFO:FILE-NAME = SUBSTRING(destinationfile,1,LENGTH(destinationfile) - LENGTH(SUBSTRING(destinationfile,(R-INDEX(destinationfile,"\")) + 1))).
     IF FILE-INFO:FULL-PATHNAME EQ ? THEN
     DO:
        MESSAGE "The path specified does not exist!" VIEW-AS ALERT-BOX.
        UNDO getfile, RETRY getfile.
     END.      
     ELSE
     DO:     
        IF SEARCH(destinationfile) EQ destinationfile THEN
        DO:
            MESSAGE "A file named" destinationfile "exists, Please use another name" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            UNDO  getfile, RETRY getfile.
        END.
     END.
END.
OS-COPY VALUE(sourcefilename) VALUE(destinationfile).

IF OS-ERROR <> 0 THEN DO:
    MESSAGE "An error occured" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
END.

OS-DELETE VALUE(sourcefilename).
IF OS-ERROR <> 0 THEN DO:
    MESSAGE "An error occured" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
END.



