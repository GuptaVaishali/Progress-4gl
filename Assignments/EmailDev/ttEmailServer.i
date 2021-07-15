/*------------------------------------------------------------------------
    File        : ttEmailServer.i
    Purpose     : Temp-table for Email Server Settings

    Syntax      : 

    Description : 

    Author(s)   : RGupta
    Created     : Fri Jun 17 17:25:56 2021
    Notes       : 
 ----------------------------------------------------------------------*/
DEFINE TEMP-TABLE ttEmailServer{&AppendName} NO-UNDO {&reference-only}
    FIELD EmailServer              AS CHARACTER
    FIELD EmailPort                AS INTEGER 
    FIELD IsBodyHtml               AS LOGICAL
.   