&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions --- */

DEFINE TEMP-TABLE ttCustomer NO-UNDO
        FIELD custNum AS INTEGER
        FIELD NAME    AS CHARACTER
        FIELD country AS CHARACTER
        FIELD city    AS CHARACTER
        FIELD state   AS CHARACTER
        FIELD phoneNo AS CHARACTER.  
        
DEFINE VARIABLE cSourceType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReadMode   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFile       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRetOK      AS LOGICAL   NO-UNDO.
//DEFINE VARIABLE httCust     AS HANDLE    NO-UNDO.
DEFINE VARIABLE lReturn     AS LOGICAL   NO-UNDO.

//CREATE TEMP-TABLE httCust.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 FFilePath btnReadJson 
&Scoped-Define DISPLAYED-OBJECTS FFilePath FCustNum FName FCountry FCity ~
FState FPhoneNo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnReadJson 
     LABEL "Read Json" 
     SIZE 17 BY 1.38
     BGCOLOR 9 FGCOLOR 9 FONT 6.

DEFINE VARIABLE FCity AS CHARACTER FORMAT "X(256)":U 
     LABEL "City" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FCountry AS CHARACTER FORMAT "X(256)":U 
     LABEL "Country" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FCustNum AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "CustNum" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FFilePath AS CHARACTER FORMAT "X(256)":U 
     LABEL "File Path" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     BGCOLOR 15 FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE VARIABLE FName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Name" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FPhoneNo AS CHARACTER FORMAT "X(256)":U 
     LABEL "PhoneNo" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FState AS CHARACTER FORMAT "X(256)":U 
     LABEL "State" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    ROUNDED 
     SIZE 55 BY 4.29
     BGCOLOR 9 FGCOLOR 8 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 55 BY 5.71
     BGCOLOR 8 FGCOLOR 9 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FFilePath AT ROW 2.91 COL 16 COLON-ALIGNED WIDGET-ID 4
     btnReadJson AT ROW 4.81 COL 25 WIDGET-ID 22
     FCustNum AT ROW 8.62 COL 13 COLON-ALIGNED WIDGET-ID 6
     FName AT ROW 8.62 COL 39 COLON-ALIGNED WIDGET-ID 8
     FCountry AT ROW 10.29 COL 13 COLON-ALIGNED WIDGET-ID 10
     FCity AT ROW 10.29 COL 39 COLON-ALIGNED WIDGET-ID 12
     FState AT ROW 12.19 COL 13 COLON-ALIGNED WIDGET-ID 24
     FPhoneNo AT ROW 12.19 COL 39 COLON-ALIGNED WIDGET-ID 26
     RECT-1 AT ROW 2.43 COL 5 WIDGET-ID 28
     RECT-2 AT ROW 8.14 COL 5 WIDGET-ID 30
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 63.4 BY 13.81 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "JsonRead"
         HEIGHT             = 13.81
         WIDTH              = 63.4
         MAX-HEIGHT         = 33.57
         MAX-WIDTH          = 273.2
         VIRTUAL-HEIGHT     = 33.57
         VIRTUAL-WIDTH      = 273.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN FCity IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FCountry IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FCustNum IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FName IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FPhoneNo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FState IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* JsonRead */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* JsonRead */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReadJson
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReadJson C-Win
ON CHOOSE OF btnReadJson IN FRAME DEFAULT-FRAME /* Read Json */
DO:
    DEFINE VARIABLE vcFilePath AS CHARACTER NO-UNDO.
    vcFilePath =  FFilePath:SCREEN-VALUE.
    
    MESSAGE vcFilePath VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    
    
    getfile:
    DO ON ERROR UNDO,RETRY:
         /******************EXTRACTING DIRECTORY PATH BY EXCLUDING FILENAME FROM THE VALUE ENTERED BY USER**************************/
         FILE-INFO:FILE-NAME = SUBSTRING(vcFilePath,1,LENGTH(vcFilePath) - LENGTH(SUBSTRING(vcFilePath,(R-INDEX(vcFilePath,"\")) + 1))).
         IF FFilePath:SCREEN-VALUE = "" THEN
         DO:
            MESSAGE "PLEASE ENTER FILE PATH" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            UNDO getfile, RETRY getfile.
         END.
         ELSE IF FILE-INFO:FULL-PATHNAME EQ ? THEN
         DO:
            MESSAGE "THE PATH SPECIFIED DOES NOT EXIST!" VIEW-AS ALERT-BOX.
            UNDO getfile, RETRY getfile.
         END.      
         ELSE IF SUBSTRING(vcFilePath,(R-INDEX(vcFilePath,".")) + 1) NE "json" THEN
         DO:
            MESSAGE "PLEASE ENTER JSON FILE PATH" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            UNDO  getfile, RETRY getfile.
         END.
         ELSE IF SEARCH(vcFilePath) NE vcFilePath THEN
         DO:
                MESSAGE "A FILE NAMED" vcFilePath "DOES NOT EXISTS" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
                UNDO  getfile, RETRY getfile.  
         END.
         ELSE
         DO:          
            ASSIGN
              cSourceType = "file"
              cFile       = vcFilePath 
              cReadMode   = "empty".
            
            lReturn = TEMP-TABLE ttCustomer:READ-JSON(cSourceType, cFile, cReadMode).
                                                                        
            IF lReturn THEN
                FOR EACH ttCustomer NO-LOCK:
                    ASSIGN
                        FCustNum:SCREEN-VALUE = STRING(custnum)
                        FName:SCREEN-VALUE    = NAME
                        FCountry:SCREEN-VALUE = country
                        FCity:SCREEN-VALUE    = city
                        FState:SCREEN-VALUE   = state
                        FPhoneNo:SCREEN-VALUE = phoneNo.
                END.
            END.   /* if lReturn end */
         END.  /* else end */
    END.    /* getfile end */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY FFilePath FCustNum FName FCountry FCity FState FPhoneNo 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 RECT-2 FFilePath btnReadJson 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

