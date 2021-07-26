&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports2021       PROGRESS
*/
&Scoped-define WINDOW-NAME contractObj
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS contractObj 
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
DEFINE INPUT PARAMETER given-custNum AS INT.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME browse_Contract

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES fileData

/* Definitions for BROWSE browse_Contract                               */
&Scoped-define FIELDS-IN-QUERY-browse_Contract fileData.CustNum ~
fileData.File-Name fileData.CreatedDateTime 
&Scoped-define ENABLED-FIELDS-IN-QUERY-browse_Contract 
&Scoped-define QUERY-STRING-browse_Contract FOR EACH fileData NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-browse_Contract OPEN QUERY browse_Contract FOR EACH fileData NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-browse_Contract fileData
&Scoped-define FIRST-TABLE-IN-QUERY-browse_Contract fileData


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS browse_Contract Btn_Upload ~
Fill_In_Contract_Path 
&Scoped-Define DISPLAYED-OBJECTS Fill_In_Contract_Path 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR contractObj AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Upload 
     LABEL "Upload" 
     SIZE 18 BY 1.62
     BGCOLOR 9 .

DEFINE VARIABLE Fill_In_Contract_Path AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enter Path" 
     VIEW-AS FILL-IN 
     SIZE 63 BY 1.19
     BGCOLOR 8  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY browse_Contract FOR 
      fileData SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE browse_Contract
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browse_Contract contractObj _STRUCTURED
  QUERY browse_Contract NO-LOCK DISPLAY
      fileData.CustNum FORMAT ">>>>9":U
      fileData.File-Name FORMAT "x(28)":U WIDTH 63.2
      fileData.CreatedDateTime FORMAT "99/99/9999 HH:MM:SS.SSS":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 102 BY 13.57
         TITLE "Contract Details Of Particular Cust Num" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     browse_Contract AT ROW 3.38 COL 9 WIDGET-ID 200
     Btn_Upload AT ROW 18.86 COL 88 WIDGET-ID 10
     Fill_In_Contract_Path AT ROW 19.1 COL 15 COLON-ALIGNED WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 117.6 BY 22.05 WIDGET-ID 100.


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
  CREATE WINDOW contractObj ASSIGN
         HIDDEN             = YES
         TITLE              = "Contract Window"
         HEIGHT             = 22.05
         WIDTH              = 117.6
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
/* SETTINGS FOR WINDOW contractObj
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB browse_Contract 1 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(contractObj)
THEN contractObj:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browse_Contract
/* Query rebuild information for BROWSE browse_Contract
     _TblList          = "Sports2021.fileData"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Sports2021.fileData.CustNum
     _FldNameList[2]   > Sports2021.fileData.File-Name
"fileData.File-Name" ? ? "character" ? ? ? ? ? ? no ? no no "63.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = Sports2021.fileData.CreatedDateTime
     _Query            is NOT OPENED
*/  /* BROWSE browse_Contract */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME contractObj
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contractObj contractObj
ON END-ERROR OF contractObj /* Contract Window */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contractObj contractObj
ON MOUSE-SELECT-DBLCLICK OF contractObj /* Contract Window */
DO:
  MESSAGE "asd" VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contractObj contractObj
ON WINDOW-CLOSE OF contractObj /* Contract Window */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME browse_Contract
&Scoped-define SELF-NAME browse_Contract
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browse_Contract contractObj
ON DEFAULT-ACTION OF browse_Contract IN FRAME DEFAULT-FRAME /* Contract Details Of Particular Cust Num */
DO:
  
  RUN CustMaint\LoadContract\WriteBlobData.p(INPUT fileData.custNum,INPUT fileData.FILE-NAME).
  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Upload
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Upload contractObj
ON CHOOSE OF Btn_Upload IN FRAME DEFAULT-FRAME /* Upload */
DO:
  ASSIGN Fill_In_Contract_Path.
  IF LENGTH(Fill_In_Contract_Path) NE 0 THEN 
  DO:   
  
    DEFINE VARIABLE fullname AS CHARACTER NO-UNDO FORMAT "x(55)".

    fullname = SEARCH(Fill_In_Contract_Path).
    IF fullname = ? THEN
      MESSAGE "UNABLE TO FIND FILE " VIEW-AS ALERT-BOX.
    ELSE
    DO:
        RUN CustMaint\LoadContract\readBlobData.p(INPUT given-custNum, INPUT Fill_In_Contract_Path).
        
        
        OPEN QUERY browse_Contract FOR EACH fileData
        WHERE fileData.custNum = given-custNum NO-LOCK.
    
    END.
        
  END.         
ELSE
    MESSAGE "Enter Something in file path." VIEW-AS ALERT-BOX.
  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK contractObj 


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
  
  OPEN QUERY browse_Contract FOR EACH fileData
        WHERE fileData.custNum = given-custNum NO-LOCK.
        
        
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI contractObj  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(contractObj)
  THEN DELETE WIDGET contractObj.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI contractObj  _DEFAULT-ENABLE
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
  DISPLAY Fill_In_Contract_Path 
      WITH FRAME DEFAULT-FRAME IN WINDOW contractObj.
  ENABLE browse_Contract Btn_Upload Fill_In_Contract_Path 
      WITH FRAME DEFAULT-FRAME IN WINDOW contractObj.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW contractObj.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

