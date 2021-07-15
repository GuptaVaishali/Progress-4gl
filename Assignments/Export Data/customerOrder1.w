&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CustomerReport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS CustomerReport 
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

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FLCustNum FHCustNum FileType FStartDate ~
FEndDate btnSubmit 
&Scoped-Define DISPLAYED-OBJECTS FLCustNum FHCustNum FileType FStartDate ~
FEndDate 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR CustomerReport AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnSubmit 
     LABEL "Submit" 
     SIZE 15 BY 1.14
     BGCOLOR 9 FGCOLOR 9 .

DEFINE VARIABLE FileType AS CHARACTER FORMAT "X(256)":U INITIAL "PDF" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "PDF","Excel","Text File","On Screen" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FEndDate AS DATE FORMAT "99/99/99":U 
     LABEL "End Date" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FHCustNum AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Highest Value" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FLCustNum AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Lowest Value" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FStartDate AS DATE FORMAT "99/99/99":U 
     LABEL "Start Date" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FLCustNum AT ROW 3.86 COL 19 COLON-ALIGNED WIDGET-ID 4
     FHCustNum AT ROW 3.86 COL 53 COLON-ALIGNED WIDGET-ID 12
     FileType AT ROW 6.71 COL 33 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     FStartDate AT ROW 11.48 COL 19 COLON-ALIGNED WIDGET-ID 10
     FEndDate AT ROW 11.48 COL 52 COLON-ALIGNED WIDGET-ID 8
     btnSubmit AT ROW 13.38 COL 31 WIDGET-ID 14
     "Enter Date Range:" VIEW-AS TEXT
          SIZE 24 BY 1.19 AT ROW 9.57 COL 7 WIDGET-ID 26
          BGCOLOR 15 FGCOLOR 9 FONT 6
     "Enter Customer Number Range:" VIEW-AS TEXT
          SIZE 37 BY 1.43 AT ROW 1.95 COL 7 WIDGET-ID 22
          BGCOLOR 15 FGCOLOR 9 FONT 6
     "Select File Type:" VIEW-AS TEXT
          SIZE 21 BY 1.19 AT ROW 6.71 COL 7 WIDGET-ID 24
          BGCOLOR 15 FGCOLOR 9 FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 70.8 BY 14.33 WIDGET-ID 100.


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
  CREATE WINDOW CustomerReport ASSIGN
         HIDDEN             = YES
         TITLE              = "ReportGeneration"
         HEIGHT             = 14.33
         WIDTH              = 70.8
         MAX-HEIGHT         = 32.81
         MAX-WIDTH          = 169.8
         VIRTUAL-HEIGHT     = 32.81
         VIRTUAL-WIDTH      = 169.8
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
/* SETTINGS FOR WINDOW CustomerReport
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(CustomerReport)
THEN CustomerReport:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME CustomerReport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CustomerReport CustomerReport
ON END-ERROR OF CustomerReport /* ReportGeneration */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CustomerReport CustomerReport
ON WINDOW-CLOSE OF CustomerReport /* ReportGeneration */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSubmit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSubmit CustomerReport
ON CHOOSE OF btnSubmit IN FRAME DEFAULT-FRAME /* Submit */
DO:
  MESSAGE  FStartDate:SCREEN-VALUE
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
  DEFINE VARIABLE startDate AS DATE NO-UNDO.
  DEFINE VARIABLE endDate AS DATE NO-UNDO.
  DEFINE VARIABLE i AS INTEGER     NO-UNDO.
  
/*  startDate = DATE (FStartDate:SCREEN-VALUE) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
  DO:
     MESSAGE "Error Occured during Assignment" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
     DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
        MESSAGE ERROR-STATUS:GET-NUMBER(i) ERROR-STATUS:GET-MESSAGE(i) VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
     END.
     FStartDate:SCREEN-VALUE = "".
     APPLY "Entry" TO FStartDate.
  END.
  endDate   = DATE (FEndDate:SCREEN-VALUE) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
  DO:
     MESSAGE "Error Occured during Assignment" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
     DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
        MESSAGE ERROR-STATUS:GET-NUMBER(i) ERROR-STATUS:GET-MESSAGE(i) VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
     END.
     FStartDate:SCREEN-VALUE = "".
     APPLY "Entry" TO FStartDate.
  END.   */
  
  IF FLCustNum:SCREEN-VALUE = String(0) THEN
  DO:
      MESSAGE "PLEASE ENTER VALID CUSTNUM VALUE" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
      APPLY "Entry" TO FLCustNum.
  END.
  ELSE IF FHCustNum:SCREEN-VALUE = String(0) THEN
  DO:
      MESSAGE "PLEASE ENTER VALID CUSTNUM VALUE" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
      APPLY "Entry" TO FHCustNum.
  END.
  ELSE IF FLCustNum:SCREEN-VALUE GT FHCustNum:SCREEN-VALUE THEN
  DO:
      MESSAGE "HIGHEST VALUE SHOULD BE GREATER THAN LOWEST VALUE" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
  END.
  ELSE IF startDate = ? THEN
  DO:
      MESSAGE "PLEASE ENTER START DATE" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
      APPLY "Entry" TO FStartDate.
  END.   
  ELSE IF endDate = ? THEN
  DO:
      MESSAGE "PLEASE ENTER END DATE" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK. 
      APPLY "Entry" TO FEndDate.
  END.
  ELSE IF startDate GT endDate THEN
  DO:
      MESSAGE "END DATE SHOULD BE GREATER THAN START DATE" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
  END.
  
  ELSE 
  DO:
      RUN D:\ExportData\orderReport.p(INPUT FLCustNum:SCREEN-VALUE, INPUT FHCustNum:SCREEN-VALUE, INPUT fileType:SCREEN-VALUE, INPUT FStartDate:SCREEN-VALUE,INPUT FEndDate:SCREEN-VALUE).
  END.          
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK CustomerReport 


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI CustomerReport  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(CustomerReport)
  THEN DELETE WIDGET CustomerReport.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI CustomerReport  _DEFAULT-ENABLE
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
  DISPLAY FLCustNum FHCustNum FileType FStartDate FEndDate 
      WITH FRAME DEFAULT-FRAME IN WINDOW CustomerReport.
  ENABLE FLCustNum FHCustNum FileType FStartDate FEndDate btnSubmit 
      WITH FRAME DEFAULT-FRAME IN WINDOW CustomerReport.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW CustomerReport.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

