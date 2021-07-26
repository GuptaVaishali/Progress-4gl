&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME welcomesc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS welcomesc 
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

DEFINE INPUT PARAMETER puserid   AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER pUsername AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE TEMP-TABLE ttUserRole NO-UNDO LIKE userRole.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btn1 btn2 
&Scoped-Define DISPLAYED-OBJECTS username 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR welcomesc AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn1 
     LABEL "Button 1" 
     SIZE 17 BY 1.67
     FGCOLOR 15 .

DEFINE BUTTON btn2 
     LABEL "Button 2" 
     SIZE 17 BY 1.67
     FGCOLOR 15 .

DEFINE VARIABLE username AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1.43
     FONT 17 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     username AT ROW 2.19 COL 9 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     btn1 AT ROW 8.62 COL 11 WIDGET-ID 8
     btn2 AT ROW 8.62 COL 31 WIDGET-ID 10
     "Welcome To Jkt World" VIEW-AS TEXT
          SIZE 41 BY 3.57 AT ROW 4.1 COL 11 WIDGET-ID 2
          BGCOLOR 3 FGCOLOR 15 FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 59.6 BY 10.71
         BGCOLOR 3 FGCOLOR 15  WIDGET-ID 100.


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
  CREATE WINDOW welcomesc ASSIGN
         HIDDEN             = YES
         TITLE              = "Welcome Screen"
         HEIGHT             = 10.71
         WIDTH              = 59.6
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
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
/* SETTINGS FOR WINDOW welcomesc
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN username IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(welcomesc)
THEN welcomesc:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME welcomesc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL welcomesc welcomesc
ON END-ERROR OF welcomesc /* Welcome Screen */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL welcomesc welcomesc
ON WINDOW-CLOSE OF welcomesc /* Welcome Screen */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn1 welcomesc
ON CHOOSE OF btn1 IN FRAME DEFAULT-FRAME /* Button 1 */
DO:
    FIND FIRST ttUserRole WHERE ttUserRole.user_id = puserid NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ttUserRole THEN
    DO:
        IF LOCKED ttUserRole THEN
        DO:
           MESSAGE "userRole is locked by another user"
               VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        END.
        ELSE
            MESSAGE "userRole is not available for this user"
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    END.
    ELSE
    DO:
        IF ttUserRole.ROLE = "DEV" OR ttUserRole.ROLE = "QA" THEN
        DO:
            RUN D:\DNH_Project\Login_shortcut\btn1Sc.w.  
        END.
        ELSE
            MESSAGE "UNAUTHORISED ACCESS" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.  
    END.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn2 welcomesc
ON CHOOSE OF btn2 IN FRAME DEFAULT-FRAME /* Button 2 */
DO:
    FIND FIRST ttUserRole WHERE ttUserRole.user_id = puserid NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ttUserRole THEN
    DO:
        IF LOCKED ttUserRole THEN
        DO:
           MESSAGE "userRole is locked by another user"
               VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        END.
        ELSE
            MESSAGE "userRole is not available for this user"
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    END.
    ELSE
    DO:
        IF ttUserRole.ROLE <> "DEV" AND ttUserRole.ROLE <> "QA" THEN
        DO:
            RUN D:\DNH_Project\Login_shortcut\btn2Sc.w.  
        END.
        ELSE
            MESSAGE "UNAUTHORISED ACCESS" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.  
    END.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK welcomesc 


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
  
  RUN D:\DNH_Project\Login_shortcut\tempUserRoleDef.p(INPUT-OUTPUT TABLE ttUserRole BY-REFERENCE). 
  
  username:SCREEN-VALUE = "Hi " +  pUsername.
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI welcomesc  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(welcomesc)
  THEN DELETE WIDGET welcomesc.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI welcomesc  _DEFAULT-ENABLE
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
  DISPLAY username 
      WITH FRAME DEFAULT-FRAME IN WINDOW welcomesc.
  ENABLE btn1 btn2 
      WITH FRAME DEFAULT-FRAME IN WINDOW welcomesc.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW welcomesc.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

