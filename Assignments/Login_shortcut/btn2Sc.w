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

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE vcUsername      AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcPassword      AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcConfirmPass   AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcRole          AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FUsername FPassword FConfirmPassword FRole ~
btnCrUser 
&Scoped-Define DISPLAYED-OBJECTS FUsername FPassword FConfirmPassword FRole 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD checkInteger C-Win 
FUNCTION checkInteger RETURNS LOGICAL
  ( INPUT pStr AS CHARACTER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD checkSpecialChar C-Win 
FUNCTION checkSpecialChar RETURNS LOGICAL
  ( INPUT pStr AS CHARACTER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD checkUppercase C-Win 
FUNCTION checkUppercase RETURNS LOGICAL
  ( INPUT pStr AS CHARACTER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCrUser 
     LABEL "Create User" 
     SIZE 19 BY 1.67
     BGCOLOR 7 FGCOLOR 7 .

DEFINE VARIABLE FRole AS CHARACTER FORMAT "X(256)":U 
     LABEL "Role" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Admin","HR","IT","DEV","QA" 
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE FConfirmPassword AS CHARACTER FORMAT "X(256)":U 
     LABEL "Confirm Password" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE FPassword AS CHARACTER FORMAT "X(256)":U 
     LABEL "Password" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE FUsername AS CHARACTER FORMAT "X(256)":U 
     LABEL "Username" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FUsername AT ROW 5.76 COL 34 COLON-ALIGNED WIDGET-ID 4
     FPassword AT ROW 7.67 COL 34 COLON-ALIGNED WIDGET-ID 6
     FConfirmPassword AT ROW 9.57 COL 34 COLON-ALIGNED WIDGET-ID 12
     FRole AT ROW 11.48 COL 35 COLON-ALIGNED WIDGET-ID 14
     btnCrUser AT ROW 13.38 COL 35 WIDGET-ID 10
     "Welcome Admin OR HR OR IT Team" VIEW-AS TEXT
          SIZE 63 BY 2.86 AT ROW 1.95 COL 11 WIDGET-ID 2
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 83.4 BY 15.24
         BGCOLOR 1 FGCOLOR 15  WIDGET-ID 100.


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
         TITLE              = "WelcomeAdminHrItTeam"
         HEIGHT             = 15.24
         WIDTH              = 83.4
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 83.4
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 83.4
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* WelcomeAdminHrItTeam */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* WelcomeAdminHrItTeam */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCrUser
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCrUser C-Win
ON CHOOSE OF btnCrUser IN FRAME DEFAULT-FRAME /* Create User */
DO:      
    IF vcUsername = "" THEN
    DO:
       MESSAGE "Please Enter username" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       APPLY "Entry" TO FUsername.
       RETURN.
    END.
    IF vcPassword = "" THEN
    DO:
        MESSAGE "Please Enter Password" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK. 
        APPLY "Entry" TO FPassword.
        RETURN.
    END.
    IF vcConfirmPass = "" THEN
    DO:
        MESSAGE "Please Enter Confirm Password" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.  
        APPLY "Entry" TO FConfirmPassword.
        RETURN.
    END.
    IF vcRole = "" THEN
    DO:
        MESSAGE "Please Enter Role" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK. 
        APPLY "Entry" TO FRole.
        RETURN.
    END.
      
    CREATE LoginTable.
    ASSIGN
        LoginTable.username = vcUsername
        LoginTable.password = vcPassword.
     
    CREATE UserRole.
    ASSIGN
        UserRole.user_id = LoginTable.user_id
        UserRole.ROLE    = vcRole.
        
    MESSAGE "New User Created Successfully" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FConfirmPassword
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FConfirmPassword C-Win
ON LEAVE OF FConfirmPassword IN FRAME DEFAULT-FRAME /* Confirm Password */
DO:
    IF LENGTH(vcConfirmPass) < 8 THEN
    DO:
        MESSAGE "PASSWORD MUST BE 8 CHARACTER LONG" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK. 
        RETURN NO-APPLY.
    END.
    IF checkInteger(INPUT vcConfirmPass) = FALSE THEN
    DO:
        MESSAGE "PLEASE ENTER AT LEAST ONE DIGIT" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN NO-APPLY.
    END.
    IF checkUppercase(INPUT vcConfirmPass) = FALSE THEN
    DO:
        MESSAGE "PLEASE ENTER AT LEAST ONE CAPITAL LETTER" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN NO-APPLY.
    END.
    IF checkSpecialChar(INPUT vcConfirmPass) = FALSE THEN
    DO:
        MESSAGE "PLEASE ENTER AT LEAST ONE SPECIAL LETTER" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN NO-APPLY.
    END.
    IF vcConfirmPass <> vcPassword THEN
    DO:
        MESSAGE "CONFIRM PASSWORD DOES NOT MATCH PASSWORD" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.    
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FConfirmPassword C-Win
ON VALUE-CHANGED OF FConfirmPassword IN FRAME DEFAULT-FRAME /* Confirm Password */
DO:
  vcConfirmPass = FConfirmPassword:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FPassword
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FPassword C-Win
ON LEAVE OF FPassword IN FRAME DEFAULT-FRAME /* Password */
DO:
    IF LENGTH(vcPassword) < 8 THEN
    DO:
        MESSAGE "PASSWORD MUST BE 8 CHARACTER LONG" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK. 
        RETURN NO-APPLY.
    END.  
    IF checkInteger(INPUT vcPassword) = FALSE THEN
    DO:
        MESSAGE "PLEASE ENTER AT LEAST ONE DIGIT" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN NO-APPLY.
    END.
    IF checkUppercase(INPUT vcPassword) = FALSE THEN
    DO:
        MESSAGE "PLEASE ENTER AT LEAST ONE CAPITAL LETTER" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN NO-APPLY.
    END.
    IF checkSpecialChar(INPUT vcPassword) = FALSE THEN
    DO:
        MESSAGE "PLEASE ENTER AT LEAST ONE SPECIAL LETTER" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FPassword C-Win
ON VALUE-CHANGED OF FPassword IN FRAME DEFAULT-FRAME /* Password */
DO:
  vcPassword    = FPassword:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FRole
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRole C-Win
ON VALUE-CHANGED OF FRole IN FRAME DEFAULT-FRAME /* Role */
DO:
  vcRole        = FRole:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FUsername
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FUsername C-Win
ON VALUE-CHANGED OF FUsername IN FRAME DEFAULT-FRAME /* Username */
DO:
  vcUsername    = FUsername:SCREEN-VALUE.
END.

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
  DISPLAY FUsername FPassword FConfirmPassword FRole 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE FUsername FPassword FConfirmPassword FRole btnCrUser 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION checkInteger C-Win 
FUNCTION checkInteger RETURNS LOGICAL
  ( INPUT pStr AS CHARACTER /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:   checks if any integer exists in a string value or not
    Notes:  
------------------------------------------------------------------------------*/
    MESSAGE "inside function " pStr
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    
    DEFINE VARIABLE i  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ch AS CHARACTER NO-UNDO.

    DO i = 1 TO LENGTH(pStr):
        ch = SUBSTRING(pStr,i,1).
        IF ASC(ch) >= 48 AND ASC(ch) <= 57 THEN
           RETURN TRUE.      /* Function return value. */
    END.
    RETURN FALSE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION checkSpecialChar C-Win 
FUNCTION checkSpecialChar RETURNS LOGICAL
  ( INPUT pStr AS CHARACTER /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE i  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ch AS CHARACTER NO-UNDO.

    DO i = 1 TO LENGTH(pStr):
        ch = SUBSTRING(pStr,i,1).
        IF (ASC(ch) >= 32 AND ASC(ch) <= 47) OR (ASC(ch) >= 58 AND ASC(ch) <= 64) 
            OR (ASC(ch) >= 91 AND ASC(ch) <= 96) OR (ASC(ch) >= 123 AND ASC(ch) <= 126) THEN
            RETURN TRUE.     /* Function return value. */
    END.
    RETURN FALSE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION checkUppercase C-Win 
FUNCTION checkUppercase RETURNS LOGICAL
  ( INPUT pStr AS CHARACTER /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE i  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ch AS CHARACTER NO-UNDO.

    DO i = 1 TO LENGTH(pStr):
        ch = SUBSTRING(pStr,i,1).
        IF ASC(ch) >= 65 AND ASC(ch) <= 90 THEN
           RETURN TRUE.    /* Function return value. */
    END.
    RETURN FALSE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

