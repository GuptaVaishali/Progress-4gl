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

DEFINE VARIABLE cTargetType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFile       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFormatted  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lRetOK      AS LOGICAL   NO-UNDO.

DEFINE VARIABLE vlIsValid        AS  LOGICAL  NO-UNDO INITIAL TRUE.

/* Temp-Table Definition ---                                            */ 

DEFINE TEMP-TABLE ttCustomer NO-UNDO
            FIELD custNum AS INTEGER
            FIELD NAME    AS CHARACTER
            FIELD country AS CHARACTER
            FIELD city    AS CHARACTER
            FIELD state   AS CHARACTER
            FIELD phoneNo AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FCustNum FName FCountry FCity FState ~
FPhoneNo btnGenerateFile 
&Scoped-Define DISPLAYED-OBJECTS FCustNum FName FCountry FCity FState ~
FPhoneNo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD checkInitialDigit C-Win 
FUNCTION checkInitialDigit RETURNS LOGICAL
  ( INPUT pStr AS CHARACTER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD checkInteger C-Win 
FUNCTION checkInteger RETURNS LOGICAL
  ( INPUT pStr AS CHARACTER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD isIntegerVal C-Win 
FUNCTION isIntegerVal RETURNS LOGICAL
  ( INPUT pStr AS CHARACTER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnGenerateFile 
     LABEL "Generate Json File" 
     SIZE 23 BY 1.43
     BGCOLOR 9 FGCOLOR 9 .

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


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FCustNum AT ROW 2.91 COL 15 COLON-ALIGNED WIDGET-ID 2
     FName AT ROW 2.91 COL 45 COLON-ALIGNED WIDGET-ID 4
     FCountry AT ROW 4.81 COL 15 COLON-ALIGNED WIDGET-ID 8
     FCity AT ROW 4.81 COL 45 COLON-ALIGNED WIDGET-ID 16
     FState AT ROW 6.71 COL 15 COLON-ALIGNED WIDGET-ID 14
     FPhoneNo AT ROW 6.71 COL 45 COLON-ALIGNED WIDGET-ID 12
     btnGenerateFile AT ROW 8.62 COL 24 WIDGET-ID 24
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 70 BY 9.71 WIDGET-ID 100.


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
         TITLE              = "WriteJson"
         HEIGHT             = 9.71
         WIDTH              = 70
         MAX-HEIGHT         = 32.81
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 32.81
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
ON END-ERROR OF C-Win /* WriteJson */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* WriteJson */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGenerateFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGenerateFile C-Win
ON CHOOSE OF btnGenerateFile IN FRAME DEFAULT-FRAME /* Generate Json File */
DO:
   IF FName:SCREEN-VALUE = "" THEN
   DO:
        MESSAGE "PLEASE ENTER CUSTOMER NAME" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN NO-APPLY.
   END.
   ELSE IF FCountry:SCREEN-VALUE = "" THEN
   DO:
        MESSAGE "PLEASE ENTER CUSTOMER COUNTRY" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN NO-APPLY.
   END.
   ELSE IF FCity:SCREEN-VALUE = "" THEN
   DO:
        MESSAGE "PLEASE ENTER CUSTOMER City" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN NO-APPLY.
   END.
   ELSE IF FState:SCREEN-VALUE = "" THEN
   DO:
        MESSAGE "PLEASE ENTER CUSTOMER STATE" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN NO-APPLY.
   END.
   ELSE IF FPhoneNo:SCREEN-VALUE = "" THEN
   DO:
        MESSAGE "PLEASE ENTER CUSTOMER PHONE" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN NO-APPLY.
   END.
   ELSE
   DO:
        CREATE ttCustomer.

        /* Code to populate the temp-table */ 
         ASSIGN 
          ttCustomer.custNum = INTEGER (FCustNum:SCREEN-VALUE)
          ttCustomer.NAME    = FName:SCREEN-VALUE
          ttCustomer.country = FCountry:SCREEN-VALUE
          ttCustomer.city    = FCity:SCREEN-VALUE
          ttCustomer.state   = FState:SCREEN-VALUE
          ttCustomer.phoneNo = FPhoneNo:SCREEN-VALUE.
          
         ASSIGN  
          cTargetType = "file" 
          cFile       = "d:\WriteJson\ttCustomer.json" 
          lFormatted  = TRUE. 
          
        lRetOK = TEMP-TABLE ttCustomer:WRITE-JSON(cTargetType, cFile, lFormatted).
   
        IF lRetOk THEN
            MESSAGE "Json File Created Successfully" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        ELSE
            MESSAGE "Json FILE NOT CREATED" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FCity
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FCity C-Win
ON LEAVE OF FCity IN FRAME DEFAULT-FRAME /* City */
DO:
/*     IF FCity:SCREEN-VALUE = "" THEN                                                   */
/*     DO:                                                                               */
/*        MESSAGE "PLEASE ENTER CUSTOMER CITY" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK. */
/* /*        APPLY "Entry" TO FCity. */                                                  */
/* /*        vlIsValid = FALSE.      */                                                  */
/*        RETURN NO-APPLY.                                                               */
/*     END.                                                                              */
/*                                                                                       */
    IF checkInteger(INPUT FCity:SCREEN-VALUE) = TRUE THEN
    DO:
        MESSAGE "PLEASE ENTER VALID CUSTOMER CITY" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
/*         APPLY "Entry" TO FCity. */
/*         vlIsValid = FALSE.      */
        RETURN NO-APPLY.
    END.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FCountry
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FCountry C-Win
ON LEAVE OF FCountry IN FRAME DEFAULT-FRAME /* Country */
DO:
/*     IF FCountry:SCREEN-VALUE = "" THEN                                                   */
/*     DO:                                                                               */
/*        MESSAGE "PLEASE ENTER CUSTOMER COUNTRY" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK. */
/* /*        APPLY "Entry" TO FName. */                                                  */
/* /*        vlIsValid = FALSE.      */                                                  */
/*        RETURN NO-APPLY.                                                               */
/*     END.                                                                              */
/*                                                                                       */
    IF checkInteger(INPUT FCountry:SCREEN-VALUE) = TRUE THEN
    DO:
        MESSAGE "PLEASE ENTER VALID CUSTOMER COUNTRY" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
/*         APPLY "Entry" TO FName. */
/*         vlIsValid = FALSE.      */
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FCustNum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FCustNum C-Win
ON LEAVE OF FCustNum IN FRAME DEFAULT-FRAME /* CustNum */
DO:
    IF FCustNum:SCREEN-VALUE = String(0) THEN
    DO:
       MESSAGE "PLEASE ENTER VALID CustNum" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
/*        APPLY "Entry" TO FCustNum. */
/*        vlIsValid = FALSE.         */
       RETURN NO-APPLY.
    END.
/*     IF isIntegerVal(INPUT FCustNum:SCREEN-VALUE) = FALSE THEN                                         */
/*     DO:                                                                                               */
/*        MESSAGE "PLEASE ENTER ONLY INTEGER VALUE OF CustNum" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK. */
/*        APPLY "Entry" TO FCustNum.                                                                     */
/*        vlIsValid = FALSE.                                                                             */
/*     END.                                                                                              */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FName C-Win
ON LEAVE OF FName IN FRAME DEFAULT-FRAME /* Name */
DO:
/*     IF FName:SCREEN-VALUE = "" THEN                                                   */
/*     DO:                                                                               */
/*        MESSAGE "PLEASE ENTER CUSTOMER NAME" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK. */
/*      //  APPLY "Entry" TO FName.                                                      */
/*        RETURN NO-APPLY.                                                               */
/* /*        vlIsValid = FALSE. */                                                       */
/*     END.                                                                              */
    
    IF checkInteger(INPUT FName:SCREEN-VALUE) = TRUE THEN
    DO:
        MESSAGE "PLEASE ENTER VALID CUSTOMER NAME" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
     //   APPLY "Entry" TO FName.
        RETURN NO-APPLY.
/*         vlIsValid = FALSE. */
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FPhoneNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FPhoneNo C-Win
ON LEAVE OF FPhoneNo IN FRAME DEFAULT-FRAME /* PhoneNo */
DO:
/*     IF FPhoneNo:SCREEN-VALUE = "" THEN                                                 */
/*     DO:                                                                                */
/*        MESSAGE "PLEASE ENTER CUSTOMER PHONE" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK. */
/* /*        APPLY "Entry" TO FPhoneNo. */                                                */
/* /*        vlIsValid = FALSE.         */                                                */
/*         RETURN NO-APPLY.                                                               */
/*     END.                                                                               */
/*                                                                                        */
    IF LENGTH(FPhoneNo:SCREEN-VALUE) <> 10 THEN
    DO:
       MESSAGE "PLEASE ENTER 10 DIGIT PHONE" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
/*        APPLY "Entry" TO FPhoneNo. */
/*        vlIsValid = FALSE.         */
        RETURN NO-APPLY.
    END.
    
    ELSE IF isIntegerVal(FPhoneNo:SCREEN-VALUE) = FALSE THEN
    DO:
       MESSAGE "PLEASE ENTER ONLY INTEGER VALUE OF PHONE" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
/*        APPLY "Entry" TO FPhoneNo. */
/*        vlIsValid = FALSE.         */
        RETURN NO-APPLY.
    END.
    
    ELSE IF checkInitialDigit(FPhoneNo:SCREEN-VALUE) = FALSE THEN
    DO:
       MESSAGE "PHONE NO SHOULD BE START FORM 6,7,8 OR 9" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
/*        APPLY "Entry" TO FPhoneNo. */
/*        vlIsValid = FALSE.         */
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FState
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FState C-Win
ON LEAVE OF FState IN FRAME DEFAULT-FRAME /* State */
DO:
/*     IF FState:SCREEN-VALUE = "" THEN                                                   */
/*     DO:                                                                                */
/*        MESSAGE "PLEASE ENTER CUSTOMER STATE" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK. */
/* /*        APPLY "Entry" TO FState. */                                                  */
/* /*        vlIsValid = FALSE.       */                                                  */
/*         RETURN NO-APPLY.                                                               */
/*     END.                                                                               */
    
    IF checkInteger(INPUT FState:SCREEN-VALUE) = TRUE THEN
    DO:
        MESSAGE "PLEASE ENTER VALID CUSTOMER STATE" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
/*         APPLY "Entry" TO FState. */
/*         vlIsValid = FALSE.       */
        RETURN NO-APPLY.
    END.
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
  DISPLAY FCustNum FName FCountry FCity FState FPhoneNo 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE FCustNum FName FCountry FCity FState FPhoneNo btnGenerateFile 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION checkInitialDigit C-Win 
FUNCTION checkInitialDigit RETURNS LOGICAL
  ( INPUT pStr AS CHARACTER /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE firstDigit AS INTEGER NO-UNDO.
  firstDigit = INTEGER(SUBSTRING(pStr,1,1)).
  IF firstDigit <> 6 AND firstDigit <> 7 AND firstDigit <> 8 AND firstDigit <> 9 THEN
    RETURN FALSE.    /* Function return value. */
    
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION checkInteger C-Win 
FUNCTION checkInteger RETURNS LOGICAL
  ( INPUT pStr AS CHARACTER /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  CHECKS IF INTEGER EXISTS IN SCREEN-VALUE OR NOT.
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE ch AS CHARACTER NO-UNDO.

  DO i = 1 TO LENGTH(pStr):
    ch = SUBSTRING(pStr,i,1).
    IF ASC(ch) >= 48 AND ASC(ch) <= 57 THEN
        RETURN TRUE.  
  END.
//  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION isIntegerVal C-Win 
FUNCTION isIntegerVal RETURNS LOGICAL
  ( INPUT pStr AS CHARACTER /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  CHECKS IF SCREEN-VALUE IS INTEGER VALUE OR NOT.
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE isInt AS INTEGER NO-UNDO.
  DEFINE VARIABLE i     AS INTEGER NO-UNDO.
  DO i = 1 TO LENGTH(pStr):
    isInt = INTEGER(SUBSTRING(pStr,i,1)) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN FALSE.   /* Function return value. */
  END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

