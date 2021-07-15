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

 DEF VARIABLE operand1 AS CHARACTER NO-UNDO.
 DEF VARIABLE operator AS CHARACTER NO-UNDO.
 DEF VARIABLE operand2 AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Result AcBtn modbtn dividebtn Num7 Num8 Num9 ~
productbtn Num4 Num5 Num6 subtractbtn Num1 Num2 Num3 addbtn Num0 decimalbtn ~
EqBtn 
&Scoped-Define DISPLAYED-OBJECTS Result 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON AcBtn 
     LABEL "AC" 
     SIZE 34 BY 2
     FONT 17.

DEFINE BUTTON addbtn 
     LABEL "+" 
     SIZE 17 BY 2
     FONT 17.

DEFINE BUTTON decimalbtn 
     LABEL "." 
     SIZE 17 BY 2
     FONT 17.

DEFINE BUTTON dividebtn 
     LABEL "/" 
     SIZE 17 BY 2
     FONT 17.

DEFINE BUTTON EqBtn 
     LABEL "=" 
     SIZE 34 BY 2
     FONT 17.

DEFINE BUTTON modbtn 
     LABEL "%" 
     SIZE 17 BY 2
     FONT 17.

DEFINE BUTTON Num0 
     LABEL "0" 
     SIZE 17 BY 2
     FONT 17.

DEFINE BUTTON Num1 
     LABEL "1" 
     SIZE 17 BY 2
     FONT 17.

DEFINE BUTTON Num2 
     LABEL "2" 
     SIZE 17 BY 2
     FONT 17.

DEFINE BUTTON Num3 
     LABEL "3" 
     SIZE 17 BY 2
     FONT 17.

DEFINE BUTTON Num4 
     LABEL "4" 
     SIZE 17 BY 2
     FONT 17.

DEFINE BUTTON Num5 
     LABEL "5" 
     SIZE 17 BY 2
     FONT 17.

DEFINE BUTTON Num6 
     LABEL "6" 
     SIZE 17 BY 2
     FONT 17.

DEFINE BUTTON Num7 
     LABEL "7" 
     SIZE 17 BY 2
     FONT 17.

DEFINE BUTTON Num8 
     LABEL "8" 
     SIZE 17 BY 2
     FONT 17.

DEFINE BUTTON Num9 
     LABEL "9" 
     SIZE 17 BY 2
     FONT 17.

DEFINE BUTTON productbtn 
     LABEL "*" 
     SIZE 17 BY 2
     FONT 17.

DEFINE BUTTON subtractbtn 
     LABEL "-" 
     SIZE 17 BY 2
     FONT 17.

DEFINE VARIABLE Result AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 68 BY 3.81
     FONT 17 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Result AT ROW 4.81 COL 18 COLON-ALIGNED NO-LABEL WIDGET-ID 48
     AcBtn AT ROW 8.62 COL 20 WIDGET-ID 28
     modbtn AT ROW 8.62 COL 54 WIDGET-ID 32
     dividebtn AT ROW 8.62 COL 71 WIDGET-ID 42
     Num7 AT ROW 10.52 COL 20 WIDGET-ID 16
     Num8 AT ROW 10.52 COL 37 WIDGET-ID 18
     Num9 AT ROW 10.52 COL 70 RIGHT-ALIGNED WIDGET-ID 20
     productbtn AT ROW 10.52 COL 71 WIDGET-ID 34
     Num4 AT ROW 12.43 COL 20 WIDGET-ID 10
     Num5 AT ROW 12.43 COL 37 WIDGET-ID 12
     Num6 AT ROW 12.43 COL 54 WIDGET-ID 14
     subtractbtn AT ROW 12.43 COL 71 WIDGET-ID 36
     Num1 AT ROW 14.33 COL 20 WIDGET-ID 4
     Num2 AT ROW 14.33 COL 37 WIDGET-ID 6
     Num3 AT ROW 14.33 COL 54 WIDGET-ID 8
     addbtn AT ROW 14.33 COL 71 WIDGET-ID 38
     Num0 AT ROW 16.24 COL 20 WIDGET-ID 22
     decimalbtn AT ROW 16.24 COL 37 WIDGET-ID 46
     EqBtn AT ROW 16.24 COL 54 WIDGET-ID 44
     "Calculator" VIEW-AS TEXT
          SIZE 26 BY 1.67 AT ROW 1.95 COL 44 WIDGET-ID 2
          FGCOLOR 12 FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 105.2 BY 19.48 WIDGET-ID 100.


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
         TITLE              = "Calculator"
         HEIGHT             = 19.48
         WIDTH              = 104.8
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
/* SETTINGS FOR BUTTON Num9 IN FRAME DEFAULT-FRAME
   ALIGN-R                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Calculator */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Calculator */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME AcBtn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL AcBtn C-Win
ON CHOOSE OF AcBtn IN FRAME DEFAULT-FRAME /* AC */
DO:
  operand1 = "".
  operand2 = "".
  operator = "".
  Result:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME addbtn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL addbtn C-Win
ON CHOOSE OF addbtn IN FRAME DEFAULT-FRAME /* + */
DO:
  operator = "+".
  RESULT:SCREEN-VALUE = operand1 + operator.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME decimalbtn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL decimalbtn C-Win
ON CHOOSE OF decimalbtn IN FRAME DEFAULT-FRAME /* . */
DO:
   RUN AssignOperandsAndPrint(INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dividebtn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dividebtn C-Win
ON CHOOSE OF dividebtn IN FRAME DEFAULT-FRAME /* / */
DO:
  operator = "/".
  RESULT:SCREEN-VALUE = operand1 + operator.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME EqBtn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EqBtn C-Win
ON CHOOSE OF EqBtn IN FRAME DEFAULT-FRAME /* = */
DO:
  IF operator = "+" THEN
  DO:
     RESULT:SCREEN-VALUE = "= " + STRING(dec(operand1) + dec(operand2)).
     operand1 = STRING(dec(operand1) + dec(operand2)).
  END.
  ELSE IF operator = "-" THEN
  DO:
     RESULT:SCREEN-VALUE = "= " + STRING(dec(operand1) - dec(operand2)).
     operand1 = STRING(dec(operand1) - dec(operand2)).
  END.
  ELSE IF operator = "*" THEN
  DO:
     RESULT:SCREEN-VALUE = "= " + STRING(dec(operand1) * dec(operand2)).
     operand1 = STRING(dec(operand1) * dec(operand2)).
  END.
  ELSE IF operator = "/" THEN
  DO:
     RESULT:SCREEN-VALUE = "= " + STRING(dec(operand1) / dec(operand2)).
     operand1 = STRING(dec(operand1) / dec(operand2)).
  END.
  ELSE IF operator = "%" THEN
  DO:
     RESULT:SCREEN-VALUE = "= " + STRING(dec(operand1) MOD dec(operand2)).
     operand1 = STRING(dec(operand1) MOD dec(operand2)).
  END.    
  operand2 = "".
  operator = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME modbtn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL modbtn C-Win
ON CHOOSE OF modbtn IN FRAME DEFAULT-FRAME /* % */
DO:
  operator = "%".
  RESULT:SCREEN-VALUE = operand1 + operator.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Num0
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Num0 C-Win
ON CHOOSE OF Num0 IN FRAME DEFAULT-FRAME /* 0 */
DO:
  RUN AssignOperandsAndPrint(INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Num1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Num1 C-Win
ON CHOOSE OF Num1 IN FRAME DEFAULT-FRAME /* 1 */
DO:
   RUN AssignOperandsAndPrint(INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Num2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Num2 C-Win
ON CHOOSE OF Num2 IN FRAME DEFAULT-FRAME /* 2 */
DO:
  RUN AssignOperandsAndPrint(INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Num3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Num3 C-Win
ON CHOOSE OF Num3 IN FRAME DEFAULT-FRAME /* 3 */
DO:
    RUN AssignOperandsAndPrint(INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Num4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Num4 C-Win
ON CHOOSE OF Num4 IN FRAME DEFAULT-FRAME /* 4 */
DO:
 RUN AssignOperandsAndPrint(INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Num5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Num5 C-Win
ON CHOOSE OF Num5 IN FRAME DEFAULT-FRAME /* 5 */
DO:
  RUN AssignOperandsAndPrint(INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Num6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Num6 C-Win
ON CHOOSE OF Num6 IN FRAME DEFAULT-FRAME /* 6 */
DO:
    RUN AssignOperandsAndPrint(INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Num7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Num7 C-Win
ON CHOOSE OF Num7 IN FRAME DEFAULT-FRAME /* 7 */
DO:
    RUN AssignOperandsAndPrint(INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Num8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Num8 C-Win
ON CHOOSE OF Num8 IN FRAME DEFAULT-FRAME /* 8 */
DO:
   RUN AssignOperandsAndPrint(INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Num9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Num9 C-Win
ON CHOOSE OF Num9 IN FRAME DEFAULT-FRAME /* 9 */
DO:
    RUN AssignOperandsAndPrint(INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME productbtn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL productbtn C-Win
ON CHOOSE OF productbtn IN FRAME DEFAULT-FRAME /* * */
DO:
  operator = "*".
  RESULT:SCREEN-VALUE = operand1 + operator.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME subtractbtn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL subtractbtn C-Win
ON CHOOSE OF subtractbtn IN FRAME DEFAULT-FRAME /* - */
DO:
  operator = "-".
  RESULT:SCREEN-VALUE = operand1 + operator.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AssignOperandsAndPrint C-Win 
PROCEDURE AssignOperandsAndPrint :
/*------------------------------------------------------------------------------
  Purpose: To Assign and Print operand and operators    
  Parameters: button labels
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER cLabelValue AS  CHARACTER NO-UNDO.
  /*if operator is not assigned then put button label value in operand1 else in operand2 */
  IF operator = "" THEN
  DO:
      operand1 = operand1 + cLabelValue.
      Result:SCREEN-VALUE IN FRAME {&FRAME-NAME} = operand1. 
   END.
   ELSE
   DO:
        operand2 = operand2 + cLabelValue.
        Result:SCREEN-VALUE IN FRAME {&FRAME-NAME} = operand1 + operator + operand2.
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY Result 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE Result AcBtn modbtn dividebtn Num7 Num8 Num9 productbtn Num4 Num5 Num6 
         subtractbtn Num1 Num2 Num3 addbtn Num0 decimalbtn EqBtn 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

