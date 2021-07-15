&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports1          PROGRESS
*/
&Scoped-define WINDOW-NAME Customer-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Customer-Win 
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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Customer

/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define FIELDS-IN-QUERY-DEFAULT-FRAME Customer.Cust-Num ~
Customer.Name Customer.City Customer.Postal-Code Customer.Address ~
Customer.State Customer.Address2 Customer.Phone Customer.Contact ~
Customer.Country Customer.Credit-Limit Customer.Balance Customer.Terms ~
Customer.Discount Customer.Comments 
&Scoped-define ENABLED-FIELDS-IN-QUERY-DEFAULT-FRAME Customer.Cust-Num ~
Customer.Name Customer.City Customer.Postal-Code Customer.Address ~
Customer.State Customer.Address2 Customer.Phone Customer.Contact ~
Customer.Country Customer.Credit-Limit Customer.Balance Customer.Terms ~
Customer.Discount Customer.Comments 
&Scoped-define ENABLED-TABLES-IN-QUERY-DEFAULT-FRAME Customer
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-DEFAULT-FRAME Customer
&Scoped-define QUERY-STRING-DEFAULT-FRAME FOR EACH Customer SHARE-LOCK
&Scoped-define OPEN-QUERY-DEFAULT-FRAME OPEN QUERY DEFAULT-FRAME FOR EACH Customer SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DEFAULT-FRAME Customer
&Scoped-define FIRST-TABLE-IN-QUERY-DEFAULT-FRAME Customer


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Customer.Cust-Num Customer.Name Customer.City ~
Customer.Postal-Code Customer.Address Customer.State Customer.Address2 ~
Customer.Phone Customer.Contact Customer.Country Customer.Credit-Limit ~
Customer.Balance Customer.Terms Customer.Discount Customer.Comments 
&Scoped-define ENABLED-TABLES Customer
&Scoped-define FIRST-ENABLED-TABLE Customer
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-3 BtnFirst BtnNext ~
BtnPrev BUTTON-1 BUTTON-3 BtnCancel BtnDone BtnLast BUTTON-2 BtnOK 
&Scoped-Define DISPLAYED-FIELDS Customer.Cust-Num Customer.Name ~
Customer.City Customer.Postal-Code Customer.Address Customer.State ~
Customer.Address2 Customer.Phone Customer.Contact Customer.Country ~
Customer.Credit-Limit Customer.Balance Customer.Terms Customer.Discount ~
Customer.Comments 
&Scoped-define DISPLAYED-TABLES Customer
&Scoped-define FIRST-DISPLAYED-TABLE Customer


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR Customer-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Done" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BtnFirst 
     LABEL "&First" 
     SIZE 15 BY 1.14
     BGCOLOR 13 FGCOLOR 13 .

DEFINE BUTTON BtnLast 
     LABEL "&Last" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BtnNext 
     LABEL "&Next" 
     SIZE 15 BY 1.14
     BGCOLOR 8 FGCOLOR 12 .

DEFINE BUTTON BtnOK AUTO-GO DEFAULT 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BtnPrev 
     LABEL "&Prev" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-1 
     LABEL "Button 1" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-2 
     LABEL "Button 2" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-3 
     LABEL "Btn 3" 
     SIZE 13 BY 1.19.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 92 BY 6.9 TOOLTIP "contact info"
     FGCOLOR 4 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 88 BY 3.57
     BGCOLOR 14 FGCOLOR 13 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DEFAULT-FRAME FOR 
      Customer SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Customer.Cust-Num AT ROW 3.38 COL 22 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     Customer.Name AT ROW 3.62 COL 59 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     Customer.City AT ROW 4.57 COL 19 COLON-ALIGNED WIDGET-ID 12
          LABEL "city"
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     Customer.Postal-Code AT ROW 4.81 COL 60 COLON-ALIGNED WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     Customer.Address AT ROW 5.52 COL 19 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     Customer.State AT ROW 6 COL 60 COLON-ALIGNED WIDGET-ID 26
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     Customer.Address2 AT ROW 6.71 COL 21 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     Customer.Phone AT ROW 7.19 COL 60 COLON-ALIGNED WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     Customer.Contact AT ROW 8.14 COL 22 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     Customer.Country AT ROW 8.38 COL 61 COLON-ALIGNED WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     Customer.Credit-Limit AT ROW 11.24 COL 21 COLON-ALIGNED WIDGET-ID 32
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     Customer.Balance AT ROW 11.24 COL 55 COLON-ALIGNED WIDGET-ID 30
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     Customer.Terms AT ROW 12.67 COL 17 COLON-ALIGNED WIDGET-ID 36
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
          BGCOLOR 14 FGCOLOR 14 
     Customer.Discount AT ROW 12.67 COL 57 COLON-ALIGNED WIDGET-ID 34
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     BtnFirst AT ROW 14.57 COL 76 WIDGET-ID 46
     Customer.Comments AT ROW 14.81 COL 15 NO-LABEL WIDGET-ID 28
          VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
          SIZE 56 BY 2.38
     BtnNext AT ROW 16 COL 77 WIDGET-ID 48
     BtnPrev AT ROW 17.43 COL 77 WIDGET-ID 50
     BUTTON-1 AT ROW 17.67 COL 5 WIDGET-ID 38
     BUTTON-3 AT ROW 17.91 COL 22 WIDGET-ID 42
     BtnCancel AT ROW 17.91 COL 37.6 WIDGET-ID 56
     BtnDone AT ROW 17.91 COL 55 WIDGET-ID 44
     BtnLast AT ROW 18.86 COL 78.2 WIDGET-ID 52
     BUTTON-2 AT ROW 19.1 COL 5 WIDGET-ID 40
     BtnOK AT ROW 19.33 COL 56 WIDGET-ID 54
     "contact info" VIEW-AS TEXT
          SIZE 15 BY 1.19 AT ROW 2.19 COL 5 WIDGET-ID 60
          BGCOLOR 11 
     "Cust - Info" VIEW-AS TEXT
          SIZE 11 BY 1.43 AT ROW 1.48 COL 43 WIDGET-ID 58
          BGCOLOR 0 FGCOLOR 15 
     RECT-1 AT ROW 3.14 COL 8 WIDGET-ID 2
     RECT-2 AT ROW 6.48 COL 19 WIDGET-ID 4
     RECT-3 AT ROW 10.76 COL 10 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 100 BY 20
         DEFAULT-BUTTON BtnOK CANCEL-BUTTON BtnCancel WIDGET-ID 100.


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
  CREATE WINDOW Customer-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "CUSTOMER information"
         HEIGHT             = 23.5
         WIDTH              = 123
         MAX-HEIGHT         = 23.5
         MAX-WIDTH          = 123
         VIRTUAL-HEIGHT     = 23.5
         VIRTUAL-WIDTH      = 123
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
/* SETTINGS FOR WINDOW Customer-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       BUTTON-3:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN Customer.City IN FRAME DEFAULT-FRAME
   EXP-LABEL                                                            */
ASSIGN 
       RECT-1:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "contact-info".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(Customer-Win)
THEN Customer-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _TblList          = "sports1.Customer"
     _Query            is OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Customer-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Customer-Win Customer-Win
ON END-ERROR OF Customer-Win /* CUSTOMER information */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Customer-Win Customer-Win
ON WINDOW-CLOSE OF Customer-Win /* CUSTOMER information */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone Customer-Win
ON CHOOSE OF BtnDone IN FRAME DEFAULT-FRAME /* Done */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME BtnFirst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnFirst Customer-Win
ON CHOOSE OF BtnFirst IN FRAME DEFAULT-FRAME /* First */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN notify IN THIS-PROCEDURE ("get-first") NO-ERROR.
    &ELSE
      PUBLISH "fetchFirst":U.
    &ENDIF
  &ELSEIF "{&TABLES-IN-QUERY-{&FRAME-NAME}}" ne "" &THEN
  /* This is a simple FIRST RECORD navigation button, useful for building
     test screens quickly.  NOTE: if there are no tables in the query, then
     this code will not compile; so use the preprocessor to skip it. */
      DEFINE VARIABLE op-supported AS LOGICAL.
      GET FIRST {&FRAME-NAME}.
      IF AVAILABLE {&FIRST-TABLE-IN-QUERY-{&FRAME-NAME}} THEN DO:
          DISPLAY {&FIELDS-IN-QUERY-{&FRAME-NAME}} WITH FRAME {&FRAME-NAME}.
          {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
      END.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME BtnLast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnLast Customer-Win
ON CHOOSE OF BtnLast IN FRAME DEFAULT-FRAME /* Last */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN notify IN THIS-PROCEDURE ("get-last") NO-ERROR.
    &ELSE
      PUBLISH "fetchLast":U.
    &ENDIF
  &ELSEIF "{&TABLES-IN-QUERY-{&FRAME-NAME}}" ne "" &THEN
  /* This is a simple LAST RECORD navigation button, useful for building
     test screens quickly.  NOTE: if there are no tables in the query, then
     this code will not compile; so use the preprocessor to skip it. */
      GET LAST {&FRAME-NAME}.
      IF AVAILABLE {&FIRST-TABLE-IN-QUERY-{&FRAME-NAME}} THEN DO:
   DISPLAY {&FIELDS-IN-QUERY-{&FRAME-NAME}} WITH FRAME {&FRAME-NAME}.
   {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
      END.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME BtnNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnNext Customer-Win
ON CHOOSE OF BtnNext IN FRAME DEFAULT-FRAME /* Next */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN notify IN THIS-PROCEDURE ("get-next") NO-ERROR.
    &ELSE
      PUBLISH "fetchNext":U.
    &ENDIF
  &ELSEIF "{&TABLES-IN-QUERY-{&FRAME-NAME}}" ne "" &THEN
  /* This is a simple NEXT RECORD navigation button, useful for building
     test screens quickly.  NOTE: if there are no tables in the query, then
     this code will not compile; so use the preprocessor to skip it. */
      GET NEXT {&FRAME-NAME}.
      IF NOT AVAILABLE {&FIRST-TABLE-IN-QUERY-{&FRAME-NAME}}
          THEN GET LAST {&FRAME-NAME}.
      IF AVAILABLE {&FIRST-TABLE-IN-QUERY-{&FRAME-NAME}} THEN DO:
          DISPLAY {&FIELDS-IN-QUERY-{&FRAME-NAME}} WITH FRAME {&FRAME-NAME}.
   {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
      END.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME BtnPrev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnPrev Customer-Win
ON CHOOSE OF BtnPrev IN FRAME DEFAULT-FRAME /* Prev */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN notify IN THIS-PROCEDURE ("get-prev") NO-ERROR.
    &ELSE
      PUBLISH "fetchPrev":U.
    &ENDIF
  &ELSEIF "{&TABLES-IN-QUERY-{&FRAME-NAME}}" ne "" &THEN
  /* This is a simple PREV RECORD navigation button, useful for building
     test screens quickly.  NOTE: if there are no tables in the query, then
     this code will not compile; so use the preprocessor to skip it. */
      GET PREV {&FRAME-NAME}.
      IF NOT AVAILABLE {&FIRST-TABLE-IN-QUERY-{&FRAME-NAME}}
      THEN GET FIRST {&FRAME-NAME}.
      IF AVAILABLE {&FIRST-TABLE-IN-QUERY-{&FRAME-NAME}} THEN DO:
 DISPLAY {&FIELDS-IN-QUERY-{&FRAME-NAME}} WITH FRAME {&FRAME-NAME}.
        {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
      END.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Customer-Win 


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Customer-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(Customer-Win)
  THEN DELETE WIDGET Customer-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Customer-Win  _DEFAULT-ENABLE
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

  {&OPEN-QUERY-DEFAULT-FRAME}
  GET FIRST DEFAULT-FRAME.
  IF AVAILABLE Customer THEN 
    DISPLAY Customer.Cust-Num Customer.Name Customer.City Customer.Postal-Code 
          Customer.Address Customer.State Customer.Address2 Customer.Phone 
          Customer.Contact Customer.Country Customer.Credit-Limit 
          Customer.Balance Customer.Terms Customer.Discount Customer.Comments 
      WITH FRAME DEFAULT-FRAME IN WINDOW Customer-Win.
  ENABLE RECT-1 RECT-2 RECT-3 Customer.Cust-Num Customer.Name Customer.City 
         Customer.Postal-Code Customer.Address Customer.State Customer.Address2 
         Customer.Phone Customer.Contact Customer.Country Customer.Credit-Limit 
         Customer.Balance Customer.Terms Customer.Discount BtnFirst 
         Customer.Comments BtnNext BtnPrev BUTTON-1 BUTTON-3 BtnCancel BtnDone 
         BtnLast BUTTON-2 BtnOK 
      WITH FRAME DEFAULT-FRAME IN WINDOW Customer-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW Customer-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

