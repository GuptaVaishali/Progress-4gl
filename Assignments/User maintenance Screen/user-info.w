&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports2021       PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TTNewUser NO-UNDO LIKE NewUser.



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
  DEF VAR isAdd AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME UserBrowse

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TTNewUser

/* Definitions for BROWSE UserBrowse                                    */
&Scoped-define FIELDS-IN-QUERY-UserBrowse TTNewUser.User_Id TTNewUser.Name ~
TTNewUser.UserGroup TTNewUser.Password TTNewUser.City TTNewUser.State ~
TTNewUser.Zip-Code TTNewUser.Address TTNewUser.EmailAddress ~
TTNewUser.CreatedDateTime TTNewUser.LastUpdatedDateTime ~
TTNewUser.SecretQuestion TTNewUser.SecretAnswer 
&Scoped-define ENABLED-FIELDS-IN-QUERY-UserBrowse 
&Scoped-define QUERY-STRING-UserBrowse FOR EACH TTNewUser NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-UserBrowse OPEN QUERY UserBrowse FOR EACH TTNewUser NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-UserBrowse TTNewUser
&Scoped-define FIRST-TABLE-IN-QUERY-UserBrowse TTNewUser


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-UserBrowse}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 UserBrowse FPassword FZipCode ~
FCreatedDateTime FSecretQuestion FName FCity FAddress FLastUpdatedDateTime ~
FSecretAnswer FUserGroup FState FEmail BtnAdd BtnDelete BtnExit 
&Scoped-Define DISPLAYED-OBJECTS FUserId FPassword FZipCode ~
FCreatedDateTime FSecretQuestion FName FCity FAddress FLastUpdatedDateTime ~
FSecretAnswer FUserGroup FState FEmail 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnAdd 
     LABEL "ADD" 
     SIZE 15 BY 1.14 TOOLTIP "Adds New Record"
     BGCOLOR 1 .

DEFINE BUTTON BtnCancel DEFAULT 
     LABEL "Cancel" 
     SIZE 15 BY 1.14 TOOLTIP "Cancels the Update"
     BGCOLOR 1 .

DEFINE BUTTON BtnDelete 
     LABEL "DELETE" 
     SIZE 15 BY 1.14 TOOLTIP "Deletes The Selected Record"
     BGCOLOR 1 FGCOLOR 7 .

DEFINE BUTTON BtnExit DEFAULT 
     LABEL "&EXIT" 
     SIZE 15 BY 1.14 TOOLTIP "Exits the window"
     BGCOLOR 1 FGCOLOR 7 .

DEFINE BUTTON BtnSave 
     LABEL "SAVE" 
     SIZE 15 BY 1.14 TOOLTIP "Saves Record In DB"
     BGCOLOR 1 .

DEFINE VARIABLE FUserGroup AS CHARACTER FORMAT "X(10)":U 
     LABEL "UserGroup" 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "Admin","General","Super" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FAddress AS CHARACTER FORMAT "X(35)":U 
     LABEL "Address" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE FCity AS CHARACTER FORMAT "X(25)":U 
     LABEL "City" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FCreatedDateTime LIKE TTNewUser.CreatedDateTime
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE FEmail AS CHARACTER FORMAT "X(50)":U 
     LABEL "Email" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE FLastUpdatedDateTime AS DATETIME FORMAT "99/99/99 HH:MM:SS":U 
     LABEL "Last Updated Date Time" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE FName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Name" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE FPassword AS CHARACTER FORMAT "X(20)":U 
     LABEL "Password" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FSecretAnswer AS CHARACTER FORMAT "X(20)":U 
     LABEL "Secret Answer" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE FSecretQuestion AS CHARACTER FORMAT "X(50)":U 
     LABEL "Secret Question" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE FState AS CHARACTER FORMAT "X(20)":U 
     LABEL "State" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FUserId LIKE TTNewUser.User_Id
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE FZipCode LIKE TTNewUser.Zip-Code
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 229 BY 7.62
     BGCOLOR 5 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 113 BY 2.14
     BGCOLOR 0 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY UserBrowse FOR 
      TTNewUser SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE UserBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS UserBrowse C-Win _STRUCTURED
  QUERY UserBrowse NO-LOCK DISPLAY
      TTNewUser.User_Id FORMAT "x(20)":U WIDTH 14.2
      TTNewUser.Name FORMAT "x(30)":U WIDTH 16.2
      TTNewUser.UserGroup FORMAT "x(10)":U
      TTNewUser.Password FORMAT "x(20)":U WIDTH 18.6
      TTNewUser.City FORMAT "x(25)":U WIDTH 18.2
      TTNewUser.State FORMAT "x(20)":U WIDTH 16.2
      TTNewUser.Zip-Code FORMAT "x(6)":U
      TTNewUser.Address FORMAT "x(35)":U WIDTH 19.8
      TTNewUser.EmailAddress FORMAT "x(50)":U WIDTH 24.2
      TTNewUser.CreatedDateTime FORMAT "99/99/9999 HH:MM:SS.SSS":U
            WIDTH 27.2
      TTNewUser.LastUpdatedDateTime FORMAT "99/99/9999 HH:MM:SS.SSS":U
            WIDTH 29.2
      TTNewUser.SecretQuestion FORMAT "x(50)":U WIDTH 23.2
      TTNewUser.SecretAnswer FORMAT "x(20)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 260 BY 9.05
         TITLE "USERS INFORMATION" ROW-HEIGHT-CHARS .86 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     UserBrowse AT ROW 4.81 COL 6 WIDGET-ID 200
     FUserId AT ROW 17.19 COL 32 COLON-ALIGNED HELP
          "" WIDGET-ID 6
     FPassword AT ROW 17.19 COL 55.4 WIDGET-ID 12
     FZipCode AT ROW 17.19 COL 93.2 HELP
          "" WIDGET-ID 18
     FCreatedDateTime AT ROW 17.19 COL 138.2 HELP
          "" WIDGET-ID 24
     FSecretQuestion AT ROW 17.19 COL 207 COLON-ALIGNED WIDGET-ID 28
     FName AT ROW 19.1 COL 32 COLON-ALIGNED WIDGET-ID 8
     FCity AT ROW 19.1 COL 61.4 WIDGET-ID 14
     FAddress AT ROW 19.1 COL 94.2 WIDGET-ID 20
     FLastUpdatedDateTime AT ROW 19.1 COL 131.6 WIDGET-ID 26
     FSecretAnswer AT ROW 19.1 COL 207 COLON-ALIGNED WIDGET-ID 30
     FUserGroup AT ROW 21 COL 33 COLON-ALIGNED WIDGET-ID 50
     FState AT ROW 21 COL 59.8 WIDGET-ID 16
     FEmail AT ROW 21 COL 101 COLON-ALIGNED WIDGET-ID 22
     BtnAdd AT ROW 25.76 COL 71 WIDGET-ID 36
     BtnSave AT ROW 25.76 COL 91 WIDGET-ID 40
     BtnCancel AT ROW 25.76 COL 111 WIDGET-ID 34
     BtnDelete AT ROW 25.76 COL 131 WIDGET-ID 42
     BtnExit AT ROW 25.76 COL 151 WIDGET-ID 32
     "User-Info" VIEW-AS TEXT
          SIZE 10 BY 1.67 AT ROW 15.29 COL 19 WIDGET-ID 48
     " USER MAINTENANCE" VIEW-AS TEXT
          SIZE 24 BY 1.91 AT ROW 1.95 COL 122 WIDGET-ID 2
          BGCOLOR 0 FGCOLOR 15 
     RECT-1 AT ROW 16 COL 16 WIDGET-ID 4
     RECT-2 AT ROW 25.29 COL 61 WIDGET-ID 44
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 273.2 BY 28.62
         DEFAULT-BUTTON BtnExit CANCEL-BUTTON BtnCancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: TTNewUser T "?" NO-UNDO Sports2021 NewUser
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "UserMaintenanceScreen"
         HEIGHT             = 28.62
         WIDTH              = 273.2
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
/* BROWSE-TAB UserBrowse RECT-2 DEFAULT-FRAME */
/* SETTINGS FOR BUTTON BtnCancel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BtnSave IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FAddress IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FCity IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FCreatedDateTime IN FRAME DEFAULT-FRAME
   ALIGN-L LIKE = Temp-Tables.TTNewUser.CreatedDateTime EXP-SIZE        */
ASSIGN 
       FCreatedDateTime:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FLastUpdatedDateTime IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN 
       FLastUpdatedDateTime:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FPassword IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FState IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FUserId IN FRAME DEFAULT-FRAME
   NO-ENABLE LIKE = Temp-Tables.TTNewUser.User_Id EXP-SIZE              */
/* SETTINGS FOR FILL-IN FZipCode IN FRAME DEFAULT-FRAME
   ALIGN-L LIKE = Temp-Tables.TTNewUser.Zip-Code                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE UserBrowse
/* Query rebuild information for BROWSE UserBrowse
     _TblList          = "Temp-Tables.TTNewUser"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.TTNewUser.User_Id
"TTNewUser.User_Id" ? ? "character" ? ? ? ? ? ? no ? no no "14.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.TTNewUser.Name
"TTNewUser.Name" ? ? "character" ? ? ? ? ? ? no ? no no "16.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = Temp-Tables.TTNewUser.UserGroup
     _FldNameList[4]   > Temp-Tables.TTNewUser.Password
"TTNewUser.Password" ? ? "character" ? ? ? ? ? ? no ? no no "18.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.TTNewUser.City
"TTNewUser.City" ? ? "character" ? ? ? ? ? ? no ? no no "18.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.TTNewUser.State
"TTNewUser.State" ? ? "character" ? ? ? ? ? ? no ? no no "16.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   = Temp-Tables.TTNewUser.Zip-Code
     _FldNameList[8]   > Temp-Tables.TTNewUser.Address
"TTNewUser.Address" ? ? "character" ? ? ? ? ? ? no ? no no "19.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.TTNewUser.EmailAddress
"TTNewUser.EmailAddress" ? ? "character" ? ? ? ? ? ? no ? no no "24.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.TTNewUser.CreatedDateTime
"TTNewUser.CreatedDateTime" ? ? "datetime" ? ? ? ? ? ? no ? no no "27.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > Temp-Tables.TTNewUser.LastUpdatedDateTime
"TTNewUser.LastUpdatedDateTime" ? ? "datetime" ? ? ? ? ? ? no ? no no "29.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > Temp-Tables.TTNewUser.SecretQuestion
"TTNewUser.SecretQuestion" ? ? "character" ? ? ? ? ? ? no ? no no "23.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   = Temp-Tables.TTNewUser.SecretAnswer
     _Query            is OPENED
*/  /* BROWSE UserBrowse */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* UserMaintenanceScreen */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* UserMaintenanceScreen */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnAdd C-Win
ON CHOOSE OF BtnAdd IN FRAME DEFAULT-FRAME /* ADD */
DO:
    /****Locking browse on choose of Add button****/ 
    DEFINE VARIABLE hBrowse AS HANDLE NO-UNDO.
    ASSIGN hBrowse = BROWSE  UserBrowse:HANDLE.
    hBrowse:SENSITIVE = FALSE.
    
    /**clearing all fill-in-fields**/
    RUN ClearFields.
    
    /**Applying Focus on First Fill-in-1*****/
    APPLY "ENTRY" TO FUserId.

 
   /**Enable Save and Cancel button***/
    BtnSave:SENSITIVE = TRUE.
    BtnCancel:SENSITIVE = TRUE.
    BtnDelete:SENSITIVE = FALSE.  
    
    isAdd = TRUE.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCancel C-Win
ON CHOOSE OF BtnCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
   UserBrowse:SENSITIVE = TRUE. 
   FuserId:SENSITIVE = FALSE.
   OPEN QUERY UserBrowse FOR EACH TTNewUser NO-LOCK INDEXED-REPOSITION.
   APPLY "VALUE-CHANGED" TO UserBrowse.
   btnSave:SENSITIVE = FALSE.
   BtnDelete:SENSITIVE = TRUE.
   btnCancel:SENSITIVE = FALSE.
   isAdd = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDelete C-Win
ON CHOOSE OF BtnDelete IN FRAME DEFAULT-FRAME /* DELETE */
DO:
  MESSAGE "Are You Sure You Want To Delete the Selected Record ?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE choice AS logical.
  IF choice THEN
  DO:
      FIND FIRST NewUser WHERE NewUser.user_id = FUserId:SCREEN-VALUE EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF NOT AVAILABLE NewUser THEN
      DO:
          IF LOCKED NewUser THEN
             MESSAGE "This record is locked..can not be deleted" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
          ELSE
            MESSAGE "Record not available" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
      END. 
      ELSE
      DO:
          DELETE NewUser.
          DELETE TTNewUser.
          MESSAGE "Record Deleted Successfully"  VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
      END.
      OPEN QUERY UserBrowse FOR EACH TTNewUser NO-LOCK INDEXED-REPOSITION.
      APPLY "value-changed" TO UserBrowse.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnExit C-Win
ON CHOOSE OF BtnExit IN FRAME DEFAULT-FRAME /* EXIT */
DO:
    /*******Check if some updates are paending******************/
    IF btnSave:SENSITIVE = TRUE AND btnCancel:SENSITIVE = TRUE THEN
    DO:
         MESSAGE "There is Some Update Pending. Do You Still Want to Exit?"         
             VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE choice AS LOGICAL.
         IF choice THEN
         DO:
            APPLY "CLOSE":U TO THIS-PROCEDURE.
         END.
    END.
    ELSE
          APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnSave C-Win
ON CHOOSE OF BtnSave IN FRAME DEFAULT-FRAME /* SAVE */
DO:   
    /*******validating All Fields************/
    DEF VAR valid AS LOGICAL NO-UNDO INITIAL TRUE.
    RUN checkValidation(INPUT-OUTPUT valid).
    
    /*******Saving record in temp table and db**********/
    IF valid THEN
    DO:
        IF isAdd THEN
        DO:
            create TTNewUser.
            Assign TTNewUser.user_id = FUserId:SCREEN-VALUE
               TTNewUser.name = FName:SCREEN-VALUE
               TTNewUser.userGroup = FUserGroup:SCREEN-VALUE
               TTNewUser.password = FPassword:SCREEN-VALUE
               TTNewUser.city = FCity:SCREEN-VALUE
               TTNewUser.state = FState:SCREEN-VALUE
               TTNewUser.zip-code = FZipCode:SCREEN-VALUE
               TTNewUser.address = FAddress:SCREEN-VALUE
               TTNewUser.email = FEmail:SCREEN-VALUE
               TTNewUser.SecretQuestion = FSecretQuestion:SCREEN-VALUE
               TTNewUser.SecretAnswer = FSecretAnswer:SCREEN-VALUE.
               TTNewUser.createdDateTime = NOW.   
               TTNewUser.lastUpdatedDateTime = NOW.        
         
            CREATE NewUser.
            BUFFER-COPY TTNewUser TO NewUser.
            MESSAGE "Record Addded Successfully" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK. 
                
            isAdd = FALSE.
        END.
    
        ELSE
        DO:
                FIND FIRST newUser WHERE newuser.user_id = FUserId:SCREEN-VALUE EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                IF NOT AVAILABLE newUser THEN
                     IF LOCKED newUser THEN
                        DISP "NewUser locked by another user". 
                     ELSE
                        DISP "NewUser not found".
                ELSE
                DO:
                    ASSIGN NewUser.name = FName:SCREEN-VALUE
                       NewUser.userGroup = FUserGroup:SCREEN-VALUE
                       NewUser.password = FPassword:SCREEN-VALUE
                       NewUser.city = FCity:SCREEN-VALUE
                       NewUser.state = FState:SCREEN-VALUE
                       NewUser.zip-code = FZipCode:SCREEN-VALUE
                       NewUser.address = FAddress:SCREEN-VALUE
                       NewUser.email = FEmail:SCREEN-VALUE
                       NewUser.SecretQuestion = FSecretQuestion:SCREEN-VALUE
                       NewUser.SecretAnswer = FSecretAnswer:SCREEN-VALUE.   
                       NewUser.lastUpdatedDateTime = NOW.  
                        
                       BUFFER-COPY NewUser EXCEPT NewUser.user_id  TO TTNewUser.
                       MESSAGE "Record updated Successfully" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    
                END.
        END.
            
        OPEN QUERY UserBrowse FOR EACH TTNewUser NO-LOCK INDEXED-REPOSITION. 
        UserBrowse:SENSITIVE = TRUE. 
        /****Enable delete button*********/
        BtnDelete:SENSITIVE = TRUE.
        
       /***Disable save and cancel button***/
       btnCancel:SENSITIVE = FALSE.
       btnSave:SENSITIVE = FALSE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FAddress
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FAddress C-Win
ON VALUE-CHANGED OF FAddress IN FRAME DEFAULT-FRAME /* Address */
DO:
  IF NOT isAdd THEN
  DO:
      UserBrowse:SENSITIVE = FALSE. 
      btnCancel:SENSITIVE = TRUE.
      btnSave:SENSITIVE = TRUE.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FCity
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FCity C-Win
ON VALUE-CHANGED OF FCity IN FRAME DEFAULT-FRAME /* City */
DO:
  IF NOT isAdd THEN
  DO:
        UserBrowse:SENSITIVE = FALSE. 
        btnCancel:SENSITIVE = TRUE.
        btnSave:SENSITIVE = TRUE.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FEmail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FEmail C-Win
ON VALUE-CHANGED OF FEmail IN FRAME DEFAULT-FRAME /* Email */
DO:
   IF NOT isAdd THEN
    DO:
        UserBrowse:SENSITIVE = FALSE. 
        btnCancel:SENSITIVE = TRUE.
        btnSave:SENSITIVE = TRUE.
    END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FName C-Win
ON VALUE-CHANGED OF FName IN FRAME DEFAULT-FRAME /* Name */
DO:
   IF NOT isAdd THEN
    DO:
        UserBrowse:SENSITIVE = FALSE. 
        btnCancel:SENSITIVE = TRUE.
        btnSave:SENSITIVE = TRUE.
    END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FPassword
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FPassword C-Win
ON VALUE-CHANGED OF FPassword IN FRAME DEFAULT-FRAME /* Password */
DO:
    IF NOT isAdd THEN
    DO:
        UserBrowse:SENSITIVE = FALSE. 
        btnCancel:SENSITIVE = TRUE.
        btnSave:SENSITIVE = TRUE.
    END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FSecretAnswer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FSecretAnswer C-Win
ON VALUE-CHANGED OF FSecretAnswer IN FRAME DEFAULT-FRAME /* Secret Answer */
DO:
   IF NOT isAdd THEN
    DO:
        UserBrowse:SENSITIVE = FALSE.  
        btnCancel:SENSITIVE = TRUE.
        btnSave:SENSITIVE = TRUE.
    END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FSecretQuestion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FSecretQuestion C-Win
ON VALUE-CHANGED OF FSecretQuestion IN FRAME DEFAULT-FRAME /* Secret Question */
DO:
  IF NOT isAdd THEN
  DO:
    UserBrowse:SENSITIVE = FALSE.
    btnCancel:SENSITIVE = TRUE.
    btnSave:SENSITIVE = TRUE.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FState
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FState C-Win
ON VALUE-CHANGED OF FState IN FRAME DEFAULT-FRAME /* State */
DO:
   IF NOT isAdd THEN
    DO:
        UserBrowse:SENSITIVE = FALSE. 
        btnCancel:SENSITIVE = TRUE.
        btnSave:SENSITIVE = TRUE.
    END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FUserGroup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FUserGroup C-Win
ON VALUE-CHANGED OF FUserGroup IN FRAME DEFAULT-FRAME /* UserGroup */
DO:
    IF NOT isAdd THEN
    DO:
       UserBrowse:SENSITIVE = FALSE. 
       btnCancel:SENSITIVE = TRUE.
       btnSave:SENSITIVE = TRUE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FZipCode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FZipCode C-Win
ON VALUE-CHANGED OF FZipCode IN FRAME DEFAULT-FRAME /* Zip-Code */
DO:
    IF NOT isAdd THEN
    DO:
        UserBrowse:SENSITIVE = FALSE.
         btnCancel:SENSITIVE = TRUE.
         btnSave:SENSITIVE = TRUE.
    END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME UserBrowse
&Scoped-define SELF-NAME UserBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL UserBrowse C-Win
ON VALUE-CHANGED OF UserBrowse IN FRAME DEFAULT-FRAME /* USERS INFORMATION */
DO:
    ASSIGN FUserId:SCREEN-VALUE = String(TTNewUser.USER_ID)
           FName:SCREEN-VALUE = TTNewUser.NAME
           //FILL-IN-3:SCREEN-VALUE = TTNewUser.UserGroup
           FUserGroup:SCREEN-VALUE = TTNewUser.UserGroup
           
           FPassword:SCREEN-VALUE = TTNewUser.Password
           FCity:SCREEN-VALUE = TTNewUser.City
           FState:SCREEN-VALUE = TTNewUser.State
           FZipCode:SCREEN-VALUE = TTNewUser.Zip-Code
           FAddress:SCREEN-VALUE = TTNewUser.Address
           FEmail:SCREEN-VALUE = TTNewUser.Email
           FCreatedDateTime:SCREEN-VALUE = String(TTNewUser.CreatedDateTime)
           FLastUpdatedDateTime:SCREEN-VALUE = STRING(TTNewUser.LastUpdatedDateTime)
           FSecretQuestion:SCREEN-VALUE = TTNewUser.SecretQUESTION
           FSecretAnswer:SCREEN-VALUE = TTNewUser.SecretAnswer.
      
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


/*********Receiving parameters userid and usergroup to view screen in view mode or update mode**/  
DEFINE INPUT PARAMETER puser_id    LIKE NewUser.user_id   NO-UNDO.
DEFINE INPUT PARAMETER puser_group LIKE NewUser.USERGROUP NO-UNDO.

IF puser_group NE "Admin" THEN
    ASSIGN
       BtnAdd:SENSITIVE = FALSE
       BtnDelete:SENSITIVE = FALSE
     
       FUserId:SENSITIVE = FALSE
       FName:SENSITIVE = FALSE
       FUserGroup:SENSITIVE = FALSE
       FPassword:SENSITIVE = FALSE
       FCity:SENSITIVE = FALSE
       FState:SENSITIVE = FALSE
       FZipCode:SENSITIVE = FALSE
       FAddress:SENSITIVE = FALSE
       FEmail:SENSITIVE = FALSE
       FCreatedDateTime:SENSITIVE = FALSE
       FLastUpdatedDateTime:SENSITIVE = FALSE
       FSecretQuestion:SENSITIVE = FALSE
       FSecretAnswer:SENSITIVE = FALSE.     

  
//copying data from databse to temp-table.
FOR EACH NewUser.
    CREATE TTNewUser.
    BUFFER-COPY NewUser TO TTNewUser.
END. 

//In main writing query to show data of temp table in browser-2.
 OPEN QUERY UserBrowse FOR EACH TTNewUser NO-LOCK INDEXED-REPOSITION. 
 
 APPLY "VALUE-CHANGED" TO UserBrowse.
 
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckValidation C-Win 
PROCEDURE CheckValidation :
/*------------------------------------------------------------------------------
  Purpose: Validating all the fill-in fields before saving to db.    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT-OUTPUT PARAMETER valid AS LOGICAL NO-UNDO.
IF isAdd THEN
DO:
IF FUserId:SCREEN-VALUE IN FRAME {&frame-name} = "" THEN
    DO:
        MESSAGE "Please Enter User Id"
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        APPLY "Entry" TO FUserId.
        valid = FALSE.
    END.
    ELSE IF LENGTH(FUserId:SCREEN-VALUE,"char") < 5 THEN
    DO:
            MESSAGE "User Id must be greater than 4 characters"
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            APPLY "Entry" TO FUserId.
            valid = FALSE.
    END.
    ELSE
    DO:
            FIND FIRST TTNewUser WHERE TTNewUser.user_id = FUserId:SCREEN-VALUE NO-ERROR.
            IF AVAILABLE TTNewUser THEN
            DO:
                MESSAGE "User Id Already Exists"
                    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
                APPLY "Entry" TO FUserId.
                valid = FALSE.
            END.
     END.
END.
    IF FName:SCREEN-VALUE = "" THEN
    DO:
        MESSAGE "Please Enter User Name"
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        APPLY "Entry" TO FName.
        valid = FALSE.
    END.
 /*   IF FILL-IN-3:SCREEN-VALUE = "" THEN
    DO:
        MESSAGE "Please Enter User Group"
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        APPLY "Entry" TO FILL-IN-3.
    END.   */
    IF FPassword:SCREEN-VALUE = "" THEN
    DO:
        MESSAGE "Please Enter Password"
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        APPLY "Entry" TO FPassword.
        valid = FALSE.
    END.
    IF FCity:SCREEN-VALUE = "" THEN
    DO:
        MESSAGE "Please Enter City"
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        APPLY "Entry" TO FCity.
        valid = FALSE.
    END.
    
    IF FState:SCREEN-VALUE = "" THEN
    DO:
        MESSAGE "Please Enter State"
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        APPLY "Entry" TO FState.
        valid = FALSE.
    END.
    IF FZipCode:SCREEN-VALUE = "" THEN
    DO:
        MESSAGE "Please Enter Zip-Code"
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        APPLY "Entry" TO FZipCode.
        valid = FALSE.
    END.
    IF FAddress:SCREEN-VALUE = "" THEN
    DO:
        MESSAGE "Please Enter Address"
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        APPLY "Entry" TO FAddress.
        valid = FALSE.
    END.
    IF FEmail:SCREEN-VALUE = "" THEN
    DO:
        MESSAGE "Please Enter Email"
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        APPLY "Entry" TO FEmail.
        valid = FALSE.
    END.
    IF FSecretQuestion:SCREEN-VALUE = "" THEN
    DO:
        MESSAGE "Please Enter Secret Question"
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        APPLY "Entry" TO FSecretQuestion.
        valid = FALSE.
    END.
    IF FSecretAnswer:SCREEN-VALUE = "" THEN
    DO:
        MESSAGE "Please Enter Secret Answer"
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        APPLY "Entry" TO FSecretAnswer.
        valid = FALSE.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ClearFields C-Win 
PROCEDURE ClearFields :
/*------------------------------------------------------------------------------
  Purpose: Clearing All the fill in fields.   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 FUserId:SENSITIVE IN FRAME {&frame-name} = TRUE.
   FUserId:CLEAR().
   FName:CLEAR().
   FPassword:CLEAR().
   FCity:CLEAR().
   FState:CLEAR().
   FZipCode:CLEAR().
   FAddress:CLEAR().
   FEmail:CLEAR().
   FCreatedDateTime:CLEAR().
   FLastUpdatedDateTime:CLEAR().
   FSecretQuestion:CLEAR().
   FSecretAnswer:CLEAR().

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
  DISPLAY FUserId FPassword FZipCode FCreatedDateTime FSecretQuestion FName 
          FCity FAddress FLastUpdatedDateTime FSecretAnswer FUserGroup FState 
          FEmail 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 RECT-2 UserBrowse FPassword FZipCode FCreatedDateTime 
         FSecretQuestion FName FCity FAddress FLastUpdatedDateTime 
         FSecretAnswer FUserGroup FState FEmail BtnAdd BtnDelete BtnExit 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

