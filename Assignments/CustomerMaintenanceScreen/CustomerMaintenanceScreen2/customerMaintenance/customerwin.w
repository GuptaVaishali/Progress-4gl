&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports           PROGRESS
*/
&Scoped-define WINDOW-NAME CustWin


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TTCustomer NO-UNDO LIKE Customer.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS CustWin 
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
&Scoped-define BROWSE-NAME brCust

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TTCustomer

/* Definitions for BROWSE brCust                                        */
&Scoped-define FIELDS-IN-QUERY-brCust TTCustomer.Cust-Num TTCustomer.Name ~
TTCustomer.City TTCustomer.State TTCustomer.Country TTCustomer.Address ~
TTCustomer.Address2 TTCustomer.Phone TTCustomer.Postal-Code ~
TTCustomer.Balance TTCustomer.Contact TTCustomer.Discount ~
TTCustomer.Sales-Rep TTCustomer.Terms TTCustomer.Credit-Limit ~
TTCustomer.Comments 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brCust 
&Scoped-define QUERY-STRING-brCust FOR EACH TTCustomer NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brCust OPEN QUERY brCust FOR EACH TTCustomer NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brCust TTCustomer
&Scoped-define FIRST-TABLE-IN-QUERY-brCust TTCustomer


/* Definitions for FRAME search-frame                                   */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnRadio btnSubmit btnExit 
&Scoped-Define DISPLAYED-OBJECTS btnRadio 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD checkInitialDigit CustWin 
FUNCTION checkInitialDigit RETURNS LOGICAL
  ( INPUT pStr AS CHARACTER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD checkInteger CustWin 
FUNCTION checkInteger RETURNS LOGICAL
  ( INPUT pStr AS CHARACTER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD isIntegerVal CustWin 
FUNCTION isIntegerVal RETURNS LOGICAL
  ( INPUT pStr AS CHARACTER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR CustWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnExit 
     LABEL "Exit" 
     SIZE 15 BY 1.14
     BGCOLOR 9 FGCOLOR 9 .

DEFINE BUTTON btnSubmit 
     LABEL "Submit" 
     SIZE 15 BY 1.14
     BGCOLOR 9 .

DEFINE VARIABLE btnRadio AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Search", "search",
"New", "new"
     SIZE 18 BY 2.86 NO-UNDO.

DEFINE BUTTON btnBack 
     LABEL "Back" 
     SIZE 15 BY 1.14
     BGCOLOR 9 FGCOLOR 9 .

DEFINE BUTTON btnSave 
     LABEL "Save" 
     SIZE 15 BY 1.14
     BGCOLOR 9 FGCOLOR 9 .

DEFINE VARIABLE FAddress AS CHARACTER FORMAT "X(256)":U 
     LABEL "Address" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FAddress2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Address2" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FBalance AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Balance" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FCity AS CHARACTER FORMAT "X(256)":U 
     LABEL "City" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FComments AS CHARACTER FORMAT "X(256)":U 
     LABEL "Comments" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FContact AS CHARACTER FORMAT "X(256)":U 
     LABEL "Contact" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FCountry AS CHARACTER FORMAT "X(256)":U INITIAL "U.S.A" 
     LABEL "Country" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FCreditLimit AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 1500 
     LABEL "CreditLimit" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FCustNum AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "custNum" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FDiscount AS INTEGER FORMAT ">>9%":U INITIAL 0 
     LABEL "Discount" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Name" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FPhone AS CHARACTER FORMAT "X(256)":U 
     LABEL "Phone" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FPostalCode AS CHARACTER FORMAT "X(256)":U 
     LABEL "PostalCode" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FSalesRep AS CHARACTER FORMAT "X(4)":U 
     LABEL "SalesRep" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FState AS CHARACTER FORMAT "X(256)":U 
     LABEL "State" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FTerms AS CHARACTER FORMAT "X(20)":U INITIAL "Net30" 
     LABEL "Terms" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE BUTTON btnReturn 
     LABEL "Return" 
     SIZE 15 BY 1.14
     BGCOLOR 9 .

DEFINE BUTTON btnSearch 
     LABEL "Search" 
     SIZE 15 BY 1.14
     BGCOLOR 9 FGCOLOR 9 .

DEFINE VARIABLE custNum AS CHARACTER FORMAT "X(256)":U 
     LABEL "custNum" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brCust FOR 
      TTCustomer SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brCust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brCust CustWin _STRUCTURED
  QUERY brCust NO-LOCK DISPLAY
      TTCustomer.Cust-Num FORMAT ">>>>9":U
      TTCustomer.Name FORMAT "x(20)":U WIDTH 17.2
      TTCustomer.City FORMAT "x(12)":U
      TTCustomer.State FORMAT "x(20)":U WIDTH 12.4
      TTCustomer.Country FORMAT "x(20)":U WIDTH 14.2
      TTCustomer.Address FORMAT "x(20)":U WIDTH 17.2
      TTCustomer.Address2 FORMAT "x(20)":U WIDTH 15.2
      TTCustomer.Phone FORMAT "x(20)":U WIDTH 17.2
      TTCustomer.Postal-Code FORMAT "x(10)":U
      TTCustomer.Balance FORMAT "->,>>>,>>9.99":U
      TTCustomer.Contact FORMAT "x(20)":U WIDTH 15
      TTCustomer.Discount FORMAT ">>9%":U
      TTCustomer.Sales-Rep FORMAT "X(4)":U
      TTCustomer.Terms FORMAT "x(20)":U WIDTH 8.4
      TTCustomer.Credit-Limit FORMAT "->,>>>,>>9.99":U WIDTH 12.2
      TTCustomer.Comments FORMAT "x(60)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 234 BY 8.1 ROW-HEIGHT-CHARS .86 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnRadio AT ROW 3.62 COL 132 NO-LABEL WIDGET-ID 2
     btnSubmit AT ROW 7.67 COL 121 WIDGET-ID 6
     btnExit AT ROW 7.67 COL 141 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 273.2 BY 33.57 WIDGET-ID 100.

DEFINE FRAME search-frame
     custNum AT ROW 2.19 COL 29 COLON-ALIGNED WIDGET-ID 2
     btnSearch AT ROW 2.19 COL 57 WIDGET-ID 4
     brCust AT ROW 4.33 COL 9 WIDGET-ID 300
     btnReturn AT ROW 13.86 COL 117 WIDGET-ID 6
     SPACE(50.00) SKIP(0.00)
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 6 ROW 9.57 SCROLLABLE  WIDGET-ID 200.

DEFINE FRAME new-frame
     FCustNum AT ROW 1.95 COL 19 COLON-ALIGNED WIDGET-ID 4
     FName AT ROW 1.95 COL 49 COLON-ALIGNED WIDGET-ID 8
     FCity AT ROW 1.95 COL 79 COLON-ALIGNED WIDGET-ID 10
     FState AT ROW 1.95 COL 109 COLON-ALIGNED WIDGET-ID 12
     FCountry AT ROW 3.86 COL 19 COLON-ALIGNED WIDGET-ID 6
     FAddress AT ROW 3.86 COL 49 COLON-ALIGNED WIDGET-ID 18
     FAddress2 AT ROW 3.86 COL 79 COLON-ALIGNED WIDGET-ID 20
     FPostalCode AT ROW 3.86 COL 109 COLON-ALIGNED WIDGET-ID 22
     FContact AT ROW 5.76 COL 19 COLON-ALIGNED WIDGET-ID 16
     FSalesRep AT ROW 5.76 COL 49 COLON-ALIGNED WIDGET-ID 30
     FPhone AT ROW 5.76 COL 79 COLON-ALIGNED WIDGET-ID 32
     FCreditLimit AT ROW 5.76 COL 109 COLON-ALIGNED WIDGET-ID 34
     FBalance AT ROW 7.67 COL 19 COLON-ALIGNED WIDGET-ID 24
     FTerms AT ROW 7.67 COL 49 COLON-ALIGNED WIDGET-ID 14
     FDiscount AT ROW 7.67 COL 79 COLON-ALIGNED WIDGET-ID 28
     FComments AT ROW 7.67 COL 109 COLON-ALIGNED WIDGET-ID 26
     btnSave AT ROW 10.29 COL 51 WIDGET-ID 38
     btnBack AT ROW 10.29 COL 71 WIDGET-ID 40
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 47 ROW 3.38
         SIZE 135 BY 11.43 WIDGET-ID 400.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: TTCustomer T "?" NO-UNDO sports Customer
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW CustWin ASSIGN
         HIDDEN             = YES
         TITLE              = "CustomerInfo"
         HEIGHT             = 33.57
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
/* SETTINGS FOR WINDOW CustWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME new-frame:FRAME = FRAME search-frame:HANDLE
       FRAME search-frame:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME search-frame:MOVE-AFTER-TAB-ITEM (btnExit:HANDLE IN FRAME DEFAULT-FRAME)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FRAME new-frame
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME new-frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FCustNum IN FRAME new-frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME search-frame
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN XXTABVALXX = FRAME new-frame:MOVE-AFTER-TAB-ITEM (btnSearch:HANDLE IN FRAME search-frame)
       XXTABVALXX = FRAME new-frame:MOVE-BEFORE-TAB-ITEM (brCust:HANDLE IN FRAME search-frame)
/* END-ASSIGN-TABS */.

/* BROWSE-TAB brCust new-frame search-frame */
ASSIGN 
       FRAME search-frame:SCROLLABLE       = FALSE
       FRAME search-frame:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(CustWin)
THEN CustWin:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brCust
/* Query rebuild information for BROWSE brCust
     _TblList          = "Temp-Tables.TTCustomer"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.TTCustomer.Cust-Num
     _FldNameList[2]   > Temp-Tables.TTCustomer.Name
"TTCustomer.Name" ? ? "character" ? ? ? ? ? ? no ? no no "17.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = Temp-Tables.TTCustomer.City
     _FldNameList[4]   > Temp-Tables.TTCustomer.State
"TTCustomer.State" ? ? "character" ? ? ? ? ? ? no ? no no "12.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.TTCustomer.Country
"TTCustomer.Country" ? ? "character" ? ? ? ? ? ? no ? no no "14.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.TTCustomer.Address
"TTCustomer.Address" ? ? "character" ? ? ? ? ? ? no ? no no "17.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.TTCustomer.Address2
"TTCustomer.Address2" ? ? "character" ? ? ? ? ? ? no ? no no "15.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.TTCustomer.Phone
"TTCustomer.Phone" ? ? "character" ? ? ? ? ? ? no ? no no "17.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   = Temp-Tables.TTCustomer.Postal-Code
     _FldNameList[10]   = Temp-Tables.TTCustomer.Balance
     _FldNameList[11]   > Temp-Tables.TTCustomer.Contact
"TTCustomer.Contact" ? ? "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   = Temp-Tables.TTCustomer.Discount
     _FldNameList[13]   = Temp-Tables.TTCustomer.Sales-Rep
     _FldNameList[14]   > Temp-Tables.TTCustomer.Terms
"TTCustomer.Terms" ? ? "character" ? ? ? ? ? ? no ? no no "8.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > Temp-Tables.TTCustomer.Credit-Limit
"TTCustomer.Credit-Limit" ? ? "decimal" ? ? ? ? ? ? no ? no no "12.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   = Temp-Tables.TTCustomer.Comments
     _Query            is NOT OPENED
*/  /* BROWSE brCust */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME CustWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CustWin CustWin
ON END-ERROR OF CustWin /* CustomerInfo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CustWin CustWin
ON WINDOW-CLOSE OF CustWin /* CustomerInfo */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME new-frame
&Scoped-define SELF-NAME btnBack
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBack CustWin
ON CHOOSE OF btnBack IN FRAME new-frame /* Back */
DO:
  HIDE FRAME new-frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExit CustWin
ON CHOOSE OF btnExit IN FRAME DEFAULT-FRAME /* Exit */
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


&Scoped-define FRAME-NAME search-frame
&Scoped-define SELF-NAME btnReturn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReturn CustWin
ON CHOOSE OF btnReturn IN FRAME search-frame /* Return */
DO:
  HIDE FRAME search-frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME new-frame
&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave CustWin
ON CHOOSE OF btnSave IN FRAME new-frame /* Save */
DO:
        DEFINE VARIABLE vlIsValid AS LOGICAL NO-UNDO INITIAL TRUE.
        RUN checkvalid(INPUT-OUTPUT vlIsvalid).
           
        IF vlIsValid THEN
        DO:     
            /*************CREATE CUSTOMER RECORD **************************/
            CREATE TTcustomer.
            
            /*********IF SALESREP VALUE DOES NOT EXIST IN SALESREP TABLE, THEN CREATE A RECORD IN SALESREP TABLE***************/
       /*     FIND FIRST salesrep WHERE salesRep.sales-rep = FSalesRep:SCREEN-VALUE NO-LOCK NO-ERROR.
            IF NOT AVAILABLE salesrep THEN
            DO:
                CREATE salesrep.
                salesrep.sales-rep = FSalesRep:SCREEN-VALUE.
            END.  */
            
            RUN  D:\D&H_Project\customerMaintenancefinal\createSalesrep.p(INPUT FSalesRep:SCREEN-VALUE).
            
            ASSIGN TTcustomer.cust-num     = INTEGER (FCustNum:SCREEN-VALUE)
                   TTcustomer.country      = FCountry:SCREEN-VALUE
                   TTcustomer.NAME         = FName:SCREEN-VALUE
                   TTcustomer.address      = FAddress:SCREEN-VALUE
                   TTcustomer.address2     = FAddress2:SCREEN-VALUE
                   TTcustomer.city         = FCity:SCREEN-VALUE
                   TTcustomer.state        = FState:SCREEN-VALUE
                   TTcustomer.postal-code  = FPostalCode:SCREEN-VALUE
                   TTcustomer.contact      = FContact:SCREEN-VALUE
                   TTcustomer.phone        = FPhone:SCREEN-VALUE
                   TTcustomer.sales-rep    = FSalesRep:SCREEN-VALUE
                   TTcustomer.credit-limit = DECIMAL(FCreditLimit:SCREEN-VALUE)
                   TTcustomer.balance      = DECIMAL(FBalance:SCREEN-VALUE)
                   TTcustomer.terms        = FTerms:SCREEN-VALUE
                   TTcustomer.discount     = INTEGER( TRIM ( FDiscount:SCREEN-VALUE, "%" ) )
                   TTcustomer.comments     = FComments:SCREEN-VALUE.
                   
           DEFINE VARIABLE rwTTCust  AS ROWID NO-UNDO.
           rwTTCust = ROWID(TTCustomer).
           
           RUN D:\D&H_Project\customerMaintenancefinal\addCustomer.p(INPUT TABLE TTcustomer BY-REFERENCE , INPUT rwTTCust).
           
           MESSAGE "RECORD CREATED SUCCESSFULLY" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
           btnSave:SENSITIVE = FALSE.
       END.   
       
       
    /*   IF vlIsValid THEN
        DO:     
            /*************CREATE CUSTOMER RECORD **************************/
            CREATE customer.
            
            /*********IF SALESREP VALUE DOES NOT EXIST IN SALESREP TABLE, THEN CREATE A RECORD IN SALESREP TABLE***************/
            FIND FIRST salesrep WHERE salesRep.sales-rep = FSalesRep:SCREEN-VALUE NO-LOCK NO-ERROR.
            IF NOT AVAILABLE salesrep THEN
            DO:
                CREATE salesrep.
                salesrep.sales-rep = FSalesRep:SCREEN-VALUE.
            END.
            ASSIGN customer.cust-num     = INTEGER (FCustNum:SCREEN-VALUE)
                   customer.country      = FCountry:SCREEN-VALUE
                   customer.NAME         = FName:SCREEN-VALUE
                   customer.address      = FAddress:SCREEN-VALUE
                   customer.address2     = FAddress2:SCREEN-VALUE
                   customer.city         = FCity:SCREEN-VALUE
                   customer.state        = FState:SCREEN-VALUE
                   customer.postal-code  = FPostalCode:SCREEN-VALUE
                   customer.contact      = FContact:SCREEN-VALUE
                   customer.phone        = FPhone:SCREEN-VALUE
                   customer.sales-rep    = FSalesRep:SCREEN-VALUE
                   customer.credit-limit = DECIMAL(FCreditLimit:SCREEN-VALUE)
                   customer.balance      = DECIMAL(FBalance:SCREEN-VALUE)
                   customer.terms        = FTerms:SCREEN-VALUE
                   customer.discount     = INTEGER( TRIM ( FDiscount:SCREEN-VALUE, "%" ) )
                   customer.comments     = FComments:SCREEN-VALUE.
                   
           MESSAGE "RECORD CREATED SUCCESSFULLY" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
           btnSave:SENSITIVE = FALSE.
       END.   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME search-frame
&Scoped-define SELF-NAME btnSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSearch CustWin
ON CHOOSE OF btnSearch IN FRAME search-frame /* Search */
DO:
  FIND FIRST TTcustomer WHERE TTcustomer.cust-num = INTEGER (custNum:SCREEN-VALUE) NO-LOCK NO-ERROR.
  IF AVAILABLE TTcustomer THEN
  DO:
     OPEN QUERY brcust FOR EACH TTcustomer WHERE TTcustomer.cust-num = INTEGER (custNum:SCREEN-VALUE) NO-LOCK.
  END.
  ELSE
    MESSAGE "Customer not found" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnSubmit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSubmit CustWin
ON CHOOSE OF btnSubmit IN FRAME DEFAULT-FRAME /* Submit */
DO:
  IF btnRadio:SCREEN-VALUE = "search" THEN
  DO:
      VIEW FRAME search-frame.
  END.
  ELSE
  DO:
        MESSAGE "before hidden"
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    //  FRAME search-frame:HIDDEN = TRUE.
      MESSAGE "after hidden"
          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
      VIEW FRAME new-frame.
  /*   /********AUTO-GENERATED VALUE OF CUSTOMER*************************/
      FIND LAST customer NO-LOCK NO-ERROR.
      IF AVAILABLE customer THEN
          FCustNum:SCREEN-VALUE = STRING (customer.cust-num + 1).
      ELSE
          FCustNum:SCREEN-VALUE = STRING (1).      */
       
       DEFINE VARIABLE viCustNum AS INTEGER NO-UNDO.
       
       RUN  D:\D&H_Project\customerMaintenancefinal\lastCustNum.p(OUTPUT viCustNum).
     //  FRAME search-frame:HIDDEN = TRUE.
       RUN disable_ui.
       VIEW FRAME default-frame.
       brCust:HIDDEN = TRUE.
    //   custNum:HIDDEN = TRUE.
    //   btnSearch:HIDDEN = TRUE.
       MESSAGE "after run"
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       FCustNum:SCREEN-VALUE = STRING(viCustNum).
       MESSAGE "at last"
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brCust
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK CustWin 


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
  
 /* FOR EACH customer:
    CREATE TTcustomer.
    BUFFER-COPY customer TO TTcustomer.
  END.   */
  
  RUN D:\D&H_Project\customerMaintenancefinal\ttCustData.p(INPUT-OUTPUT TABLE TTcustomer BY-REFERENCE).
  
  OPEN QUERY brCust FOR EACH TTcustomer NO-LOCK INDEXED-REPOSITION.
  APPLY "VALUE-CHANGED" TO BROWSE brCust.
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkValid CustWin 
PROCEDURE checkValid :
/*------------------------------------------------------------------------------
  Purpose:  Validations applied on all fields   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT-OUTPUT PARAMETER vlIsValid AS  LOGICAL NO-UNDO.
    
    IF FName:SCREEN-VALUE IN FRAME new-frame = "" THEN
    DO:
       MESSAGE "PLEASE ENTER CUSTOMER NAME" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       APPLY "Entry" TO FName.
       vlIsValid = FALSE.
    END.
    
    ELSE IF checkInteger(INPUT FName:SCREEN-VALUE) = TRUE THEN
    DO:
        MESSAGE "PLEASE ENTER VALID CUSTOMER NAME" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        APPLY "Entry" TO FName.
        vlIsValid = FALSE.
    END.
        
    ELSE IF FAddress:SCREEN-VALUE = "" THEN
    DO:
       MESSAGE "PLEASE ENTER CUSTOMER ADDRESS" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       APPLY "Entry" TO FAddress.
       vlIsValid = FALSE.
    END.
    
    ELSE IF FCity:SCREEN-VALUE = "" THEN
    DO:
       MESSAGE "PLEASE ENTER CUSTOMER CITY" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       APPLY "Entry" TO FCity.
       vlIsValid = FALSE.
    END.   
    
    ELSE IF checkInteger(INPUT FCity:SCREEN-VALUE) = TRUE THEN
    DO:
        MESSAGE "PLEASE ENTER VALID CUSTOMER CITY" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        APPLY "Entry" TO FCity.
        vlIsValid = FALSE.
    END.
    
    ELSE IF FState:SCREEN-VALUE = "" THEN
    DO:
       MESSAGE "PLEASE ENTER CUSTOMER STATE" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       APPLY "Entry" TO FState.
       vlIsValid = FALSE.
    END.
    
    ELSE IF checkInteger(INPUT FState:SCREEN-VALUE) = TRUE THEN
    DO:
        MESSAGE "PLEASE ENTER VALID CUSTOMER STATE" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        APPLY "Entry" TO FState.
        vlIsValid = FALSE.
    END.
    
    ELSE IF FPostalCode:SCREEN-VALUE = "" THEN
    DO:
       MESSAGE "PLEASE ENTER CUSTOMER POSTALCODE" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       APPLY "Entry" TO FPostalCode. 
       vlIsValid = FALSE.
    END.
    ELSE IF LENGTH(FPostalCode:SCREEN-VALUE) <> 6 THEN
    DO:
       MESSAGE "PLEASE ENTER 6 DIGIT POSTALCODE" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       APPLY "Entry" TO FPostalCode. 
       vlIsValid = FALSE.
    END.

    ELSE IF isIntegerVal(FPostalCode:SCREEN-VALUE) = FALSE THEN
    DO:
       MESSAGE "PLEASE ENTER ONLY INTEGER VALUE OF POSTAL-CODE" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       APPLY "Entry" TO FPostalCode. 
       vlIsValid = FALSE.
    END.
    
    ELSE IF FContact:SCREEN-VALUE = "" THEN
    DO:
       MESSAGE "PLEASE ENTER CUSTOMER CONTACT" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       APPLY "Entry" TO FContact. 
       vlIsValid = FALSE.
    END.
    
    ELSE IF checkInteger(INPUT FContact:SCREEN-VALUE) = TRUE THEN
    DO:
        MESSAGE "PLEASE ENTER VALID CUSTOMER CONTACT" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        APPLY "Entry" TO FContact.
        vlIsValid = FALSE.
    END.
       
    ELSE IF FSalesRep:SCREEN-VALUE = "" THEN
    DO:
       MESSAGE "PLEASE ENTER CUSTOMER SALESREP" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       APPLY "Entry" TO FSalesRep. 
       vlIsValid = FALSE.
    END. 
    
    ELSE IF checkInteger(INPUT FSalesRep:SCREEN-VALUE) = TRUE THEN
    DO:
        MESSAGE "PLEASE ENTER VALID SALESREP VALUE" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        APPLY "Entry" TO FSalesRep.
        vlIsValid = FALSE.
    END.
    
    ELSE IF FPhone:SCREEN-VALUE = "" THEN
    DO:
       MESSAGE "PLEASE ENTER CUSTOMER PHONE" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       APPLY "Entry" TO FPhone. 
       vlIsValid = FALSE.
    END.
    
    ELSE IF LENGTH(FPhone:SCREEN-VALUE) <> 10 THEN
    DO:
       MESSAGE "PLEASE ENTER 10 DIGIT PHONE" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       APPLY "Entry" TO FPhone. 
       vlIsValid = FALSE.
    END.
    
    ELSE IF isIntegerVal(FPhone:SCREEN-VALUE) = FALSE THEN
    DO:
       MESSAGE "PLEASE ENTER ONLY INTEGER VALUE OF PHONE" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       APPLY "Entry" TO FPhone. 
       vlIsValid = FALSE.
    END.
    
    ELSE IF checkInitialDigit(FPhone:SCREEN-VALUE) = FALSE THEN
    DO:
       MESSAGE "PHONE NO SHOULD BE START FORM 6,7,8 OR 9" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       APPLY "Entry" TO FPhone. 
       vlIsValid = FALSE.
    END.
    
    ELSE IF FComments:SCREEN-VALUE = "" THEN
    DO:
       MESSAGE "PLEASE ENTER COMMENTS" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       APPLY "Entry" TO FComments. 
       vlIsValid = FALSE.
    END.    
    
    ELSE IF checkInteger(INPUT FComments:SCREEN-VALUE) = TRUE THEN
    DO:
        MESSAGE "PLEASE ENTER VALID COMMENTS" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        APPLY "Entry" TO FSalesRep.
        vlIsValid = FALSE.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI CustWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(CustWin)
  THEN DELETE WIDGET CustWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI CustWin  _DEFAULT-ENABLE
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
  DISPLAY btnRadio 
      WITH FRAME DEFAULT-FRAME IN WINDOW CustWin.
  ENABLE btnRadio btnSubmit btnExit 
      WITH FRAME DEFAULT-FRAME IN WINDOW CustWin.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY FCustNum FName FCity FState FCountry FAddress FAddress2 FPostalCode 
          FContact FSalesRep FPhone FCreditLimit FBalance FTerms FDiscount 
          FComments 
      WITH FRAME new-frame IN WINDOW CustWin.
  ENABLE FName FCity FState FCountry FAddress FAddress2 FPostalCode FContact 
         FSalesRep FPhone FCreditLimit FBalance FTerms FDiscount FComments 
         btnSave btnBack 
      WITH FRAME new-frame IN WINDOW CustWin.
  {&OPEN-BROWSERS-IN-QUERY-new-frame}
  DISPLAY custNum 
      WITH FRAME search-frame IN WINDOW CustWin.
  ENABLE custNum btnSearch brCust btnReturn 
      WITH FRAME search-frame IN WINDOW CustWin.
  {&OPEN-BROWSERS-IN-QUERY-search-frame}
  VIEW CustWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION checkInitialDigit CustWin 
FUNCTION checkInitialDigit RETURNS LOGICAL
  ( INPUT pStr AS CHARACTER /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  CHECKS IF INITIAL DIGIT IS 6,7,8,9.
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE firstDigit AS INTEGER NO-UNDO.
     
    firstDigit = INTEGER(SUBSTRING(pStr,1,1)).
    IF firstDigit <> 6 AND firstDigit <> 7 AND firstDigit <> 8 AND firstDigit <> 9 THEN
        RETURN FALSE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION checkInteger CustWin 
FUNCTION checkInteger RETURNS LOGICAL
  ( INPUT pStr AS CHARACTER /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  CHECKS IF INTEGER EXISTS IN SCREEN-VALUE OR NOT
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE i  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ch AS CHARACTER NO-UNDO.

    DO i = 1 TO LENGTH(pStr):
        ch = SUBSTRING(pStr,i,1).
        IF ASC(ch) >= 48 AND ASC(ch) <= 57 THEN
           RETURN TRUE.  
    END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION isIntegerVal CustWin 
FUNCTION isIntegerVal RETURNS LOGICAL
  ( INPUT pStr AS CHARACTER /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  CHECKS IF SCREEN-VALUE IS INTEGER VALUE OR NOT
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE isInt AS INTEGER NO-UNDO.
    DEFINE VARIABLE i     AS INTEGER NO-UNDO.
    DO i = 1 TO LENGTH(pStr):
        isInt = INTEGER(SUBSTRING(pStr,i,1)) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            RETURN FALSE.
    END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

