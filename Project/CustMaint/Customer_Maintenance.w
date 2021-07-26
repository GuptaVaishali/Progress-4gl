&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports2021       PROGRESS
*/
&Scoped-define WINDOW-NAME CustomerMaintenance


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tempCustomer NO-UNDO LIKE Customer.
DEFINE TEMP-TABLE tempOrder NO-UNDO LIKE Order.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS CustomerMaintenance 
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

//DEFINE INPUT PARAMETER user_Id LIKE newUser.user_Id NO-UNDO.                                    
DEFINE INPUT PARAMETER userGroup LIKE newUser.userGroup NO-UNDO.




/* Local Variable Definitions ---                                       */

DEFINE VAR flagCheck AS INT INIT 0 NO-UNDO.
DEFINE VAR checkUser LIKE newUser.userGroup NO-UNDO.
DEFINE VARIABLE iCustnum LIKE customer.custnum NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME custBrowse

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tempCustomer

/* Definitions for BROWSE custBrowse                                    */
&Scoped-define FIELDS-IN-QUERY-custBrowse tempCustomer.CustNum ~
tempCustomer.Name tempCustomer.Address tempCustomer.Address2 ~
tempCustomer.City tempCustomer.State tempCustomer.Country ~
tempCustomer.PostalCode tempCustomer.Phone tempCustomer.EmailAddress ~
tempCustomer.Fax tempCustomer.Balance tempCustomer.Contact ~
tempCustomer.CreditLimit tempCustomer.Discount tempCustomer.SalesRep ~
tempCustomer.Terms tempCustomer.Comments 
&Scoped-define ENABLED-FIELDS-IN-QUERY-custBrowse 
&Scoped-define QUERY-STRING-custBrowse FOR EACH tempCustomer NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-custBrowse OPEN QUERY custBrowse FOR EACH tempCustomer NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-custBrowse tempCustomer
&Scoped-define FIRST-TABLE-IN-QUERY-custBrowse tempCustomer


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS custBrowse RECT-5 custName RECT-6 RECT-7 ~
Address address2 custState custCity custCountry Contact Phone fax ~
PostalCode email Balance Discount creditLimit SalesRep terms Comments ~
addCust deleteCust loadCust contract BtnDone 
&Scoped-Define DISPLAYED-OBJECTS CustNum custName Address address2 ~
custState custCity custCountry Contact Phone fax PostalCode email Balance ~
Discount creditLimit SalesRep terms Comments 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR CustomerMaintenance AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON addCust 
     LABEL "Add" 
     SIZE 13 BY .95.

DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Exit" 
     SIZE 14 BY .95
     BGCOLOR 8 .

DEFINE BUTTON cancelCust 
     LABEL "Cancel" 
     SIZE 13 BY .95.

DEFINE BUTTON contract 
     LABEL "Contract" 
     SIZE 12 BY .95.

DEFINE BUTTON deleteCust 
     LABEL "Delete" 
     SIZE 14 BY .95.

DEFINE BUTTON loadCust 
     LABEL "Load" 
     SIZE 12 BY .95.

DEFINE BUTTON saveCust 
     LABEL "Save" 
     SIZE 13 BY .95.

DEFINE VARIABLE Address LIKE tempCustomer.Address
     VIEW-AS FILL-IN 
     SIZE 25.8 BY 1 NO-UNDO.

DEFINE VARIABLE address2 LIKE tempCustomer.Address2
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE Balance LIKE tempCustomer.Balance
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1 NO-UNDO.

DEFINE VARIABLE Comments LIKE tempCustomer.Comments
     VIEW-AS FILL-IN 
     SIZE 121 BY 1.19 NO-UNDO.

DEFINE VARIABLE Contact LIKE tempCustomer.Contact
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1 NO-UNDO.

DEFINE VARIABLE creditLimit LIKE tempCustomer.CreditLimit
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE custCity LIKE tempCustomer.City
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE custCountry LIKE tempCustomer.Country
     VIEW-AS FILL-IN 
     SIZE 12.2 BY 1 NO-UNDO.

DEFINE VARIABLE custName LIKE tempCustomer.Name
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE CustNum LIKE tempCustomer.CustNum
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE custState LIKE tempCustomer.State
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE Discount AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Discount" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE email LIKE tempCustomer.EmailAddress
     VIEW-AS FILL-IN 
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE fax LIKE tempCustomer.Fax
     VIEW-AS FILL-IN 
     SIZE 12.4 BY 1 NO-UNDO.

DEFINE VARIABLE Phone LIKE tempCustomer.Phone
     VIEW-AS FILL-IN 
     SIZE 13.8 BY 1 NO-UNDO.

DEFINE VARIABLE PostalCode LIKE tempCustomer.PostalCode
     VIEW-AS FILL-IN 
     SIZE 12.4 BY 1 NO-UNDO.

DEFINE VARIABLE SalesRep LIKE tempCustomer.SalesRep
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE terms LIKE tempCustomer.Terms
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 85 BY 11.19.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 141 BY 5.95.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 131 BY 3.1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY custBrowse FOR 
      tempCustomer SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE custBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS custBrowse CustomerMaintenance _STRUCTURED
  QUERY custBrowse NO-LOCK DISPLAY
      tempCustomer.CustNum FORMAT ">>>>9":U WIDTH 11.2
      tempCustomer.Name FORMAT "x(30)":U
      tempCustomer.Address FORMAT "x(35)":U
      tempCustomer.Address2 FORMAT "x(35)":U
      tempCustomer.City FORMAT "x(25)":U
      tempCustomer.State FORMAT "x(20)":U
      tempCustomer.Country FORMAT "x(20)":U
      tempCustomer.PostalCode FORMAT "x(10)":U
      tempCustomer.Phone FORMAT "x(20)":U
      tempCustomer.EmailAddress FORMAT "x(50)":U
      tempCustomer.Fax FORMAT "x(20)":U
      tempCustomer.Balance FORMAT "->,>>>,>>9.99":U
      tempCustomer.Contact FORMAT "x(30)":U
      tempCustomer.CreditLimit FORMAT "->,>>>,>>9":U WIDTH 21.4
      tempCustomer.Discount FORMAT ">>9%":U WIDTH 18.2
      tempCustomer.SalesRep FORMAT "x(4)":U
      tempCustomer.Terms FORMAT "x(20)":U WIDTH 17.4
      tempCustomer.Comments FORMAT "x(80)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 234 BY 10.71
         FONT 0
         TITLE "Customer Information" ROW-HEIGHT-CHARS .7 FIT-LAST-COLUMN TOOLTIP "Customer Information".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     custBrowse AT ROW 4.81 COL 17 WIDGET-ID 200
     CustNum AT ROW 18.14 COL 20.8 HELP
          "Please enter a customer number." WIDGET-ID 22
     custName AT ROW 20.05 COL 24.2 HELP
          "Please enter a name." WIDGET-ID 26
     Address AT ROW 21.71 COL 22.4 HELP
          "Please enter an address." WIDGET-ID 44
     address2 AT ROW 23.33 COL 21.2 HELP
          "Please enter an address." WIDGET-ID 34
     custState AT ROW 24.81 COL 25.2 HELP
          "Please enter standard state abbreviation." WIDGET-ID 38
     custCity AT ROW 26.71 COL 26.4 HELP
          "Please enter a city." WIDGET-ID 32
     custCountry AT ROW 18.14 COL 60.2 HELP
          "Please enter a country." WIDGET-ID 28
     Contact AT ROW 20.1 COL 59.8 HELP
          "Please enter a contact." WIDGET-ID 50
     Phone AT ROW 21.86 COL 60.8 HELP
          "Please enter a phone number" WIDGET-ID 54
     fax AT ROW 23.48 COL 64 HELP
          "Please enter a fax number." WIDGET-ID 48
     PostalCode AT ROW 24.95 COL 56 HELP
          "Please enter the appropriate Postal Code." WIDGET-ID 52
     email AT ROW 26.71 COL 62.6 HELP
          "Please enter an full Internet Email Address." WIDGET-ID 42
     Balance AT ROW 18.62 COL 114.6 COLON-ALIGNED HELP
          "Please enter a balance." WIDGET-ID 40
     Discount AT ROW 18.62 COL 142.6 COLON-ALIGNED HELP
          "Please enter a percentage from 0 to 100." WIDGET-ID 30
     creditLimit AT ROW 18.62 COL 170.6 COLON-ALIGNED HELP
          "Please enter a Credit Limit." WIDGET-ID 24
     SalesRep AT ROW 18.62 COL 199.2 COLON-ALIGNED HELP
          "Please Enter a Sales Rep." WIDGET-ID 36
     terms AT ROW 18.62 COL 224.2 COLON-ALIGNED HELP
          "Please enter terms" WIDGET-ID 60
     Comments AT ROW 21.05 COL 114.6 COLON-ALIGNED HELP
          "Please enter comments." WIDGET-ID 62
     addCust AT ROW 26.14 COL 119.4 WIDGET-ID 6
     saveCust AT ROW 26.1 COL 136.4 WIDGET-ID 12
     cancelCust AT ROW 26.05 COL 152.6 WIDGET-ID 10
     deleteCust AT ROW 26 COL 169 WIDGET-ID 8
     loadCust AT ROW 26 COL 186 WIDGET-ID 16
     contract AT ROW 26 COL 202 WIDGET-ID 14
     BtnDone AT ROW 26 COL 217.4 WIDGET-ID 2
     "Customer Maintenance Window" VIEW-AS TEXT
          SIZE 59 BY 2.62 AT ROW 1.71 COL 108 WIDGET-ID 4
          FGCOLOR 1 FONT 28
     RECT-5 AT ROW 16.95 COL 16.8 WIDGET-ID 68
     RECT-6 AT ROW 17.14 COL 105.4 WIDGET-ID 70
     RECT-7 AT ROW 24.95 COL 110.4 WIDGET-ID 72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 273.2 BY 33.52
         BGCOLOR 15 
         DEFAULT-BUTTON BtnDone WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tempCustomer T "?" NO-UNDO Sports2021 Customer
      TABLE: tempOrder T "?" NO-UNDO Sports2021 Order
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW CustomerMaintenance ASSIGN
         HIDDEN             = YES
         TITLE              = "Customer Information"
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
/* SETTINGS FOR WINDOW CustomerMaintenance
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB custBrowse 1 DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN Address IN FRAME DEFAULT-FRAME
   ALIGN-L LIKE = Temp-Tables.tempCustomer. EXP-SIZE                    */
/* SETTINGS FOR FILL-IN address2 IN FRAME DEFAULT-FRAME
   ALIGN-L LIKE = Temp-Tables.tempCustomer.Address2 EXP-SIZE            */
/* SETTINGS FOR FILL-IN Balance IN FRAME DEFAULT-FRAME
   LIKE = Temp-Tables.tempCustomer.Balance EXP-SIZE                     */
/* SETTINGS FOR BUTTON cancelCust IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Comments IN FRAME DEFAULT-FRAME
   LIKE = Temp-Tables.tempCustomer. EXP-SIZE                            */
/* SETTINGS FOR FILL-IN Contact IN FRAME DEFAULT-FRAME
   ALIGN-L LIKE = Temp-Tables.tempCustomer.Contact EXP-SIZE             */
/* SETTINGS FOR FILL-IN creditLimit IN FRAME DEFAULT-FRAME
   LIKE = Temp-Tables.tempCustomer.CreditLimit EXP-SIZE                 */
/* SETTINGS FOR FILL-IN custCity IN FRAME DEFAULT-FRAME
   ALIGN-L LIKE = Temp-Tables.tempCustomer.City EXP-SIZE                */
/* SETTINGS FOR FILL-IN custCountry IN FRAME DEFAULT-FRAME
   ALIGN-L LIKE = Temp-Tables.tempCustomer.Country EXP-SIZE             */
/* SETTINGS FOR FILL-IN custName IN FRAME DEFAULT-FRAME
   ALIGN-L LIKE = Temp-Tables.tempCustomer.Name EXP-SIZE                */
/* SETTINGS FOR FILL-IN CustNum IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L LIKE = Temp-Tables.tempCustomer. EXP-SIZE          */
ASSIGN 
       CustNum:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN custState IN FRAME DEFAULT-FRAME
   ALIGN-L LIKE = Temp-Tables.tempCustomer.State EXP-SIZE               */
/* SETTINGS FOR FILL-IN email IN FRAME DEFAULT-FRAME
   ALIGN-L LIKE = Temp-Tables.tempCustomer.EmailAddress EXP-SIZE        */
/* SETTINGS FOR FILL-IN fax IN FRAME DEFAULT-FRAME
   ALIGN-L LIKE = Temp-Tables.tempCustomer.Fax EXP-SIZE                 */
/* SETTINGS FOR FILL-IN Phone IN FRAME DEFAULT-FRAME
   ALIGN-L LIKE = Temp-Tables.tempCustomer. EXP-SIZE                    */
/* SETTINGS FOR FILL-IN PostalCode IN FRAME DEFAULT-FRAME
   ALIGN-L LIKE = Temp-Tables.tempCustomer. EXP-SIZE                    */
/* SETTINGS FOR FILL-IN SalesRep IN FRAME DEFAULT-FRAME
   LIKE = Temp-Tables.tempCustomer.                                     */
/* SETTINGS FOR BUTTON saveCust IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN terms IN FRAME DEFAULT-FRAME
   LIKE = Temp-Tables.tempCustomer.Terms                                */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(CustomerMaintenance)
THEN CustomerMaintenance:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE custBrowse
/* Query rebuild information for BROWSE custBrowse
     _TblList          = "Temp-Tables.tempCustomer"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.tempCustomer.CustNum
"tempCustomer.CustNum" ? ? "integer" ? ? ? ? ? ? no ? no no "11.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = Temp-Tables.tempCustomer.Name
     _FldNameList[3]   = Temp-Tables.tempCustomer.Address
     _FldNameList[4]   = Temp-Tables.tempCustomer.Address2
     _FldNameList[5]   = Temp-Tables.tempCustomer.City
     _FldNameList[6]   = Temp-Tables.tempCustomer.State
     _FldNameList[7]   = Temp-Tables.tempCustomer.Country
     _FldNameList[8]   = Temp-Tables.tempCustomer.PostalCode
     _FldNameList[9]   = Temp-Tables.tempCustomer.Phone
     _FldNameList[10]   = Temp-Tables.tempCustomer.EmailAddress
     _FldNameList[11]   = Temp-Tables.tempCustomer.Fax
     _FldNameList[12]   = Temp-Tables.tempCustomer.Balance
     _FldNameList[13]   = Temp-Tables.tempCustomer.Contact
     _FldNameList[14]   > Temp-Tables.tempCustomer.CreditLimit
"tempCustomer.CreditLimit" ? ? "decimal" ? ? ? ? ? ? no ? no no "21.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > Temp-Tables.tempCustomer.Discount
"tempCustomer.Discount" ? ? "integer" ? ? ? ? ? ? no ? no no "18.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   = Temp-Tables.tempCustomer.SalesRep
     _FldNameList[17]   > Temp-Tables.tempCustomer.Terms
"tempCustomer.Terms" ? ? "character" ? ? ? ? ? ? no ? no no "17.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   = Temp-Tables.tempCustomer.Comments
     _Query            is NOT OPENED
*/  /* BROWSE custBrowse */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME CustomerMaintenance
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CustomerMaintenance CustomerMaintenance
ON END-ERROR OF CustomerMaintenance /* Customer Information */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CustomerMaintenance CustomerMaintenance
ON WINDOW-CLOSE OF CustomerMaintenance /* Customer Information */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME addCust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL addCust CustomerMaintenance
ON CHOOSE OF addCust IN FRAME DEFAULT-FRAME /* Add */
DO:
          custBrowse:SENSITIVE = FALSE.
          deleteCust:SENSITIVE = FALSE.
          cancelCust:SENSITIVE = TRUE.
          saveCust:SENSITIVE = TRUE.
          ASSIGN custNum = 0 custName = "" phone = "" custCity = "" custState = "" custCountry = "" address = ""
                 address2 = "" postalCode = "" email = "" fax = "" balance = 0.0 discount = 0 creditLimit = 0
                 salesRep = "" terms = "" contact = "" comments = "". 
          
          FIND LAST tempcustomer.
                       ASSIGN custNum = tempCustomer.custNum + 1.
          DISP custNum  custName custCity custState custcountry phone contact address address2 postalCode 
               email fax balance discount creditLimit salesRep terms comments WITH FRAME {&frame-name}.
               
          DISABLE custNum WITH FRAME {&frame-name}.
          
          addCust:SENSITIVE = FALSE.
          flagCheck = 1.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Address
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Address CustomerMaintenance
ON LEAVE OF Address IN FRAME DEFAULT-FRAME /* Address */
DO:
  IF LENGTH(address:SCREEN-VALUE) < 2 THEN
    DO:
          MESSAGE "Invalid Value please Retry!!"
              VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
          ASSIGN address = "" .
         
          RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone CustomerMaintenance
ON CHOOSE OF BtnDone IN FRAME DEFAULT-FRAME /* Exit */
DO:
      IF flagCheck = 0 THEN
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
       ELSE
          MESSAGE "Update Pending"
              VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
          
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cancelCust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cancelCust CustomerMaintenance
ON CHOOSE OF cancelCust IN FRAME DEFAULT-FRAME /* Cancel */
DO:
                       MESSAGE "You Will Lost The Updates??"
                              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirm " UPDATE Choice AS LOGICAL.
                            
                       IF choice THEN
                          DO:
                                 FIND FIRST tempCustomer NO-LOCK NO-ERROR.      
                                 ASSIGN custNum = tempCustomer.custNum custName = tempCustomer.NAME phone = tempCustomer.phone custCity = tempCustomer.city 
                                        custState = tempCustomer.state custCountry = tempCustomer.country address = tempCustomer.address address2 = tempCustomer.address2
                                        postalCode = tempCustomer.postalCode email = tempCustomer.emailAddress /*fax = "" balance = 0.0 discount = 0.0 creditLimit = 0.0
                                        salesRep = "" terms = "" contact = "" comments = ""*/ . 
                                          
                                  DISP custNum custName custCity custState custcountry phone contact address address2 postalCode 
                                       email /*fax balance discount creditLimit salesRep terms comments*/ WITH FRAME {&frame-name}. 
                                  custBrowse:SENSITIVE = TRUE.
                                  saveCust:SENSITIVE = FALSE.
                                  addCust:SENSITIVE = TRUE.
                                  deleteCust:SENSITIVE = TRUE.
                                  cancelCust:SENSITIVE = FALSE.
                                  flagCheck = 0.
                                  
                            END.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Contact
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Contact CustomerMaintenance
ON LEAVE OF Contact IN FRAME DEFAULT-FRAME /* Contact */
DO:
     IF LENGTH(contact:SCREEN-VALUE) < 1 THEN
    DO:
          MESSAGE "Invalid Value please Retry!!"
              VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
          ASSIGN contact = "".
    
          RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contract
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contract CustomerMaintenance
ON CHOOSE OF contract IN FRAME DEFAULT-FRAME /* Contract */
DO:
  
  SELF:SENSITIVE = FALSE.
  RUN custMaint\LoadContract\ContractWindow.w(INPUT iCustnum).
  SELF:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME custBrowse
&Scoped-define SELF-NAME custBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL custBrowse CustomerMaintenance
ON VALUE-CHANGED OF custBrowse IN FRAME DEFAULT-FRAME /* Customer Information */
DO:
      ASSIGN custNum:SCREEN-VALUE IN FRAME {&frame-name} = STRING(tempCustomer.custNum)
             custNAME:SCREEN-VALUE IN FRAME {&frame-name} = tempCustomer.NAME
             custcity:SCREEN-VALUE IN FRAME {&frame-name} = tempCustomer.city
             custstate:SCREEN-VALUE IN FRAME {&frame-name} = tempCustomer.state
             address:SCREEN-VALUE IN FRAME {&frame-name} = tempCustomer.address
             address2:SCREEN-VALUE IN FRAME {&frame-name} = tempCustomer.address2
             postalCode:SCREEN-VALUE IN FRAME {&frame-name} = tempCustomer.postalCode
             phone:SCREEN-VALUE IN FRAME {&frame-name} = tempCustomer.phone
             email:SCREEN-VALUE IN FRAME {&frame-name} = tempCustomer.emailAddress
             fax:SCREEN-VALUE IN FRAME {&frame-name} = tempCustomer.fax
             balance:SCREEN-VALUE IN FRAME {&frame-name} = STRING(tempCustomer.balance)
             creditLimit:SCREEN-VALUE IN FRAME {&frame-name} = string(tempCustomer.creditLimit)
             salesRep:SCREEN-VALUE IN FRAME {&frame-name} = tempCustomer.salesRep
             terms:SCREEN-VALUE IN FRAME {&frame-name} = tempCustomer.terms
             discount:SCREEN-VALUE IN FRAME {&frame-name} = string(tempCustomer.discount)
             comments:SCREEN-VALUE IN FRAME {&frame-name} = tempCustomer.comments
             contact:SCREEN-VALUE IN FRAME {&frame-name} = tempCustomer.contact.
             iCustnum = INT (custNum:SCREEN-VALUE IN FRAME {&frame-name}).
          
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME custCity
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL custCity CustomerMaintenance
ON LEAVE OF custCity IN FRAME DEFAULT-FRAME /* City */
DO:
    IF LENGTH(custCity:SCREEN-VALUE) < 2 THEN
    DO:
          MESSAGE "Invalid Value please Retry!!"
              VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
          ASSIGN custCity = "".
          
    
          RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME custCountry
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL custCountry CustomerMaintenance
ON LEAVE OF custCountry IN FRAME DEFAULT-FRAME /* Country */
DO:
     IF LENGTH(custCountry:SCREEN-VALUE) < 1 THEN
    DO:
          MESSAGE "Invalid Value please Retry!!"
              VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
          ASSIGN custCountry = "".
         
          RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME custName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL custName CustomerMaintenance
ON LEAVE OF custName IN FRAME DEFAULT-FRAME /* Name */
DO:
    IF LENGTH(custName:SCREEN-VALUE) < 1 THEN
    DO:
          MESSAGE "Invalid Value please Retry!!"
              VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
          ASSIGN custName = "".  
                
          RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME custState
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL custState CustomerMaintenance
ON LEAVE OF custState IN FRAME DEFAULT-FRAME /* State */
DO:
    IF LENGTH(custState:SCREEN-VALUE) < 1 THEN
    DO:
          MESSAGE "Invalid Value please Retry!!"
              VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
          ASSIGN custState = "".
          

          RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME deleteCust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL deleteCust CustomerMaintenance
ON CHOOSE OF deleteCust IN FRAME DEFAULT-FRAME /* Delete */
DO:  
    
        FIND FIRST Customer WHERE Customer.custNum = int(custNum:SCREEN-VALUE) NO-ERROR.
        IF AVAILABLE Customer THEN
           DO:
                IF NOT CAN-FIND(order WHERE order.custNum = int(custNum:SCREEN-VALUE)) THEN
                   DO:   
                        MESSAGE "Are You Sure You Wannt To Delete The Selected Record?"
                                 VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirm Deletion" UPDATE ans AS LOGICAL.
                        IF ans THEN
                           DO:
                                DELETE tempCustomer.
                                DELETE customer.
                                MESSAGE "Record Deleted Succesfully!!"
                                         VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
                                OPEN QUERY custBrowse FOR EACH tempCustomer NO-LOCK INDEXED-REPOSITION.
                                APPLY "value-changed" TO custBrowse.    
                           END.    
                                
                     END.
                                
                 ELSE
                        MESSAGE "Cant Delete Record Exist In Order Table" VIEW-AS ALERT-BOX.
            END.
        ELSE
              MESSAGE "Record Not Exist"
                       VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME loadCust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL loadCust CustomerMaintenance
ON CHOOSE OF loadCust IN FRAME DEFAULT-FRAME /* Load */
DO:
    SELF:SENSITIVE = FALSE.
    RUN CustMaint\LoadContract\chooseWindow.w.
    SELF:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Phone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Phone CustomerMaintenance
ON LEAVE OF Phone IN FRAME DEFAULT-FRAME /* Phone */
DO:
      IF LENGTH(phone:SCREEN-VALUE) NE 10 THEN
    DO:
          MESSAGE "Invalid Value please Retry!!"
              VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
          ASSIGN phone = "".
          
        
          RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME PostalCode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL PostalCode CustomerMaintenance
ON LEAVE OF PostalCode IN FRAME DEFAULT-FRAME /* Postal Code */
DO:
  IF LENGTH(postalCode:SCREEN-VALUE) < 6 THEN
    DO:
          MESSAGE "Invalid Value please Retry!!"
              VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
          ASSIGN postalCode = "".
          
  
          RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME saveCust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL saveCust CustomerMaintenance
ON CHOOSE OF saveCust IN FRAME DEFAULT-FRAME /* Save */
DO:
    
    
                      CREATE tempCustomer.
                      ASSIGN tempCustomer.custNum = int(custNum:SCREEN-VALUE)
                             tempCustomer.NAME = custName:SCREEN-VALUE
                             tempCustomer.city = custCity:SCREEN-VALUE
                             tempCustomer.state = custState:SCREEN-VALUE
                             tempCustomer.country = custCountry:SCREEN-VALUE
                             tempCustomer.phone = phone:SCREEN-VALUE
                             tempCustomer.contact = contact:SCREEN-VALUE
                             tempCustomer.address = address:SCREEN-VALUE
                             tempCustomer.address2 = address2:SCREEN-VALUE
                             tempCustomer.postalCode = postalCode:SCREEN-VALUE
                             tempCustomer.emailAddress = email:SCREEN-VALUE
                             tempCustomer.fax = fax:SCREEN-VALUE 
                             tempCustomer.balance = decimal(balance:SCREEN-VALUE)
                             tempCustomer.creditLimit = INTEGER(creditLimit:SCREEN-VALUE)
                             tempCustomer.salesRep = salesRep:SCREEN-VALUE
                             tempCustomer.terms = terms:SCREEN-VALUE
                             tempCustomer.comments = comments:SCREEN-VALUE.
                             tempCustomer.discount = decimal(discount:SCREEN-VALUE). 
                 
                       
                       MESSAGE "Record Saved Successfully!!"
                          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
                                 
                       
                       DISABLE custNum custName custCity custState custcountry phone contact address address2 postalCode 
                                 email fax balance discount creditLimit salesRep terms comments WITH FRAME {&frame-name}.
                          
                       custBrowse:SENSITIVE = TRUE.
                       deleteCust:SENSITIVE = TRUE.
                       addCust:SENSITIVE = TRUE.
                       savecust:SENSITIVE = FALSE.
                       cancelcust:SENSITIVE = FALSE.
                       
                       ENABLE custName custCity custState custcountry phone contact address address2 postalCode 
                              email fax balance discount creditLimit salesRep terms comments WITH FRAME {&frame-name}.
                       FIND LAST tempCustomer NO-LOCK NO-ERROR.
                       IF AVAILABLE tempCustomer THEN
                           DO:
                                    CREATE customer.
                                    BUFFER-COPY  tempCustomer TO customer.
                                     OPEN QUERY custBrowse FOR EACH tempCustomer NO-LOCK INDEXED-REPOSITION.
                                         APPLY "value-changed" TO custBrowse.
                           END.
                          
                       ELSE 
                           MESSAGE "Record Not Exist"
                               VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
                        flagCheck = 0.        
                    
              END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME terms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL terms CustomerMaintenance
ON LEAVE OF terms IN FRAME DEFAULT-FRAME /* Terms */
DO:
     ASSIGN terms = "NET30".
     DISP terms WITH FRAME {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK CustomerMaintenance 


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

  IF lc(userGroup) EQ "general"  THEN
  DO:
       addcust:SENSITIVE = false.
       deletecust:SENSITIVE = false.
       custName:SENSITIVE = false.
       custCity:SENSITIVE = false.
       custState:SENSITIVE = false.
       custCountry:SENSITIVE = false.
       phone:SENSITIVE = false.
       contact:SENSITIVE = false.
       address:SENSITIVE = false.
       address2:SENSITIVE = false.
       postalCode:SENSITIVE = false.
       email:SENSITIVE = false.
       fax:SENSITIVE = false.
       balance:SENSITIVE = false.
       discount:SENSITIVE = false.
       creditLimit:SENSITIVE = false.
       salesRep:SENSITIVE = false.
       terms:SENSITIVE = false.
       comments:SENSITIVE = false.

  END.
   FOR EACH customer:
       CREATE tempCustomer.
       BUFFER-COPY customer TO tempCustomer.
   END.
   OPEN QUERY custBrowse FOR EACH tempCustomer NO-LOCK INDEXED-REPOSITION.
   APPLY "value-changed" TO custBrowse.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI CustomerMaintenance  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(CustomerMaintenance)
  THEN DELETE WIDGET CustomerMaintenance.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI CustomerMaintenance  _DEFAULT-ENABLE
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
  DISPLAY CustNum custName Address address2 custState custCity custCountry 
          Contact Phone fax PostalCode email Balance Discount creditLimit 
          SalesRep terms Comments 
      WITH FRAME DEFAULT-FRAME IN WINDOW CustomerMaintenance.
  ENABLE custBrowse RECT-5 custName RECT-6 RECT-7 Address address2 custState 
         custCity custCountry Contact Phone fax PostalCode email Balance 
         Discount creditLimit SalesRep terms Comments addCust deleteCust 
         loadCust contract BtnDone 
      WITH FRAME DEFAULT-FRAME IN WINDOW CustomerMaintenance.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW CustomerMaintenance.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

