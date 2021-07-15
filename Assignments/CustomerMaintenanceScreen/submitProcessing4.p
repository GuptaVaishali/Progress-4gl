/*
    PURPOSE OF PROGRAM - SEARCH AND UPDATE CUSTOMERS AND CREATION OF NEW CUSTOMER RECORDS.
    AUTHOR-NAME - VAISHALI GUPTA
    CREATION DATE - 14-06-2021
*/

/*******DEFINING INPUT PARAMETER******************/
DEFINE INPUT PARAMETER pSelectedValue AS CHARACTER NO-UNDO.

 /*********DEFINING ALL VARIABLES**********************************/
DEFINE VARIABLE viCnum        LIKE customer.cust-num     VIEW-AS FILL-IN NO-UNDO.
DEFINE VARIABLE vcName        LIKE customer.NAME         VIEW-AS FILL-IN NO-UNDO.
DEFINE VARIABLE vcCountry     LIKE customer.country      VIEW-AS FILL-IN NO-UNDO INITIAL "U.S.A".
DEFINE VARIABLE vcAddress     LIKE customer.address      VIEW-AS FILL-IN NO-UNDO.
DEFINE VARIABLE vcAddress2    LIKE customer.address2     VIEW-AS FILL-IN NO-UNDO.
DEFINE VARIABLE vcCity        LIKE customer.city         VIEW-AS FILL-IN NO-UNDO.
DEFINE VARIABLE vcState       LIKE customer.state        VIEW-AS FILL-IN NO-UNDO.
DEFINE VARIABLE vcPostalCode  LIKE customer.postal-code  VIEW-AS FILL-IN NO-UNDO. 
DEFINE VARIABLE vcContact     LIKE customer.contact      VIEW-AS FILL-IN NO-UNDO.
DEFINE VARIABLE vcSalesRep    LIKE customer.sales-rep    VIEW-AS FILL-IN NO-UNDO.
DEFINE VARIABLE vcPhone       LIKE customer.phone        VIEW-AS FILL-IN NO-UNDO. 
DEFINE VARIABLE vdCreditLimit LIKE customer.credit-limit VIEW-AS FILL-IN NO-UNDO.
DEFINE VARIABLE vdBalance     LIKE customer.balance      VIEW-AS FILL-IN NO-UNDO.
DEFINE VARIABLE vcTerms       LIKE customer.terms        VIEW-AS FILL-IN NO-UNDO.
DEFINE VARIABLE viDiscount    LIKE customer.discount     VIEW-AS FILL-IN NO-UNDO.
DEFINE VARIABLE vcComments    LIKE customer.comments     VIEW-AS FILL-IN NO-UNDO.

/*****DEFINE BUTTON**************/
DEFINE BUTTON btnSave LABEL "Save".

/********DEFINE FRAME "NEW-FRAME"*********/
DEFINE FRAME new-frame 
      viCnum vcName vcCountry vcAddress vcAddress2 vcCity vcState vcPostalCode vcContact vcSalesRep vcPhone vdCreditLimit vdBalance vcTerms viDiscount vcComments btnSave.

/********CHECKS IF INTEGER EXISTS IN SCREEN-VALUE OR NOT********/
FUNCTION checkInteger RETURNS LOGICAL (INPUT pStr AS CHARACTER).
    DEFINE VARIABLE i  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ch AS CHARACTER NO-UNDO.

    DO i = 1 TO LENGTH(pStr):
        ch = SUBSTRING(pStr,i,1).
        IF ASC(ch) >= 48 AND ASC(ch) <= 57 THEN
           RETURN TRUE.  
    END.
END FUNCTION.

/***********CHECKS IF SCREEN-VALUE IS INTEGER VALUE OR NOT************/
FUNCTION isIntegerVal RETURNS LOGICAL(INPUT pStr AS CHARACTER).
    DEFINE VARIABLE isInt AS INTEGER NO-UNDO.
    DEFINE VARIABLE i     AS INTEGER NO-UNDO.
    DO i = 1 TO LENGTH(pStr):
        isInt = INTEGER(SUBSTRING(pStr,i,1)) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            RETURN FALSE.
    END.
END FUNCTION.

/***********CHECKS IF INITIAL DIGIT IS 6,7,8,9 ************/
FUNCTION checkInitialDigit RETURNS LOGICAL(INPUT pStr AS CHARACTER).
    DEFINE VARIABLE firstDigit AS INTEGER NO-UNDO.
     
    firstDigit = INTEGER(SUBSTRING(pStr,1,1)).
    IF firstDigit <> 6 AND firstDigit <> 7 AND firstDigit <> 8 AND firstDigit <> 9 THEN
        RETURN FALSE.
END FUNCTION.  

/***************VALIDATING ALL FIELDS*****************************************/
PROCEDURE checkValid:
    DEFINE INPUT-OUTPUT PARAMETER vlIsValid AS  LOGICAL NO-UNDO.
    
    IF vcName:SCREEN-VALUE IN FRAME new-frame = "" THEN
    DO:
       MESSAGE "PLEASE ENTER CUSTOMER NAME" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       APPLY "Entry" TO vcName.
       vlIsValid = FALSE.
    END.
    
    ELSE IF checkInteger(INPUT vcName:SCREEN-VALUE) = TRUE THEN
    DO:
        MESSAGE "PLEASE ENTER VALID CUSTOMER NAME" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        APPLY "Entry" TO vcName.
        vlIsValid = FALSE.
    END.
        
    ELSE IF vcAddress:SCREEN-VALUE = "" THEN
    DO:
       MESSAGE "PLEASE ENTER CUSTOMER ADDRESS" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       APPLY "Entry" TO vcAddress.
       vlIsValid = FALSE.
    END.
    
    ELSE IF vcCity:SCREEN-VALUE = "" THEN
    DO:
       MESSAGE "PLEASE ENTER CUSTOMER CITY" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       APPLY "Entry" TO vcCity.
       vlIsValid = FALSE.
    END.   
    
    ELSE IF checkInteger(INPUT vcCity:SCREEN-VALUE) = TRUE THEN
    DO:
        MESSAGE "PLEASE ENTER VALID CUSTOMER CITY" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        APPLY "Entry" TO vcCity.
        vlIsValid = FALSE.
    END.
    
    ELSE IF vcState:SCREEN-VALUE = "" THEN
    DO:
       MESSAGE "PLEASE ENTER CUSTOMER STATE" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       APPLY "Entry" TO vcState.
       vlIsValid = FALSE.
    END.
    
    ELSE IF checkInteger(INPUT vcState:SCREEN-VALUE) = TRUE THEN
    DO:
        MESSAGE "PLEASE ENTER VALID CUSTOMER STATE" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        APPLY "Entry" TO vcState.
        vlIsValid = FALSE.
    END.
    
    ELSE IF vcPostalCode:SCREEN-VALUE = "" THEN
    DO:
       MESSAGE "PLEASE ENTER CUSTOMER POSTALCODE" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       APPLY "Entry" TO vcPostalCode. 
       vlIsValid = FALSE.
    END.
    ELSE IF LENGTH(vcPostalCode:SCREEN-VALUE) <> 6 THEN
    DO:
       MESSAGE "PLEASE ENTER 6 DIGIT POSTALCODE" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       APPLY "Entry" TO vcPostalCode. 
       vlIsValid = FALSE.
    END.

    ELSE IF isIntegerVal(vcPostalCode:SCREEN-VALUE) = FALSE THEN
    DO:
       MESSAGE "PLEASE ENTER ONLY INTEGER VALUE OF POSTAL-CODE" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       APPLY "Entry" TO vcPostalCode. 
       vlIsValid = FALSE.
    END.
    
    ELSE IF vcContact:SCREEN-VALUE = "" THEN
    DO:
       MESSAGE "PLEASE ENTER CUSTOMER CONTACT" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       APPLY "Entry" TO vcContact. 
       vlIsValid = FALSE.
    END.
    
    ELSE IF checkInteger(INPUT vcContact:SCREEN-VALUE) = TRUE THEN
    DO:
        MESSAGE "PLEASE ENTER VALID CUSTOMER CONTACT" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        APPLY "Entry" TO vcContact.
        vlIsValid = FALSE.
    END.
       
    ELSE IF vcSalesRep:SCREEN-VALUE = "" THEN
    DO:
       MESSAGE "PLEASE ENTER CUSTOMER SALESREP" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       APPLY "Entry" TO vcSalesRep. 
       vlIsValid = FALSE.
    END. 
    
    ELSE IF checkInteger(INPUT vcSalesRep:SCREEN-VALUE) = TRUE THEN
    DO:
        MESSAGE "PLEASE ENTER VALID SALESREP VALUE" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        APPLY "Entry" TO vcSalesRep.
        vlIsValid = FALSE.
    END.
    
    ELSE IF vcPhone:SCREEN-VALUE = "" THEN
    DO:
       MESSAGE "PLEASE ENTER CUSTOMER PHONE" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       APPLY "Entry" TO vcPhone. 
       vlIsValid = FALSE.
    END.
    
    ELSE IF LENGTH(vcPhone:SCREEN-VALUE) <> 10 THEN
    DO:
       MESSAGE "PLEASE ENTER 10 DIGIT PHONE" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       APPLY "Entry" TO vcPhone. 
       vlIsValid = FALSE.
    END.
    
    ELSE IF isIntegerVal(vcPhone:SCREEN-VALUE) = FALSE THEN
    DO:
       MESSAGE "PLEASE ENTER ONLY INTEGER VALUE OF PHONE" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       APPLY "Entry" TO vcPhone. 
       vlIsValid = FALSE.
    END.
    
    ELSE IF checkInitialDigit(vcPhone:SCREEN-VALUE) = FALSE THEN
    DO:
       MESSAGE "PHONE NO SHOULD BE START FORM 6,7,8 OR 9" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       APPLY "Entry" TO vcPhone. 
       vlIsValid = FALSE.
    END.
    
 
    ELSE IF vcComments:SCREEN-VALUE = "" THEN
    DO:
       MESSAGE "PLEASE ENTER COMMENTS" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       APPLY "Entry" TO vcComments. 
       vlIsValid = FALSE.
    END.    
    
    ELSE IF checkInteger(INPUT vcComments:SCREEN-VALUE) = TRUE THEN
    DO:
        MESSAGE "PLEASE ENTER VALID COMMENTS" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        APPLY "Entry" TO vcSalesRep.
        vlIsValid = FALSE.
    END.
END.    /* PROCEDURE CHECK VALID END */


/********IF RADIO BUTTON VALUE IS SEARCH************************/
IF pSelectedValue = "search" THEN
DO:    
    /**ASK CUSTOMER NUMBER FROM USER TO SEARCH*****/
    PROMPT-FOR viCnum WITH FRAME new-frame.
    ASSIGN viCnum.
    
    FIND FIRST customer WHERE customer.cust-num = INTEGER (viCnum:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE customer THEN
    DO:
        DISPLAY viCnum vcName vcCountry vcAddress vcAddress2 vcCity vcState vcPostalCode vcContact vcSalesRep vcPhone vdCreditLimit vdBalance vcTerms viDiscount vcComments 
            WITH FRAME new-frame 2 COLUMNS VIEW-AS DIALOG-BOX TITLE "Display Customer".
       
        ASSIGN vcCountry:SCREEN-VALUE     = customer.country    
               vcName:SCREEN-VALUE        = customer.NAME          
               vcAddress:SCREEN-VALUE     = customer.address       
               vcAddress2:SCREEN-VALUE    = customer.address2      
               vcCity:SCREEN-VALUE        = customer.city         
               vcState:SCREEN-VALUE       = customer.state        
               vcPostalCode:SCREEN-VALUE  = customer.postal-code   
               vcContact:SCREEN-VALUE     = customer.contact     
               vcPhone:SCREEN-VALUE       = customer.phone         
               vcSalesRep:SCREEN-VALUE    = customer.sales-rep     
               vdCreditLimit:SCREEN-VALUE = STRING(customer.credit-limit)
               vdBalance:SCREEN-VALUE     = STRING(customer.balance)     
               vcTerms:SCREEN-VALUE       = customer.terms        
               viDiscount:SCREEN-VALUE    = STRING(customer.discount)      
               vcComments:SCREEN-VALUE    = customer.comments.
        
        ON CHOOSE OF btnSave IN FRAME new-frame
        DO:
            DEFINE VARIABLE vlIsValid AS LOGICAL NO-UNDO INITIAL TRUE.
            RUN checkvalid(INPUT-OUTPUT vlIsvalid).
           
            IF vlIsValid THEN
            DO:
                FIND FIRST customer WHERE customer.cust-num = INTEGER (viCnum:SCREEN-VALUE) EXCLUSIVE-LOCK NO-ERROR.
                IF NOT AVAILABLE customer THEN
                DO:
                    IF LOCKED customer THEN
                        MESSAGE "RECORD IS LOCKED BY ANOTHER USER" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
                END.
                ELSE
                DO:
                    ASSIGN customer.country      = vcCountry:SCREEN-VALUE IN FRAME new-frame
                           customer.NAME         = vcName:SCREEN-VALUE
                           customer.address      = vcAddress:SCREEN-VALUE
                           customer.address2     = vcAddress2:SCREEN-VALUE
                           customer.city         = vcCity:SCREEN-VALUE
                           customer.state        = vcState:SCREEN-VALUE
                           customer.postal-code  = vcPostalCode:SCREEN-VALUE
                           customer.contact      = vcContact:SCREEN-VALUE
                           customer.phone        = vcPhone:SCREEN-VALUE
                           customer.sales-rep    = vcSalesRep:SCREEN-VALUE
                           customer.credit-limit = DECIMAL(vdCreditLimit:SCREEN-VALUE)
                           customer.balance      = DECIMAL(vdBalance:SCREEN-VALUE)
                           customer.terms        = vcTerms:SCREEN-VALUE
                           customer.discount     = DECIMAL ( TRIM ( viDiscount:SCREEN-VALUE IN FRAME new-frame, "%" ) )
                           customer.comments     = vcComments:SCREEN-VALUE.
                           
                    MESSAGE "RECORD UPDATED SUCCESSFULLY" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.  
                END.
            END.
        END.     /* btnSave TRIGGER END */
                   
        ENABLE ALL EXCEPT viCnum WITH FRAME new-frame.  
        WAIT-FOR "Window-close" OF FRAME new-frame.
    END.
    ELSE
        MESSAGE "CUSTOMER SEARCH CRITERION IS NOT VALID" VIEW-AS ALERT-BOX INFORMATION BUTTON OK.        
END.

/************IF RADIO BUTTON VALUE IS NEW**********************************/
ELSE 
DO:  
     /********AUTO-GENERATED VALUE OF CUSTOMER*************************/
    FIND LAST customer NO-LOCK NO-ERROR.
    IF AVAILABLE customer THEN
        viCnum = customer.cust-num + 1.
    ELSE
        viCnum = 1.        
       
    
    /************DISPLAY ALL FIELDS*********************************/
    DISPLAY viCnum vcName vcCountry vcAddress vcAddress2 vcCity vcState vcPostalCode vcContact vcSalesRep vcPhone vdCreditLimit vdBalance vcTerms viDiscount vcComments 
       WITH FRAME new-frame 2 COLUMNS VIEW-AS DIALOG-BOX TITLE "Insert Customer".
 
    
    ON CHOOSE OF btnSave 
    DO: 
        DEFINE VARIABLE vlIsValid AS LOGICAL NO-UNDO INITIAL TRUE.
        RUN checkvalid(INPUT-OUTPUT vlIsvalid).
           
        IF vlIsValid THEN
        DO:     
            /*************CREATE CUSTOMER RECORD **************************/
            CREATE customer.
            
            /*********IF SALESREP VALUE DOES NOT EXIST IN SALESREP TABLE, THEN CREATE A RECORD IN SALESREP TABLE***************/
            FIND FIRST salesrep WHERE salesRep.sales-rep = vcSalesRep NO-LOCK NO-ERROR.
            IF NOT AVAILABLE salesrep THEN
            DO:
                CREATE salesrep.
                salesrep.sales-rep = vcSalesRep.
            END.
            ASSIGN customer.cust-num     = viCnum
                   customer.country      = vcCountry:SCREEN-VALUE
                   customer.NAME         = vcName:SCREEN-VALUE
                   customer.address      = vcAddress:SCREEN-VALUE
                   customer.address2     = vcAddress2:SCREEN-VALUE
                   customer.city         = vcCity:SCREEN-VALUE
                   customer.state        = vcState:SCREEN-VALUE
                   customer.postal-code  = vcPostalCode:SCREEN-VALUE
                   customer.contact      = vcContact:SCREEN-VALUE
                   customer.phone        = vcPhone:SCREEN-VALUE
                   customer.sales-rep    = vcSalesRep:SCREEN-VALUE
                   customer.credit-limit = DECIMAL(vdCreditLimit:SCREEN-VALUE)
                   customer.balance      = DECIMAL(vdBalance:SCREEN-VALUE)
                   customer.terms        = vcTerms:SCREEN-VALUE
                   customer.discount     = INTEGER( TRIM ( viDiscount:SCREEN-VALUE IN FRAME new-frame, "%" ) )
                   customer.comments     = vcComments:SCREEN-VALUE.
                   
           MESSAGE "RECORD CREATED SUCCESSFULLY" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
           btnSave:SENSITIVE = FALSE.
       END.
    END.   /* END OF BtnSave TRIGGER */
    
    ENABLE ALL EXCEPT viCnum WITH FRAME new-frame.  
    WAIT-FOR "Window-close" OF FRAME new-frame.
 END.
