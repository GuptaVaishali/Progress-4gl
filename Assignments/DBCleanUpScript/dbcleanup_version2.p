/* 
    PURPOSE - RECORDS DELETION ACCORDING TO CONFIGURATION SETTINGS STORED IN DB.
    AUTHOR  - VAISHALI GUPTA
    CREATION-DATE - 07-JUN-2021
*/

/*************Defining Variables************************************/
DEFINE VARIABLE cTableName LIKE configuration.tablename NO-UNDO.
DEFINE VARIABLE iDayCount  AS   INT                     NO-UNDO.                         
DEFINE VARIABLE iTimeLimit LIKE configuration.timelimit NO-UNDO.
DEFINE VARIABLE iDelLimit  LIKE configuration.dellimit  NO-UNDO.
DEFINE VARIABLE iLockLimit LIKE configuration.locklimit NO-UNDO.
DEFINE VARIABLE iRecLimit  LIKE configuration.reclimit  NO-UNDO.  

DEFINE VARIABLE idelCount    AS INT NO-UNDO.
DEFINE VARIABLE ilockedCount AS INT NO-UNDO.                                          
DEFINE VARIABLE irecCount    AS INT NO-UNDO.
DEFINE VARIABLE iCnum        AS INT NO-UNDO.
DEFINE VARIABLE iOrdernum    AS INT NO-UNDO.

/****************Defining Buffers*****************************/
DEFINE BUFFER buCustomer FOR customer.
DEFINE BUFFER buOrder    FOR order.


SET cTableName.
FIND configuration WHERE configuration.tablename = cTableName NO-LOCK NO-ERROR.  
IF NOT AVAILABLE configuration THEN
DO:
    MESSAGE "CONFIGURATION DOES NOT EXIST FOR THIS TABLE" + cTableName VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    RETURN.
END.
ELSE
    /*****************Checking IF all Entries are in one field*******************************/
    IF NUM-ENTRIES(configuration.dayCount) > 1 THEN                                                                                                                  
        IF NUM-ENTRIES(configuration.dayCount) <> 5 THEN
        DO:
            MESSAGE "CONFIGURATION NOT SETUP PROPERLY" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            RETURN.
        END.
        ELSE
        DO:   
            iDayCount  = INTEGER(ENTRY(1,configuration.dayCount)).   
            iTimeLimit = INTEGER(ENTRY(2,configuration.dayCount)).
            iDelLimit  = INTEGER(ENTRY(3,configuration.dayCount)).
            iLockLimit = INTEGER(ENTRY(4,configuration.dayCount)).
            iRecLimit  = INTEGER(ENTRY(5,configuration.dayCount)). 
        END.
    /***************All Entries are in seperate fields*************************************************/
    ELSE
        IF configuration.dayCount = "" OR configuration.timeLimit = 0  OR configuration.delLimit = 0          
            OR configuration.lockLimit = 0 OR configuration.recLimit = 0 THEN
        DO:
             MESSAGE "CONFIGURATION NOT SETUP PROPERLY" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
             RETURN.
        END.
        ELSE
        DO:
            iDayCount  = INTEGER(configuration.dayCount).
            iTimeLimit = configuration.timeLimit.
            iDelLimit  = configuration.delLimit.
            iLockLimit = configuration.lockLimit.
            iRecLimit  = configuration.recLimit. 
        END.

ETIME(YES).

IF  cTableName = "customer" THEN
DO:
    REPEAT WHILE ETIME < iTimeLimit AND idelCount < iDelLimit AND ilockedCount <= iLockLimit TRANSACTION:
        FOR EACH customer WHERE createdDate < (TODAY - iDayCount) AND customer.cust-num > iCnum NO-LOCK irecCount = 1 TO iRecLimit:
            FIND FIRST buCustomer WHERE ROWID(buCustomer) = ROWID(customer) EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            IF NOT AVAILABLE buCustomer THEN
            DO:
                IF LOCKED buCustomer THEN                               
                    ilockedCount = ilockedCount + 1.
            END.
            ELSE
            DO:
                iCnum = buCustomer.cust-num.
                DELETE buCustomer.                                
                idelCount = idelCount + 1. 
            END.
        END. 
    END.  
    MESSAGE idelCount "RECORDS DELETED SUCCESSFULLY" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
END.
ELSE IF  cTableName = "order" THEN
DO:
    REPEAT WHILE ETIME < iTimeLimit AND idelCount < iDelLimit AND ilockedCount <= iLockLimit TRANSACTION:
        FOR EACH order WHERE createdDate < (TODAY - iDayCount) AND order.order-num > iOrdernum  NO-LOCK irecCount = 1 TO iRecLimit:
            FIND FIRST buOrder WHERE ROWID(buOrder) = ROWID(order) EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            IF NOT AVAILABLE buOrder THEN
            DO:
                IF LOCKED buOrder THEN                               
                    ilockedCount = ilockedCount + 1.
            END.
            ELSE
            DO:
                iOrdernum = buOrder.order-num.
                DELETE buOrder.  
                idelCount = idelCount + 1.
            END.    
        END.          
    END.  
    MESSAGE idelCount "RECORDS DELETED SUCCESSFULLY" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
END.     




