/* 
    PURPOSE - RECORDS DELETION ACCORDING TO CONFIGURATION SETTINGS STORED IN DB.
    AUTHOR  - VAISHALI GUPTA
    CREATION-DATE - 06-JUN-2021
*/

/*************Variables Declaration************************************/
DEFINE VARIABLE cTableName LIKE configuration.tablename NO-UNDO.
DEFINE VARIABLE iDayCount  AS   INT                     NO-UNDO.
DEFINE VARIABLE iTimeLimit LIKE configuration.timelimit NO-UNDO.
DEFINE VARIABLE iDelLimit  LIKE configuration.dellimit  NO-UNDO.                                
DEFINE VARIABLE iLockLimit LIKE configuration.locklimit NO-UNDO.
DEFINE VARIABLE iRecLimit  LIKE configuration.reclimit  NO-UNDO.  

DEFINE VARIABLE delCount       AS INT     INITIAL 1    NO-UNDO.
DEFINE VARIABLE lockedCount    AS INT     INITIAL 0    NO-UNDO.                                          
DEFINE VARIABLE recCount       AS INT     INITIAL 1    NO-UNDO.
DEFINE VARIABLE checkAvailable AS LOGICAL INITIAL TRUE NO-UNDO.


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
    REPEAT WHILE ETIME < iTimeLimit AND delcount <= iDelLimit AND checkAvailable AND lockedCount <= iLockLimit TRANSACTION:
        recCount = 1.
        DO WHILE ETIME < iTimeLimit AND recCount <= iRecLimit AND delcount <= iDelLimit AND lockedCount <= iLockLimit AND checkAvailable:       
            FIND NEXT customer WHERE createdDate < (TODAY - iDayCount) EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            IF NOT AVAILABLE customer THEN
                IF LOCKED customer THEN
                DO:                               
                    lockedCount = lockedCount + 1.
                    FIND NEXT customer WHERE createdDate < (TODAY - iDayCount) NO-LOCK NO-ERROR.  
                END.
                ELSE 
                DO:
                    MESSAGE "Record Not Available" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
                    checkAvailable = FALSE.
                END.
            ELSE
            DO:
                DELETE customer.                                
                delcount = delcount + 1.
                recCount = recCount + 1.
            END.    
        END.           
    END.  
    MESSAGE delCount + "RECORDS DELETED SUCCESSFULLY" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
END.
ELSE IF  cTableName = "order" THEN
DO:
    REPEAT WHILE ETIME < iTimeLimit AND delcount <= iDelLimit AND checkAvailable AND lockedCount <= iLockLimit TRANSACTION:
        recCount = 1.
        DO WHILE ETIME < iTimeLimit AND recCount <= iRecLimit AND delCount <= iDelLimit AND lockedCount <= iLockLimit AND checkAvailable:       
            FIND NEXT order WHERE createdDate < (TODAY - iDayCount) EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            IF NOT AVAILABLE order THEN
                IF LOCKED order THEN
                DO:                               
                    lockedCount = lockedCount + 1.
                    FIND NEXT order WHERE createdDate < (TODAY - iDayCount) NO-LOCK NO-ERROR.  
                END.
                ELSE 
                DO:
                    MESSAGE "Record Not Available" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
                    checkAvailable = FALSE.
                END.
            ELSE
            DO:
                DELETE order.
                delCount = delCount + 1.
                recCount = recCount + 1.
            END.    
        END.           
    END. 
    MESSAGE "RECORDS DELETED SUCCESSFULLY" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
END.     




