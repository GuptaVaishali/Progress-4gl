/*------------------------------------------------------------------------
   File        : EmailServerDA.cls
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : Ruchi Gupta
   Created     : Thurs Jun 16 17:24:31 2021
   Notes       :                                             
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.   

BLOCK-LEVEL ON ERROR UNDO, THROW. 

CLASS EmailServerDA IMPLEMENTS EmailServerI: 

        {pdsEmailServer.i       &reference-only="REFERENCE-ONLY"} 
        
        DEFINE VARIABLE vcClsName                  AS CHARACTER NO-UNDO INITIAL "EmailServerDA".
        DEFINE VARIABLE vcVersion                  AS CHARACTER NO-UNDO INITIAL "10000".

        /*------------------------------------------------------------------------------
         Purpose: Retrieves and returns email settings parameters stored in CodeTable in
				  APPRISE DB
         Notes: 
        ------------------------------------------------------------------------------*/ 
        METHOD PUBLIC VOID GetEmailServer( 
            INPUT-OUTPUT DATASET pdsEmailServer): 		
            
            IF LOG-MANAGER:LOGFILE-NAME > "" THEN
            DO:
                LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("System Log for &1; Version &2", vcClsName, vcVersion)).
                LOG-MANAGER:WRITE-MESSAGE("EmailServerDA - START GetEmailServer", "INFO").     
            END.
            EMPTY TEMP-TABLE ttEmailServer. 

            CREATE ttEmailServer.
			
			ASSIGN 
				ttEmailServer.EmailServer   = ReadDBTableEntry(INPUT "EmailServer":U)
				ttEmailServer.EmailPort     = INTEGER(ReadDBTableEntry(INPUT "EmailPort":U)) 
				ttEmailServer.IsBodyHtml    = NO.     
				
			IF LOG-MANAGER:LOGFILE-NAME > "" THEN
			DO: 
                LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("EmailServerDA - GetEmailServer - ttEmailServer.EmailServer: &1", ttEmailServer.EmailServer), "INFO"). 
    			LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("EmailServerDA - GetEmailServer - ttEmailServer.EmailPort: &1", STRING(ttEmailServer.EmailPort)), "INFO"). 
    			LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("EmailServerDA - GetEmailServer - ttEmailServer.IsBodyHtml: &1", STRING(ttEmailServer.IsBodyHtml)), "INFO").
    			LOG-MANAGER:WRITE-MESSAGE("EmailServerDA - END GetEmailServer", "INFO").
            END. 
        END METHOD. 
		
        /*------------------------------------------------------------------------------
         Purpose: Retrieves and returns email settings parameters stored in CodeTable in
				  APPRISE DB
         Notes: 
        ------------------------------------------------------------------------------*/ 
        METHOD PUBLIC CHARACTER ReadDBTableEntry( 
            INPUT ipcServerPort AS CHARACTER): 
			
			IF LOG-MANAGER:LOGFILE-NAME > "" THEN
            DO: 
                LOG-MANAGER:WRITE-MESSAGE("EmailServerDA - START ReadDBTableEntry", "INFO").
			    LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("EmailServerDA - ReadDBTableEntry - ipcServerPort: &1", ipcServerPort), "INFO"). 
            END. 
            
            DEFINE BUFFER bCodeTable FOR CodeTable.
			
			FIND FIRST bCodeTable
                WHERE bCodeTable.system-id = "DH":U
                  AND bCodeTable.CodeType  = "EmailSettings":U
                  AND bCodeTable.Key1      = ipcServerPort
                  AND bCodeTable.Key2      = "":U
                NO-LOCK NO-ERROR. 
            
            IF AVAILABLE bCodeTable THEN
			DO:       
			    IF LOG-MANAGER:LOGFILE-NAME > "" THEN
                DO: 
				    LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("EmailServerDA - ReadDBTableEntry - RETURN bCodeTabl.CodeString: &1", bCodeTable.CodeVal), "INFO").
				    LOG-MANAGER:WRITE-MESSAGE("EmailServerDA - END ReadDBTableEntry", "INFO").
				END. 
				RETURN bCodeTable.CodeVal.
			END. 
			ELSE
			DO:    
			    IF LOG-MANAGER:LOGFILE-NAME > "" THEN
                DO:
                    LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("EmailServerDA - ReadDBTableEntry - Error occured in reading the record: &1", 
			            ERROR-STATUS:GET-MESSAGE(1)), "ERROR").
				    LOG-MANAGER:WRITE-MESSAGE("EmailServerDA - END ReadDBTableEntry", "INFO").
				END. 
				RETURN ?. 
			END.	
			 
		END METHOD. 

END CLASS.
