/*------------------------------------------------------------------------
	File		: CoreEmailServer
	Purpose	 	: Provide a Single Session end point to access EmailServer 
	Syntax	  	:  
	Description : 
    Author(s)   : Ruchi Gupta
    Created     : Thurs Jun 16 17:24:31 2021
	Notes	   	: 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*. 

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS CoreEmailServer:     
    
	{HTTPStatusCodes.i}

	DEFINE PUBLIC STATIC PROPERTY coreEmailServer          AS CoreEmailServer              NO-UNDO 
		GET.
		PRIVATE SET.
    
    DEFINE PUBLIC STATIC PROPERTY EmailServer 				AS CHARACTER 					NO-UNDO
        GET.
        PRIVATE SET.  
	 
	DEFINE PUBLIC STATIC PROPERTY EmailPort 				AS INTEGER 						NO-UNDO
        GET.
        PRIVATE SET.   
		
	DEFINE PUBLIC STATIC PROPERTY IsBodyHtml         		AS LOGICAL 						NO-UNDO
        GET.
        PRIVATE SET.  
    DEFINE STATIC   VARIABLE  vcClsName              AS CHARACTER                   NO-UNDO INITIAL "CoreEmailServer".
	DEFINE STATIC   VARIABLE  vcVersion 			 AS CHARACTER 				    NO-UNDO INITIAL "010000". 
	DEFINE STATIC	VARIABLE  EmailServerSI      	 AS EmailServerSI				NO-UNDO.
	 
    /*------------------------------------------------------------------------------
	 Purpose : Private Constructor. Does initial work.
	 Notes	 : Method is defined to call getEnvSettingsSI and populate General Settings
	------------------------------------------------------------------------------*/
	CONSTRUCTOR PRIVATE CoreEmailServer (): 	
	    IF LOG-MANAGER:LOGFILE-NAME > "" THEN
            LOG-MANAGER:WRITE-MESSAGE("CoreEmailServer - START Private Constructor CoreEmailServer", "INFO").
		getEmailServer().
		IF LOG-MANAGER:LOGFILE-NAME > "" THEN
            LOG-MANAGER:WRITE-MESSAGE("CoreEmailServer - END Private Constructor CoreEmailServer", "INFO").   
	END CONSTRUCTOR. 
	
	/*------------------------------------------------------------------------------
	 Purpose	: gets EmailServer from DB - via SI/BE
	 Notes		:
	------------------------------------------------------------------------------*/ 
	METHOD PUBLIC STATIC CoreEmailServer getSingleInst(
        INPUT iplRefresh AS LOGICAL): 
		
		IF LOG-MANAGER:LOGFILE-NAME > "" THEN
        DO:
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("System Log for &1; Version &2", vcClsName, vcVersion)).
            LOG-MANAGER:WRITE-MESSAGE("CoreEmailServer - START getSingleInst", "INFO").   
		    LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("CoreEmailServer - getSingleInst - iplRefresh: &1", iplRefresh), "INFO").
		END.
		
 		IF iplRefresh THEN
			IF VALID-OBJECT(coreEmailServer) THEN 
				DELETE OBJECT coreEmailServer.
        
		IF NOT VALID-OBJECT(coreEmailServer) THEN 
		DO:
			ASSIGN coreEmailServer = NEW CoreEmailServer() NO-ERROR.
			
		    IF LOG-MANAGER:LOGFILE-NAME > "" THEN
                LOG-MANAGER:WRITE-MESSAGE("CoreEmailServer - will create singleton instance", "INFO").   
			
		END.
		
		IF LOG-MANAGER:LOGFILE-NAME > "" THEN
            LOG-MANAGER:WRITE-MESSAGE("CoreEmailServer - END getSingleInst", "INFO").  
		
		RETURN coreEmailServer. 
		
	END METHOD.
	 
    
	/*------------------------------------------------------------------------------
	 Purpose	: gets EmailServer from DB - via SI/BE
	 Notes		:
	------------------------------------------------------------------------------*/ 
	METHOD PRIVATE VOID getEmailServer():
		DEFINE VARIABLE ResponseCode 		AS INTEGER  											 NO-UNDO.
		DEFINE VARIABLE ResponseBody 		AS LONGCHAR 											 NO-UNDO.
        DEFINE VARIABLE myParser            AS CLASS     Progress.Json.ObjectModel.ObjectModelParser NO-UNDO.
        DEFINE VARIABLE oJsonObj            AS CLASS     Progress.Json.ObjectModel.JsonObject        NO-UNDO. 
		
		IF LOG-MANAGER:LOGFILE-NAME > "" THEN
            LOG-MANAGER:WRITE-MESSAGE("CoreEmailServer - START getEmailServer", "INFO"). 
		
		// message "getEmailServer " view-as alert-box.
		EmailServerSI = NEW EmailServerSI(). 
        
        
		EmailServerSI:GetEmailServer( 
			INPUT "", 
			INPUT "", 
			INPUT "", 
			INPUT "",
			OUTPUT ResponseCode,
			OUTPUT ResponseBody).
                                                           
        IF LOG-MANAGER:LOGFILE-NAME > "" THEN
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("CoreEmailServer - getEmailServer - ResponseCode: &1 ResponseBody: &2", ResponseCode, ResponseBody), "INFO").   

        myParser = NEW Progress.Json.ObjectModel.ObjectModelParser(). 
        oJsonObj = CAST(myParser:Parse(ResponseBody), Progress.Json.ObjectModel.JsonObject) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            UNDO, THROW NEW Progress.Lang.AppError ("Error parsing ResponseBody",{&HTTP-BadRequest}).  
		 
        ASSIGN 
            EmailServer   = oJsonObj:GetCharacter("EmailServer")  
            EmailPort     = oJsonObj:GetInteger("EmailPort")
		    IsBodyHtml 	  = oJsonObj:GetLogical("IsBodyHtml").
            
      
		IF LOG-MANAGER:LOGFILE-NAME > "" THEN
		DO:
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("CoreEmailServer - getEmailServer - EmailServer: &1 EmailPort: &2 IsBodyHtml: &3", 
                EmailServer, EmailPort, IsBodyHtml), "INFO"). 
        
            LOG-MANAGER:WRITE-MESSAGE("CoreEmailServer - END getEmailServer", "INFO"). 
        END.                 		
		RETURN.
		
		FINALLY:
		    IF VALID-OBJECT(EmailServerSI) THEN
                DELETE OBJECT EmailServerSI.
        END FINALLY.

	END METHOD. /* END getEnvSettings */ 
	
END CLASS.
