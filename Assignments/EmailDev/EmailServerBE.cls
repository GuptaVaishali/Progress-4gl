/*------------------------------------------------------------------------
   File        : EmailServerBE.cls
   Purpose     : 
   Syntax      : 
   Description :         
   Author(s)   : Ruchi Gupta
   Created     : Thurs Jun 16 17:24:31 2021
   Notes       :     
 ----------------------------------------------------------------------*/
USING Progress.Lang.*.
USING Progress.Json.ObjectModel.JsonObject. 

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS EmailServerBE:  
    
    {pdsEmailServer.i}
    
    DEFINE VARIABLE vcClsName       AS CHARACTER                    NO-UNDO INITIAL "EmailServerBE".
	DEFINE VARIABLE vcVersion 		AS CHARACTER					NO-UNDO INITIAL "010000".
    DEFINE VARIABLE vcEmailServer   AS CHARACTER                    NO-UNDO INITIAL ?. 
	DEFINE VARIABLE viEmailPort     AS INTEGER                      NO-UNDO INITIAL ?.   
	DEFINE VARIABLE vlIsBodyHtml    AS LOGICAL                      NO-UNDO INITIAL ?.   
    DEFINE VARIABLE oEmailServerDA  AS CLASS 		EmailServerDA   NO-UNDO.     
        
    CONSTRUCTOR PUBLIC EmailServerBE (  ):
        SUPER ().    
        IF LOG-MANAGER:LOGFILE-NAME > "" THEN
            LOG-MANAGER:WRITE-MESSAGE("EmailServerBE - START Constructor EmailServerBE", "INFO").
        
        oEmailServerDA = NEW EmailServerDA(). 
        IF LOG-MANAGER:LOGFILE-NAME > "" THEN
            LOG-MANAGER:WRITE-MESSAGE("EmailServerBE - END Constructor EmailServerBE", "INFO").       
    END CONSTRUCTOR.
    
    DESTRUCTOR PUBLIC EmailServerBE (  ):
        IF LOG-MANAGER:LOGFILE-NAME > "" THEN
            LOG-MANAGER:WRITE-MESSAGE("EmailServerBE - START DESTRUCTOR EmailServerBE", "INFO").
        DELETE OBJECT oEmailServerDA NO-ERROR.
        IF LOG-MANAGER:LOGFILE-NAME > "" THEN
            LOG-MANAGER:WRITE-MESSAGE("EmailServerBE - END DESTRUCTOR EmailServerBE", "INFO").
    END DESTRUCTOR.
        
    /*------------------------------------------------------------------------------
     Purpose:  
     Notes:  
    ------------------------------------------------------------------------------*/ 
    METHOD PUBLIC LONGCHAR GetEmailServer ():   
	      
        DEFINE VARIABLE vlcJsonResponse             AS LONGCHAR      NO-UNDO.   
		
		IF LOG-MANAGER:LOGFILE-NAME > "" THEN
		DO:
            LOG-MANAGER:WRITE-MESSAGE("EmailServerBE - START GetEmailServer", "INFO").
		    LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("EmailServerBE - GetEmailServer - &1 Version: &2", vcClsName, vcVersion), "INFO").
		END. 
		
        oEmailServerDA:GetEmailServer ( 
            INPUT-OUTPUT DATASET pdsEmailServer BY-REFERENCE). 
			
		FIND FIRST ttEmailServer NO-ERROR.
		
		ASSIGN 
			vcEmailServer   = ttEmailServer.EmailServer
			viEmailPort     = ttEmailServer.EmailPort   
			vlIsBodyHtml    = ttEmailServer.IsBodyHtml.  
			
		IF LOG-MANAGER:LOGFILE-NAME > "" THEN
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("EmailServerBE - GetEmailServer - vcEmailServer: &1 viEmailPort: &2 vlIsBodyHtml: &3", 
                vcEmailServer, viEmailPort, vlIsBodyHtml), "INFO"). 
         
        ASSIGN 
            vlcJsonResponse = THIS-OBJECT:SerializeJson().   
        
        IF LOG-MANAGER:LOGFILE-NAME > "" THEN
        DO:
            LOG-MANAGER:WRITE-MESSAGE(
            SUBSTITUTE("EmailServerBE - GetEmailServer - &1: ", vlcJsonResponse), "INFO"). 
            LOG-MANAGER:WRITE-MESSAGE("EmailServerBE - END GetEmailServer", "INFO").
        END.
                   
        RETURN vlcJsonResponse.    
			 
    END METHOD.  

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR SerializeJson():    
        
        DEFINE VARIABLE JsonEmailSettings   AS Progress.Json.ObjectModel.JsonObject NO-UNDO. 
        DEFINE VARIABLE lcJsonEmailSettings AS LONGCHAR                             NO-UNDO.  
              
        IF LOG-MANAGER:LOGFILE-NAME > "" THEN
            LOG-MANAGER:WRITE-MESSAGE("EmailServerBE - Start SerializeJson", "INFO").
        
        // create response as Json object           
        JsonEmailSettings = NEW Progress.Json.ObjectModel.JsonObject().
          
        JsonEmailSettings:ADD("EmailServer", vcEmailServer).
        JsonEmailSettings:ADD("EmailPort", viEmailPort).  
        JsonEmailSettings:ADD("IsBodyHtml", vlIsBodyHtml). 
        
        JsonEmailSettings:WRITE(lcJsonEmailSettings, TRUE).
        
        IF LOG-MANAGER:LOGFILE-NAME > "" THEN
        DO:
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("EmailServerBE - SerializeJson - lcJsonEmailSettings: &1", lcJsonEmailSettings), "INFO"). 
            LOG-MANAGER:WRITE-MESSAGE("EmailServerBE - End SerializeJson", "INFO").
        END. 
        
        RETURN lcJsonEmailSettings. 
        
        FINALLY:
            DELETE OBJECT JsonEmailSettings NO-ERROR.   
        END FINALLY.   

    END METHOD.	
	 
END CLASS.    
