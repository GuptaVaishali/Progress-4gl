/*------------------------------------------------------------------------
   File        : EmailServerSI.cls
   Purpose     : 
   Syntax      : 
   Description : Service Interface
   Author(s)   : Ruchi Gupta
   Created     : Thurs Jun 16 17:24:31 2021
   Notes       :  
 ----------------------------------------------------------------------*/
BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Progress.Json.ObjectModel.*.

CLASS EmailServerSI USE-WIDGET-POOL: //INHERITS Service.ServiceMainSI USE-WIDGET-POOL:
    
	{HTTPStatusCodes.i}
	
	DEFINE VARIABLE vcClsName                  AS CHARACTER NO-UNDO INITIAL "EmailServerSI".
    DEFINE VARIABLE vcVersion                  AS CHARACTER NO-UNDO INITIAL "10000".
        
    DEFINE VARIABLE viStartTime                AS INTEGER   NO-UNDO.  
    DEFINE VARIABLE viEndTime                  AS INTEGER   NO-UNDO. 
    DEFINE VARIABLE viStartTimeMS              AS INTEGER   NO-UNDO.  
    DEFINE VARIABLE viEndTimeMS                AS INTEGER   NO-UNDO. 
  
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/    
    CONSTRUCTOR PUBLIC EmailServerSI (  ):
        SUPER ().
    
    END CONSTRUCTOR.   
    
   
    /*------------------------------------------------------------------------------
     Purpose: Delete for EmailServer.
     Notes:
    ------------------------------------------------------------------------------*/       
    METHOD PUBLIC VOID DeleteEmailServer (
        INPUT  dandh-tenant           AS CHARACTER,
        INPUT  dandh-source           AS CHARACTER,
        INPUT  dandh-application-user AS CHARACTER,
        INPUT  dandh-sessionid        AS CHARACTER,
        OUTPUT ResponseCode           AS INTEGER, 
        OUTPUT ResponseBody           AS LONGCHAR ):
        
        IF LOG-MANAGER:LOGFILE-NAME > "" THEN
        DO: 
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("System Log for &1; Version &2", vcClsName, vcVersion)).
            LOG-MANAGER:WRITE-MESSAGE("EmailServerSI - START DeleteEmailServer;", "INFO").                          
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("EmailServerSI - DeleteEmailServer - dandh-tenant: &1", dandh-tenant), "INFO"). 
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("EmailServerSI - DeleteEmailServer - dandh-source: &1", dandh-source), "INFO").                   
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("EmailServerSI - DeleteEmailServer - dandh-application-user: &1", dandh-application-user), "INFO"). 
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("EmailServerSI - DeleteEmailServer - dandh-sessionid: &1", dandh-sessionid), "INFO").
        END. 
        
        UNDO, THROW NEW Progress.Lang.AppError ("Method Not Allowed", {&HTTP-MethodNotAllowed}).
 //       {\\devwms01\WMS\Server\Common\v1\Service\serviceSICatch.i}
    
    END METHOD.       
    
    /*------------------------------------------------------------------------------
     Purpose: Get EmailServer.
     Notes:
    ------------------------------------------------------------------------------*/       
    METHOD PUBLIC VOID GetEmailServer (
        INPUT  dandh-tenant           AS CHARACTER,
        INPUT  dandh-source           AS CHARACTER,
        INPUT  dandh-application-user AS CHARACTER,
        INPUT  dandh-sessionid        AS CHARACTER,
        OUTPUT ResponseCode           AS INTEGER, 
        OUTPUT ResponseBody           AS LONGCHAR ): 
        
        DEFINE VARIABLE oEmailServerBE      AS CLASS     EmailServerBE       NO-UNDO. 
              
        IF LOG-MANAGER:LOGFILE-NAME > "" THEN
        DO: 
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("System Log for &1; Version &2", vcClsName, vcVersion)).
            LOG-MANAGER:WRITE-MESSAGE("EmailServerSI - START GetEmailServer;", "INFO").                          
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("EmailServerSI - GetEmailServer - dandh-tenant: &1", dandh-tenant), "INFO"). 
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("EmailServerSI - GetEmailServer - dandh-source: &1", dandh-source), "INFO").                   
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("EmailServerSI - GetEmailServer - dandh-application-user: &1", dandh-application-user), "INFO"). 
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("EmailServerSI - GetEmailServer - dandh-sessionid: &1", dandh-sessionid), "INFO").
        END. 
        ASSIGN viStartTime   = TIME
               viStartTimeMS = ETIME(NO).
               
        ASSIGN
            ResponseCode = {&HTTP-OK}. 
            
        // instantiate and call the EmailServer BE
        oEmailServerBE = NEW EmailServerBE().
            
        
        // Execute Methods 
        ResponseBody = oEmailServerBE:GetEmailServer().  
        
        ASSIGN viEndTime   = TIME
               viEndTimeMS = ETIME(NO). 
                    
        IF LOG-MANAGER:LOGFILE-NAME > "" THEN
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("EmailServerSI - GetEmailServer - Time Consumption Statistics: 
                API: &1 Start time: &2 End time: &3 Total time consumed: &4 Seconds &5 milliseconds ", PROGRAM-NAME(1), STRING(viStartTime, "hh:mm:ss"), 
                STRING(viEndTime, "hh:mm:ss"), STRING(viEndTime - viStartTime), STRING(viEndTimeMS - viStartTimeMS)), "INFO").
        
        IF ResponseBody = "" THEN
            UNDO, THROW NEW Progress.Lang.AppError ("No Email Settings found.", {&HTTP-UnprocessableEntity}).
            
		IF LOG-MANAGER:LOGFILE-NAME > "" THEN
        DO: 
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("EmailServerSI - GetEmailServer - ResponseCode: &1 ResponseBody: &2", ResponseCode, ResponseBody), "INFO"). 
		    LOG-MANAGER:WRITE-MESSAGE("EmailServerSI - END GetEmailServer", "INFO").
        END. 
            
        RETURN. 
        
     //   {\\devwms01\WMS\Server\Common\v1\Service\serviceSICatch.i}             
    
        FINALLY:
            DELETE OBJECT oEmailServerBE NO-ERROR.
        END FINALLY.        
        
    END METHOD.                                                                                         
     
    /*------------------------------------------------------------------------------
     Purpose: Post for EmailServer.
     Notes:
    ------------------------------------------------------------------------------*/       
    METHOD PUBLIC VOID PostEmailServer (
        INPUT  dandh-tenant           AS CHARACTER,
        INPUT  dandh-source           AS CHARACTER,
        INPUT  dandh-application-user AS CHARACTER,
        INPUT  dandh-sessionid        AS CHARACTER,
        OUTPUT ResponseCode           AS INTEGER, 
        OUTPUT ResponseBody           AS LONGCHAR ):
        
        IF LOG-MANAGER:LOGFILE-NAME > "" THEN
        DO: 
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("System Log for &1; Version &2", vcClsName, vcVersion)).
            LOG-MANAGER:WRITE-MESSAGE("EmailServerSI - START PostEmailServer;", "INFO").                          
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("EmailServerSI - PostEmailServer - dandh-tenant: &1", dandh-tenant), "INFO"). 
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("EmailServerSI - PostEmailServer - dandh-source: &1", dandh-source), "INFO").                   
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("EmailServerSI - PostEmailServer - dandh-application-user: &1", dandh-application-user), "INFO"). 
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("EmailServerSI - PostEmailServer - dandh-sessionid: &1", dandh-sessionid), "INFO").
        END. 
        UNDO, THROW NEW Progress.Lang.AppError ("Method Not Allowed", {&HTTP-MethodNotAllowed}).
	//	{\\devwms01\WMS\Server\Common\v1\Service\serviceSICatch.i}
    
    END METHOD.           

    /*------------------------------------------------------------------------------
     Purpose: Put for EmailServer.
     Notes:
    ------------------------------------------------------------------------------*/       
    METHOD PUBLIC VOID PutEmailServer (
        INPUT  dandh-tenant           AS CHARACTER,
        INPUT  dandh-source           AS CHARACTER,
        INPUT  dandh-application-user AS CHARACTER,
        INPUT  dandh-sessionid        AS CHARACTER,
        OUTPUT ResponseCode           AS INTEGER, 
        OUTPUT ResponseBody           AS LONGCHAR ):
        
        IF LOG-MANAGER:LOGFILE-NAME > "" THEN
        DO: 
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("System Log for &1; Version &2", vcClsName, vcVersion)).
            LOG-MANAGER:WRITE-MESSAGE("EmailServerSI - START PutEmailServer;", "INFO").                          
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("EmailServerSI - PutEmailServer - dandh-tenant: &1", dandh-tenant), "INFO"). 
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("EmailServerSI - PutEmailServer - dandh-source: &1", dandh-source), "INFO").                   
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("EmailServerSI - PutEmailServer - dandh-application-user: &1", dandh-application-user), "INFO"). 
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("EmailServerSI - PutEmailServer - dandh-sessionid: &1", dandh-sessionid), "INFO").
        END. 
        UNDO, THROW NEW Progress.Lang.AppError ("Method Not Allowed", {&HTTP-MethodNotAllowed}).
      //  {\\devwms01\WMS\Server\Common\v1\Service\serviceSICatch.i}
    
    END METHOD.     
    
END CLASS.
