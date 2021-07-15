/*------------------------------------------------------------------------
    File        : EmailClient
    Purpose     : 
    Syntax      : Receives Json formatted data containing info on Email Settings
                  {
                      "EmailServer": "string",                  Email server e.g. Email.dandh.com
                      "EmailPort": integer,                     This will be 25 always
                      "EmailTo": "string,string",               Comma delimited list of recipients
                      "EmailFrom": "string",                    Email sender's details
                      "ReplyTo": "string,string",               Comma delimited list of ReplyTo addresses  
                      "EmailSender": "string",                  Email sender's details
                      "EmailCC": "string,string",               Comma delimited list of CC addresses  
                      "EmailBCC": "string,string",              Comma delimited list of BCC addresses
                      "EmailSubject": "string",                 Email subject
                      "EmailBody": "string",                    Email body text 
                      "IsBodyHtml": boolean,                    Email body type: text(false)/HTML(true) 
                      "Attachments": "string,string",           Comma delimited list of attachment files
                      "AttachmentNames": "string,string",       Comma delimited list of attachment names
                      "ReadReceipt": boolean                    Email read receipt flag (false - no read receipt/true - read receipt is required)
                  }               
    Description : Sends Email using System.Net.Email.SmtpClient.
    Author(s)   : Ruchi Gupta
    Created     :  
    Notes       :
        
    MODIFICATION LOG:
        initials - mm/dd/yyyy - RTB # - description
             RG  - 06/11/2011 - XXXXX - Created. 
  ----------------------------------------------------------------------*/ 
BLOCK-LEVEL ON ERROR UNDO, THROW.

USING System.Net.Mail.*.
USING Progress.Json.ObjectModel.*.
USING System.Net.Mail.SmtpDeliveryMethod.


CLASS EmailClient: 

    DEFINE STATIC VARIABLE vcClsName            AS CHARACTER                                      NO-UNDO INITIAL "EmailClient".
    DEFINE STATIC VARIABLE vcVersion            AS CHARACTER                                      NO-UNDO INITIAL "10000".
    
    DEFINE STATIC VARIABLE vcError              AS CHARACTER                                      NO-UNDO.  
    DEFINE STATIC VARIABLE oJsonObject          AS JsonObject                                     NO-UNDO. 
    DEFINE STATIC VARIABLE vlcRetJsonObject     AS LONGCHAR                                       NO-UNDO.

    METHOD STATIC PUBLIC JsonObject SendEmail 
        (INPUT oJsonObj AS JsonObject): 
		          
        DEFINE VARIABLE oSmtpClient             AS CLASS SmtpClient                                      NO-UNDO.
        DEFINE VARIABLE oEmailMsg               AS CLASS MailMessage                                     NO-UNDO. 
        DEFINE VARIABLE oAttachment             AS CLASS Attachment                                      NO-UNDO.  
   //     DEFINE VARIABLE oJsonObj                AS CLASS JsonObject                                      NO-UNDO.  
        DEFINE VARIABLE oObjModelParser         AS CLASS ObjectModelParser                               NO-UNDO.    
         
        DEFINE VARIABLE vcAttachUse             AS CHARACTER                                             NO-UNDO.
        DEFINE VARIABLE viCount                 AS INTEGER                                               NO-UNDO. 
        DEFINE VARIABLE vcEmailServer           AS CHARACTER                                             NO-UNDO.   
        DEFINE VARIABLE viEmailPort             AS INTEGER                                               NO-UNDO.   
        DEFINE VARIABLE vcTo                    AS CHARACTER                                             NO-UNDO.   
        DEFINE VARIABLE vcFrom                  AS CHARACTER                                             NO-UNDO.   
        DEFINE VARIABLE vcReplyTo               AS CHARACTER                                             NO-UNDO.   
        DEFINE VARIABLE vcSender                AS CHARACTER                                             NO-UNDO.   
        DEFINE VARIABLE vcCC                    AS CHARACTER                                             NO-UNDO.   
        DEFINE VARIABLE vcBCC                   AS CHARACTER                                             NO-UNDO.  
        DEFINE VARIABLE vcSubject               AS CHARACTER                                             NO-UNDO. 
        DEFINE VARIABLE vcBodyText              AS CHARACTER                                             NO-UNDO. 
        DEFINE VARIABLE vlIsBodyHtml            AS LOGICAL                                               NO-UNDO. 
        DEFINE VARIABLE vcAttachment            AS CHARACTER                                             NO-UNDO. 
        DEFINE VARIABLE vcAttachmentName        AS CHARACTER                                             NO-UNDO.
        DEFINE VARIABLE vlReadReceipt           AS LOGICAL                                               NO-UNDO.  
        
        IF LOG-MANAGER:LOGFILE-NAME > "" THEN
        DO: 
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("System Log for &1; Version &2", vcClsName, vcVersion)).
            LOG-MANAGER:WRITE-MESSAGE("START SendEmail", "INFO").   
         //   LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("SendEmail - Input EmailSettings Json: &1", STRING(iplcEmailSettings)),"INFO").
        END. 
        
	/*	ASSIGN 
		    oObjModelParser = NEW ObjectModelParser(). 
		    oJsonObj = CAST(oObjModelParser:Parse(iplcEmailSettings), Progress.Json.ObjectModel.JsonObject) NO-ERROR.      */
		    
	/*	IF ERROR-STATUS:ERROR THEN
		DO:
		    vcError = "Error parsing inputted emailsettings json".
		    IF LOG-MANAGER:LOGFILE-NAME > "" THEN
            DO: 
                LOG-MANAGER:WRITE-MESSAGE("SendEmail - Error parsing inputted emailsettings json", "ERROR"). 
            END.  
            RETURN SerializeJson().  
		END. /* IF ERROR-STATUS:ERROR THEN */     */
		
	    ASSIGN vcEmailServer     = oJsonObj:GetCharacter("EmailServer")
		       viEmailPort       = oJsonObj:GetInteger("EmailPort") 
	           vcTo              = oJsonObj:GetCharacter("EmailTo") 
	           vcFrom            = oJsonObj:GetCharacter("EmailFrom") 
	           vcReplyTo         = oJsonObj:GetCharacter("ReplyTo")
	           vcSender          = oJsonObj:GetCharacter("EmailSender") 
	           vcCC              = oJsonObj:GetCharacter("EmailCC") 
	           vcBCC             = oJsonObj:GetCharacter("EmailBCC") 
	           vcSubject         = oJsonObj:GetCharacter("EmailSubject") 
	           vcBodyText        = oJsonObj:GetCharacter("EmailBody")      
	           vlIsBodyHtml      = oJsonObj:GetLogical("IsBodyHtml") 
	           vcAttachment      = oJsonObj:GetCharacter("Attachments") 
	           vcAttachmentName  = oJsonObj:GetCharacter("AttachmentNames") 
	           vlReadReceipt     = oJsonObj:GetLogical("ReadReceipt").  
               
       
	    
	    IF LOG-MANAGER:LOGFILE-NAME > "" THEN
	    DO:
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("SendEmail - vcEmailServer: &1", vcEmailServer), "INFO"). 
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("SendEmail - viEmailPort: &1", STRING(viEmailPort)), "INFO").
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("SendEmail - vcTo: &1", vcTo), "INFO").
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("SendEmail - vcFrom: &1", vcFrom), "INFO"). 
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("SendEmail - vcReplyTo: &1", vcReplyTo), "INFO").
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("SendEmail - vcSender: &1", vcSender), "INFO").
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("SendEmail - vcCC: &1", vcCC), "INFO"). 
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("SendEmail - vcBCC: &1", vcBCC), "INFO").
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("SendEmail - vcSubject: &1", vcSubject), "INFO").
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("SendEmail - vcBodyText: &1", vcBodyText), "INFO"). 
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("SendEmail - vlIsBodyHtml: &1", STRING(vlIsBodyHtml)), "INFO").
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("SendEmail - vcAttachment: &1", vcAttachment), "INFO").
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("SendEmail - vcAttachmentName: &1", vcAttachmentName), "INFO"). 
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("SendEmail - vlReadReceipt: &1", STRING(vlReadReceipt)), "INFO"). 
        END. 
		
		IF vcEmailServer > "" THEN 
		DO:  
		    // Create SmtpClient instance 
			ASSIGN oSmtpClient = NEW SmtpClient(vcEmailServer, viEmailPort).  
			
			// Create MailMessage instance 
			ASSIGN 
				oEmailMsg 		     = NEW MailMessage(vcFrom, vcTo, vcSubject, vcBodyText).
				oEmailMsg:IsBodyHtml = vlIsBodyHtml. 
			
			// Specify read receipt settings	
			IF vlReadReceipt = TRUE THEN 
                oEmailMsg:Headers:Add("Disposition-Notification-To", vcFrom).			
			
			// Specify ReplyTo 	
			IF vcReplyTo <> "":U AND vcReplyTo <> ? THEN  
				oEmailMsg:ReplyToList:Add(vcReplyTo).  
			
			//Specify Sender 
			IF vcSender <> "":U AND vcSender <> ? THEN  
				oEmailMsg:Sender = NEW MailAddress(vcSender).  
			
			//Specify CC
			IF vcCC <> "":U AND vcCC <> ? THEN 	
				oEmailMsg:CC:Add(vcCC).   
			
			//Specify BCC
			IF vcBCC <> "":U AND vcBCC <> ? THEN 			
				oEmailMsg:BCC:Add(vcBCC).  					
			
			// Specify Attachments and/or Attachment Names
			IF vcAttachmentName = "":U OR vcAttachmentName = ? THEN 
			DO viCount = 1 TO NUM-ENTRIES(vcAttachment):  
				ASSIGN vcAttachUse = ENTRY(viCount, vcAttachment).  
				oEmailMsg:Attachments:Add(NEW System.Net.Mail.Attachment(vcAttachUse)).
			END. /* DO viCount = 1 TO NUM-ENTRIES(vcAttachment): */
			ELSE
			DO viCount = 1 TO NUM-ENTRIES(vcAttachment):  
				ASSIGN vcAttachUse = ENTRY(viCount, vcAttachment).   
				oAttachment = NEW System.Net.Mail.Attachment(vcAttachUse). 			
				IF vcAttachmentName <> "" AND vcAttachmentName <> ? AND NUM-ENTRIES(vcAttachmentName) >= viCount THEN
					oAttachment:NAME = ENTRY(viCount, vcAttachmentName).
				oEmailMsg:Attachments:Add(oAttachment).
			END. /* DO viCount = 1 TO NUM-ENTRIES(vcAttachment):  */  
			
		//	&message "RG3"

			//message "11:DeliveryFormat: " oSmtpClient:DeliveryFormat  "DeliveryMethod: " oSmtpClient:DeliveryMethod
			// view-as alert-box.			
			 
			oSmtpClient:DeliveryFormat = SmtpDeliveryFormat:International.
			 
			// message "2:DeliveryFormat: " oSmtpClient:DeliveryFormat  "DeliveryMethod: " oSmtpClient:DeliveryMethod
			// view-as alert-box.
			// Send email
//			oSmtpClient:Send(oEmailMsg). 		          
			
		END. /* IF vcEmailServer > "" THEN  */
		ELSE
			ASSIGN vcError = "Received invalid EmailServer settings.".    
        
        IF LOG-MANAGER:LOGFILE-NAME > "" THEN
        DO: 
            IF vcError <> "":U THEN
                LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("SendEmail - vcError: &1", vcError), "ERROR"). 
        END. 
            
        RETURN SerializeJson(). 
        
        CATCH e AS "Progress.Lang.Error":

            DEFINE VARIABLE viErrIdx AS INTEGER     NO-UNDO.
            
            ASSIGN vcError = "".
            DO viErrIdx = 1 TO e:NumMessages:
                IF e:GetMessage(viErrIdx) > "" THEN
                    ASSIGN vcError = vcError
                                   + (IF vcError > "" THEN "~n" ELSE "")
                                   + e:GetMessage(viErrIdx).
            END. 
            
            IF LOG-MANAGER:LOGFILE-NAME > "" THEN
                LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("SendEmail - vcError: &1", vcError), "ERROR"). 
            
			RETURN SerializeJson(). 
	
        END CATCH.

        FINALLY:  
            
            ASSIGN vcError = "".
            
            IF VALID-OBJECT(oEmailMsg) THEN	
                DELETE OBJECT oEmailMsg. 
				
            IF VALID-OBJECT(oSmtpClient) THEN	
                DELETE OBJECT oSmtpClient.  
				
            IF VALID-OBJECT(oAttachment) THEN	
                DELETE OBJECT oAttachment.  
            
            IF VALID-OBJECT(oObjModelParser) THEN	
                DELETE OBJECT oObjModelParser. 
            
            IF VALID-OBJECT(oJsonObj) THEN	
                DELETE OBJECT oJsonObj.               
            
     /*       IF VALID-OBJECT(oJsonObject) THEN	
                DELETE OBJECT oJsonObject.         */  
            	
        END FINALLY.        
        
    END METHOD.	   
    
    /*------------------------------------------------------------------------------
     Purpose: Serailize response JSON and store it into longchar
     Notes:
    ------------------------------------------------------------------------------*/    
    METHOD STATIC PUBLIC JsonObject SerializeJson():
            
        IF LOG-MANAGER:LOGFILE-NAME > "" THEN    
            LOG-MANAGER:WRITE-MESSAGE("START SerializeJson", "INFO").
        oJsonObject = NEW JsonObject().
        IF vcError <> "":U AND vcError <> ? THEN 
            oJsonObject:ADD("ERROR", vcError).   
        
  //      oJsonObject:WRITE(vlcRetJsonObject, TRUE).
        
        IF LOG-MANAGER:LOGFILE-NAME > "" THEN
        DO:
            IF vcError <> "":U THEN
                LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("SendEmail - vlcRetJsonObject: &1" , STRING(vlcRetJsonObject)), "INFO"). 
            LOG-MANAGER:WRITE-MESSAGE("END SerializeJson", "INFO").
            LOG-MANAGER:WRITE-MESSAGE("END SendEmail", "INFO").
        END.        
        RETURN oJsonObject.    
        
    END METHOD.      
	
END CLASS.
