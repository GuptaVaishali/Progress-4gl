    /*------------------------------------------------------------------------
    File        : JsonBuilder
    Purpose     : BuildJson takes inputs in the form of paramaters and validate these 
				  in context of sending email. If validation results in error then
				  it returns error in the below format
				  {
					   "ERROR": "string"
				  }				  
				  Once validated successfully, JSON is constructed out of the inputted parameter values as below:
				  {
    				  "EmailServer": "string",
    				  "EmailPort": integer,
    				  "EmailTo": "string,string",
    				  "EmailFrom": "string",
    				  "ReplyTo": "string,string",
    				  "EmailSender": "string",
    				  "EmailCC": "string,string",
    				  "EmailBCC": "string,string",
    				  "EmailSubject": "string",
    				  "EmailBody": "string",  
                      "IsBodyHtml": boolean, 
    				  "Attachments": "string,string",
    				  "AttachmentNames": "string,string",
    				  "ReadReceipt": boolean 
				  } 
    Syntax      :   
    Description : Validates Emaill Setting Inputs and Generates Json object
    Author(s)   : Ruchi Gupta
    Created     : 06/29/2021 11:12:15 AM 
    Notes       :
        
    MODIFICATION LOG:
        initials - mm/dd/yyyy - RTB # - description
             RG  - 06/29/2011 - XXXXX - Created. 
  ----------------------------------------------------------------------*/ 
  
USING Progress.Lang.*.
USING Progress.Json.ObjectModel.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

    FUNCTION isNullOrBlank RETURNS CHARACTER(
        INPUT        ipcEntity     			AS CHARACTER,
        INPUT-OUTPUT ipcEntityValue  		AS CHARACTER) FORWARD.
        
    FUNCTION isNullOrZero RETURNS CHARACTER ( 
        INPUT-OUTPUT ipiEmailPort AS INTEGER) FORWARD.
        
    FUNCTION isNullOrFalse RETURNS CHARACTER ( 
        INPUT-OUTPUT iplIsBodyHtml AS LOGICAL) FORWARD.
        
    FUNCTION SerializeJson RETURNS JsonObject() FORWARD.

    DEFINE VARIABLE vcClsName         AS CHARACTER        NO-UNDO INITIAL "JsonBuilder".
    DEFINE VARIABLE vcVersion         AS CHARACTER        NO-UNDO INITIAL "10000".
    DEFINE VARIABLE vcError           AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE vlcRetJsonObject  AS LONGCHAR         NO-UNDO.
    DEFINE VARIABLE oJsonObject       AS JsonObject       NO-UNDO.   
    DEFINE VARIABLE oEmailServer      AS CoreEmailServer  NO-UNDO. 
    
    
    /*------------------------------------------------------------------------------
     Purpose: Returns either error encountered during validation or JSON formatted
              inputted parameters. 
     Notes:
    ------------------------------------------------------------------------------*/ 
    
    FUNCTION BuildJson RETURNS JsonObject 
        (INPUT ipcEmailServer 		AS CHARACTER,  
         INPUT ipiEmailPort   		AS INTEGER,   
         INPUT ipcTo         		AS CHARACTER,
         INPUT ipcFrom       		AS CHARACTER,  
         INPUT ipcReplyTo    		AS CHARACTER,   
         INPUT ipcSender     		AS CHARACTER,  
         INPUT ipcCC         		AS CHARACTER, 
         INPUT ipcBCC        		AS CHARACTER, 
         INPUT ipcSubject     		AS CHARACTER,
         INPUT ipcBodyText    		AS CHARACTER,
         INPUT iplIsBodyHtml		AS LOGICAL,   
         INPUT ipcAttachment  		AS CHARACTER,  
         INPUT ipcAttachmentName  	AS CHARACTER,
		 INPUT iplReadReceipt 		AS LOGICAL):
         
         
        IF LOG-MANAGER:LOGFILE-NAME > "" THEN
        DO:
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("System Log for &1; Version &2", vcClsName, vcVersion)).
            LOG-MANAGER:WRITE-MESSAGE("JsonBuilder - START BuildJson", "INFO"). 
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - BuildJson - ipcEmailServer: &1", ipcEmailServer), "INFO").
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - BuildJson - ipiEmailPort: &1", ipiEmailPort), "INFO").
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - BuildJson - ipcTo: &1", ipcTo), "INFO").
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - BuildJson - ipcFrom: &1", ipcFrom), "INFO").
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - BuildJson - ipcReplyTo: &1", ipcReplyTo), "INFO").
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - BuildJson - ipcSender: &1", ipcSender), "INFO").
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - BuildJson - ipcCC: &1", ipcCC), "INFO").
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - BuildJson - ipcBCC: &1", ipcBCC), "INFO").
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - BuildJson - ipcSubject: &1", ipcSubject), "INFO").
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - BuildJson - ipcBodyText: &1", ipcBodyText), "INFO").
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - BuildJson - iplIsBodyHtml: &1", iplIsBodyHtml), "INFO").
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - BuildJson - ipcAttachment: &1", ipcAttachment), "INFO").
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - BuildJson - ipcAttachmentName: &1", ipcAttachmentName), "INFO").
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - BuildJson - iplReadReceipt: &1", iplReadReceipt), "INFO").
        END.    
        
         oJsonObject = NEW JsonObject().
        
        /* Validate Blank or ? inputted as ipcEmailServer and
		read its value from DB in such a case*/
        vcError = isNullOrBlank(
				      INPUT "EmailServer":U,
				      INPUT-OUTPUT ipcEmailServer).
        

		IF vcError <> "":U THEN
		DO:
		    IF LOG-MANAGER:LOGFILE-NAME > "" THEN
		        LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - BuildJson - vcError: &1", vcError), "ERROR").
            RETURN SerializeJson().
		END.

     
        /* Validate Blank or 0 inputted as ipiEmailPort and
        read its value from DB in such a case */
        vcError = isNullOrZero(
				      INPUT-OUTPUT ipiEmailPort).
		IF vcError <> "":U THEN
		DO:
		    IF LOG-MANAGER:LOGFILE-NAME > "" THEN
                LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - BuildJson - vcError: &1", vcError), "ERROR").
            RETURN SerializeJson().
        END.
        /* Validate Blank or False inputted as iplIsBodyHtml and
        read its value from DB in such a case */
		vcError = isNullOrFalse(
				      INPUT-OUTPUT iplIsBodyHtml).

		IF vcError <> "":U THEN
		DO:
		    IF LOG-MANAGER:LOGFILE-NAME > "" THEN
                LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - BuildJson - vcError: &1", vcError), "ERROR").
            RETURN SerializeJson().
        END.

		/* Validate Blank or ? inputted as ipcTo */
		vcError = isNullOrBlank(
				      INPUT "TO":U,
				      INPUT-OUTPUT ipcTo).

		IF vcError <> "":U THEN
		DO:
		    IF LOG-MANAGER:LOGFILE-NAME > "" THEN
                LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - BuildJson - vcError: &1", vcError), "ERROR").
            RETURN SerializeJson().
        END.

		/* Validate Blank or ? inputted as ipcFrom */
		vcError = isNullOrBlank(
				      INPUT "FROM":U,
				      INPUT-OUTPUT ipcFrom).

		IF vcError <> "":U THEN
		DO:
		    IF LOG-MANAGER:LOGFILE-NAME > "" THEN
                LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - BuildJson - vcError: &1", vcError), "ERROR").
            RETURN SerializeJson().
		END.

		/* If entries are delimited by any special character other than ",",
		  swap the delimiter by ","*/
		IF INDEX(ipcTo, ";") > 0 THEN
			 RUN swapDelimiter(
				 INPUT ";",
				 INPUT-OUTPUT ipcTo).
		ELSE
		IF INDEX(ipcTo, "^") > 0 THEN
			 RUN swapDelimiter(
				 INPUT "^",
				 INPUT-OUTPUT ipcTo).

		IF INDEX(ipcReplyTo, ";") > 0 THEN
			 RUN swapDelimiter(
				 INPUT ";",
				 INPUT-OUTPUT ipcReplyTo).
		ELSE
		IF INDEX(ipcReplyTo, "^") > 0 THEN
			 RUN swapDelimiter(
				 INPUT "^",
				 INPUT-OUTPUT ipcReplyTo).

		IF INDEX(ipcCC, ";") > 0 THEN
			 RUN swapDelimiter(
				 INPUT ";",
				 INPUT-OUTPUT ipcCC).
		ELSE
		IF INDEX(ipcCC, "^") > 0 THEN
			 RUN swapDelimiter(
				 INPUT "^",
				 INPUT-OUTPUT ipcCC).

		IF INDEX(ipcBCC, ";") > 0 THEN
			 RUN swapDelimiter(
				 INPUT ";",
				 INPUT-OUTPUT ipcBCC).
		ELSE
		IF INDEX(ipcBCC, "^") > 0 THEN
			 RUN swapDelimiter(
				 INPUT "^",
				 INPUT-OUTPUT ipcBCC).

		IF INDEX(ipcAttachment, ";") > 0 THEN
			 RUN swapDelimiter(
				 INPUT ";",
				 INPUT-OUTPUT ipcAttachment).
		ELSE
		IF INDEX(ipcAttachment, "^") > 0 THEN
			 RUN swapDelimiter(
				 INPUT "^",
				 INPUT-OUTPUT ipcAttachment).

		IF INDEX(ipcAttachmentName, ";") > 0 THEN
			 RUN swapDelimiter(
				 INPUT ";",
				 INPUT-OUTPUT ipcAttachmentName).
		ELSE
		IF INDEX(ipcAttachmentName, "^") > 0 THEN
			 RUN swapDelimiter(
				 INPUT "^",
				 INPUT-OUTPUT ipcAttachmentName).

        ASSIGN ipcTo                  =  TRIM(ipcTo, ",")
               ipcReplyTo             =  TRIM(ipcReplyTo, ",")
               ipcCC                  =  TRIM(ipcCC, ",")
               ipcBCC                 =  TRIM(ipcBCC, ",")
               ipcAttachment          =  TRIM(ipcAttachment, ",")
               ipcAttachmentName      =  TRIM(ipcAttachmentName, ",").

        IF ipcAttachmentName <> "":U AND ipcAttachmentName <> ?
         AND NUM-ENTRIES(ipcAttachment) <> NUM-ENTRIES(ipcAttachmentName) THEN
        DO:
            vcError = "Attachment files and their symbolic names lists should have equal number of entries.".
            IF LOG-MANAGER:LOGFILE-NAME > "" THEN
                LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - BuildJson - vcError: &1", vcError), "ERROR").
            RETURN SerializeJson().
        END. /*  IF NUM-ENTRIES(ipcAttachment) <> NUM-ENTRIES(ipcAttachmentName) THEN  */
        
        oJsonObject:ADD("EmailServer", ipcEmailServer).
        oJsonObject:ADD("EmailPort", ipiEmailPort).  
        oJsonObject:ADD("EmailTo", ipcTo).      
        oJsonObject:ADD("EmailFrom", ipcFrom).  
        oJsonObject:ADD("ReplyTo", ipcReplyTo).  
        oJsonObject:ADD("EmailSender", ipcSender).  
        oJsonObject:ADD("EmailCC", ipcCC).  
        oJsonObject:ADD("EmailBCC", ipcBCC).  
        oJsonObject:ADD("EmailSubject", ipcSubject).  
        oJsonObject:ADD("EmailBody", ipcBodyText).    
        oJsonObject:ADD("IsBodyHtml", iplIsBodyHtml).  
        oJsonObject:ADD("Attachments", ipcAttachment).
        oJsonObject:ADD("AttachmentNames", ipcAttachmentName).   
        oJsonObject:ADD("ReadReceipt", iplReadReceipt).   
        
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
                LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - BuildJson - vcError: &1", vcError), "ERROR").
                
            RETURN SerializeJson().
	
        END CATCH.                                

      FINALLY:   
        
            ASSIGN vcError = "".
            
            IF VALID-OBJECT(oEmailServer) THEN	
                DELETE OBJECT oEmailServer. 
                
        //    IF VALID-OBJECT(oJsonObject) THEN	
         //       DELETE OBJECT oJsonObject. 
        END FINALLY.    
        
    END FUNCTION.
         
    /*------------------------------------------------------------------------------
     Purpose: Swaps ";"/"^" delimiters with ","
     Notes:
    ------------------------------------------------------------------------------*/      
     PROCEDURE swapDelimiter:
        DEFINE INPUT        PARAMETER ipcCurrDelimiter AS CHARACTER.
        DEFINE INPUT-OUTPUT PARAMETER ipcList          AS CHARACTER.
		
            
		IF LOG-MANAGER:LOGFILE-NAME > "" THEN
		DO:
            LOG-MANAGER:WRITE-MESSAGE("JsonBuilder - START swapDelimiter", "INFO"). 
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - swapDelimiter - ipcCurrDelimiter: &1", ipcCurrDelimiter), "INFO"). 
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - swapDelimiter - ipcList: &1", ipcList), "INFO").
        END.
            
		ipcList = REPLACE(ipcList, ipcCurrDelimiter, ",":U).	
		
		IF LOG-MANAGER:LOGFILE-NAME > "" THEN
		DO:
		    LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - swapDelimiter - ipcList after swapping delimiter: &1", ipcList), "INFO").	 
		    LOG-MANAGER:WRITE-MESSAGE("JsonBuilder - END swapDelimiter", "INFO"). 
		END.      
	 END PROCEDURE. 
     
     /*------------------------------------------------------------------------------
     Purpose: Validates null or blank values for EmailServer/To/From 
     Notes:
    ------------------------------------------------------------------------------*/ 	
    FUNCTION isNullOrBlank RETURNS CHARACTER.    
        DEFINE VARIABLE vcLocalError AS CHARACTER   NO-UNDO.
		
		IF LOG-MANAGER:LOGFILE-NAME > "" THEN
        DO:
            LOG-MANAGER:WRITE-MESSAGE("JsonBuilder - START isNullOrBlank", "INFO"). 
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - isNullOrBlank - ipcEntity: &1", ipcEntity), "INFO"). 
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - isNullOrBlank - ipcEntityValue: &1", ipcEntityValue), "INFO").
        END.
        
		CASE ipcEntity:
			WHEN "EmailServer":U THEN 
				IF ipcEntityValue = "":U OR ipcEntityValue = ? THEN 
				DO:        
					IF NOT VALID-OBJECT(oEmailServer) THEN
                    DO:
                        oEmailServer = CoreEmailServer:getSingleInst(INPUT NO).
                    END.
						  
                        
					IF NOT VALID-OBJECT(oEmailServer) THEN	
					DO:     
						ASSIGN vcLocalError = "Error occured while instantiating CoreEmailServer class instance to retrieve EmailServer settings.".
						
						IF LOG-MANAGER:LOGFILE-NAME > "" THEN
                            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - isNullOrBlank - vcLocalError: &1", vcLocalError), "ERROR").
						
						RETURN vcLocalError.
					END. /* IF NOT VALID-OBJECT(oEmailServer) THEN */
					
					ipcEntityValue = CoreEmailServer:EmailServer.  
            
                    IF LOG-MANAGER:LOGFILE-NAME > "" THEN
                        LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - isNullOrBlank - ipcEntityValue: &1", ipcEntityValue), "INFO").
                            
					IF ipcEntityValue = "":U OR ipcEntityValue = ? THEN 
					DO:
						ASSIGN vcLocalError = "Error occured while retrieving EmailServer settings from database.".
						IF LOG-MANAGER:LOGFILE-NAME > "" THEN
                            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - isNullOrBlank - vcLocalError: &1", vcLocalError), "ERROR").
						RETURN vcLocalError.
					END. /* IF NOT VALID-OBJECT(oEmailServer) THEN */
					ELSE	
						RETURN "".  
				END. /* IF ipcEntityValue = "":U OR ipcEntityValue = ? */   
			
			WHEN "TO":U THEN
			DO:  
				IF ipcEntityValue = "" OR ipcEntityValue = ? THEN
				DO:
					vcLocalError = "At least one recipient is required to send email. 'To' cannot be Empty String or Null.".
					IF LOG-MANAGER:LOGFILE-NAME > "" THEN
                        LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - isNullOrBlank - vcLocalError: &1", vcLocalError), "ERROR").
					RETURN vcLocalError. 
				END. /* IF ipcTo = "" OR ipcTo = ? THEN */ 
                ELSE
				    RETURN "":U. 				 
			END. /* WHEN "TO":U THEN */  			
			
			WHEN "FROM":U THEN
			DO:  
				IF ipcEntityValue = "" OR ipcEntityValue = ? THEN
				DO:
					vcLocalError = "At least one sender is required to send email. 'From' cannot be Empty String or Null.".
					IF LOG-MANAGER:LOGFILE-NAME > "" THEN
                        LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - isNullOrBlank - vcLocalError: &1", vcLocalError), "ERROR").
					RETURN vcLocalError. 
				END. /* IF ipcTo = "" OR ipcTo = ? THEN */ 
				RETURN "":U. 				 
			END. /* WHEN "FROM":U THEN */   
		END CASE. /* CASE ipcEntity: */
        
        IF LOG-MANAGER:LOGFILE-NAME > "" THEN
        DO:
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - isNullOrBlank - vcLocalError: &1", vcLocalError), "ERROR").
            LOG-MANAGER:WRITE-MESSAGE("JsonBuilder - END isNullOrBlank", "INFO"). 
        END.
        RETURN vcLocalError.
        
	END FUNCTION.
    
    
    
    /*------------------------------------------------------------------------------
     Purpose: Validates null or zero values for EmailPort 
     Notes:
    ------------------------------------------------------------------------------*/	
	FUNCTION isNullOrZero RETURNS CHARACTER. 
        
		DEFINE VARIABLE vcLocalError AS CHARACTER   NO-UNDO. 
		
		IF LOG-MANAGER:LOGFILE-NAME > "" THEN
        DO:
            LOG-MANAGER:WRITE-MESSAGE("JsonBuilder - START isNullOrZero", "INFO"). 
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - isNullOrZero - ipiEmailPort: &1", ipiEmailPort), "INFO"). 
        END.
         
		IF ipiEmailPort = 0 OR ipiEmailPort = ? THEN 
		DO: 
			IF NOT VALID-OBJECT(oEmailServer) THEN 
				oEmailServer = CoreEmailServer:getSingleInst(INPUT NO). 
				
			IF NOT VALID-OBJECT(oEmailServer) THEN	
			DO:
				ASSIGN vcLocalError = "Error occured while instantiating CoreEmailServer class instance to retrieve EmailPort settings.".
				IF LOG-MANAGER:LOGFILE-NAME > "" THEN
                    LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - isNullOrZero - vcLocalError: &1", vcLocalError), "ERROR").
				RETURN vcLocalError.
			END. /* IF NOT VALID-OBJECT(oEmailServer) THEN */
			
			ipiEmailPort = CoreEmailServer:EmailPort.  
    
            IF LOG-MANAGER:LOGFILE-NAME > "" THEN
                LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - isNullOrBlank - ipiEmailPort: &1", ipiEmailPort), "INFO").
                     
			IF ipiEmailPort = 0 OR ipiEmailPort = ? THEN 
			DO:
				ASSIGN vcLocalError = "Error occured while retrieving EmailPort settings from database.".
				IF LOG-MANAGER:LOGFILE-NAME > "" THEN
                    LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - isNullOrZero - vcLocalError: &1", vcLocalError), "ERROR").
				RETURN vcLocalError.
			END. /* IF NOT VALID-OBJECT(oEmailServer) THEN */
			ELSE	
				RETURN "". 						
		END. /* IF ipiEmailPort = "":U OR ipiEmailPort = ? */    
	    
	    IF LOG-MANAGER:LOGFILE-NAME > "" THEN
	    DO:
	        LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - isNullOrZero - vcLocalError: &1", vcLocalError), "ERROR").
	        LOG-MANAGER:WRITE-MESSAGE("JsonBuilder - END isNullOrZero", "INFO").
        END.
                           
        RETURN vcLocalError.
        
	END FUNCTION.
    
    
    /*------------------------------------------------------------------------------
     Purpose: Validates null or zero values for IsBodyHtml
     Notes:
    ------------------------------------------------------------------------------*/ 	
	FUNCTION isNullOrFalse RETURNS CHARACTER. 
        
		DEFINE VARIABLE vcLocalError AS CHARACTER   NO-UNDO. 
		
		IF LOG-MANAGER:LOGFILE-NAME > "" THEN
        DO:
            LOG-MANAGER:WRITE-MESSAGE("JsonBuilder - START isNullOrFalse", "INFO"). 
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - isNullOrFalse - iplIsBodyHtml: &1", iplIsBodyHtml), "INFO"). 
        END.
         
		IF iplIsBodyHtml = ? OR iplIsBodyHtml = FALSE THEN 
		DO: 
			IF NOT VALID-OBJECT(oEmailServer) THEN 
				oEmailServer = CoreEmailServer:getSingleInst(INPUT NO). 
				
			IF NOT VALID-OBJECT(oEmailServer) THEN	
			DO:
				ASSIGN vcLocalError = "Error occured while instantiating CoreEmailServer class instance to retrieve IsBodyHtml settings.".
				IF LOG-MANAGER:LOGFILE-NAME > "" THEN
                    LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - isNullOrFalse - vcLocalError: &1", vcLocalError), "ERROR").
				RETURN vcLocalError.
			END. /* IF NOT VALID-OBJECT(oEmailServer) THEN */
			
			iplIsBodyHtml = CoreEmailServer:IsBodyHtml.  

            IF LOG-MANAGER:LOGFILE-NAME > "" THEN
                LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - isNullOrFalse - iplIsBodyHtml: &1", iplIsBodyHtml), "INFO").
                
			IF iplIsBodyHtml = ? THEN 
			DO:
				ASSIGN vcLocalError = "Error occured while retrieving IsBodyHtml settings from database.".
				IF LOG-MANAGER:LOGFILE-NAME > "" THEN
                    LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - isNullOrFalse - vcLocalError: &1", vcLocalError), "ERROR").
				RETURN vcLocalError.
			END. /* IF NOT VALID-OBJECT(oEmailServer) THEN */
			ELSE	
				RETURN "". 						
		END. /* IF iplIsBodyHtml = ? OR iplIsBodyHtml = FALSE */  
        
        IF LOG-MANAGER:LOGFILE-NAME > "" THEN
        DO:
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - isNullOrFalse - vcLocalError: &1", vcLocalError), "ERROR").
            LOG-MANAGER:WRITE-MESSAGE("JsonBuilder - END isNullOrFalse", "INFO").
        END.
        
        RETURN vcLocalError.
        
	END FUNCTION. 
    
    
    /*------------------------------------------------------------------------------
     Purpose: Serialize response JSON and store it into longchar
     Notes:
    ------------------------------------------------------------------------------*/ 	
    FUNCTION SerializeJson RETURNS JsonObject():
        
        IF LOG-MANAGER:LOGFILE-NAME > "" THEN
            LOG-MANAGER:WRITE-MESSAGE("JsonBuilder - START SerializeJson", "INFO"). 
            
        IF vcError <> "":U AND vcError <> ? THEN 
            oJsonObject:ADD("ERROR", vcError).   
        
        IF LOG-MANAGER:LOGFILE-NAME > "" THEN
        DO:
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("JsonBuilder - SerializeJson - vlcRetJsonObject: &1", vlcRetJsonObject), "INFO").
            LOG-MANAGER:WRITE-MESSAGE("JsonBuilder - END SerializeJson", "INFO").                      
            LOG-MANAGER:WRITE-MESSAGE("JsonBuilder - END BuildJson", "INFO").
        END.
        
        RETURN oJsonObject.    
		
	END FUNCTION. 
    
    
    /*------------------------------------------------------------------------------
     Purpose: calling buildJson method 
     Notes:
    ------------------------------------------------------------------------------*/ 	
    
    PROCEDURE callingPro:
        
        DEFINE VARIABLE vlcEmailResponse AS LONGCHAR          NO-UNDO.
        DEFINE VARIABLE  vcEmlerror      AS CHARACTER         NO-UNDO.
        DEFINE VARIABLE oObjModelParser  AS ObjectModelParser NO-UNDO.
        DEFINE VARIABLE oJsonObj         AS JsonObject        NO-UNDO. 
        
        oJsonObj = BuildJson
                          (INPUT "",
                           INPUT 0,
                          // INPUT REPLACE(user-ctl.email-address,"^",","),
                           INPUT REPLACE("vaishali.gupta@jktech.com","^",","),
                           INPUT "SchedAdmin@dandh.com",
                           INPUT "SchedAdmin@dandh.com",
                           INPUT "SchedAdmin@dandh.com",
                           INPUT "",
                           INPUT "",
                           INPUT "Apprise® Automatic Note Reminder",
                          // INPUT note-text,
                           INPUT "vaishali",
                           INPUT NO,
                           INPUT "",
                           INPUT "", 
                           INPUT ?) NO-ERROR.
                 
            IF ERROR-STATUS:ERROR THEN
                vcEmlerror = "Error parsing Json Response".  
            ELSE IF oJsonObj:has("ERROR") THEN
                vcEmlerror = oJsonObj:GetCharacter("ERROR").
            ELSE  
            DO: 
                oJsonObj = EmailClient:SendEmail(oJsonObj).
                IF oJsonObj:has("ERROR") THEN
                    vcEmlerror = oJsonObj:GetCharacter("ERROR").
                ELSE
                    vcEmlerror = "".
            END.
            
            IF VALID-OBJECT(oJsonObject) THEN	
                DELETE OBJECT oJsonObject.
                
            IF VALID-OBJECT(oJsonObj) THEN	
                DELETE OBJECT oJsonObj.
                
      END PROCEDURE.
    
    
      RUN callingPro.
	
	
    

