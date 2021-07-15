 
 /*------------------------------------------------------------------------
    File        : ServiceMainSI
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : JAlcazar
    Created     : Thu Jun 25 11:29:27 EDT 2020
    Notes       : 
    Updated     : 01/12/2021 - RG - V010003 - RTB Task# 15264; TFG Bug 102849; 
                                              19006_RFGuns Iteration 6 - WEB FG 9 - 
                                              An error (Out of stack space for SUBSTITUTE 
                                              processing) appears when All (F4) is clicked. 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Common.v1.Service.ServiceMainSI INHERITS Common.v1.ServiceError.ServiceError ABSTRACT: 
   
  DEFINE VARIABLE vcVersion AS CHARACTER NO-UNDO INITIAL "010001".
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
    
  CONSTRUCTOR PUBLIC ServiceMainSI (  ):
    SUPER ().
    
  END CONSTRUCTOR.

  DESTRUCTOR PUBLIC ServiceMainSI ( ):

  END DESTRUCTOR.   
                  
  /*------------------------------------------------------------------------------
   Purpose: Sometimes the response received from BE is too large to be successfully
            logged into the PASOE log file. To avoid such a case, we are taking 3000
            characters from the response longchar variable and logging it into the 
            PASOE log file.
   Notes:
  ------------------------------------------------------------------------------*/  
  METHOD PUBLIC VOID LogJsonResponse (
      INPUT iplcResponseBody AS LONGCHAR):
 
      DEFINE VARIABLE viCount AS INTEGER NO-UNDO.
      
      viCount = 1.
       
      MESSAGE "responseBody:".
       
      DO WHILE viCount < LENGTH(iplcResponseBody): 
          MESSAGE SUBSTITUTE ("&1 ", SUBSTRING(iplcResponseBody, viCount, viCount + 2999)).  
          viCount = viCount + 3000. 
          
      END. /* DO WHILE viCount < LENGTH(iplcResponseBody): */  
      
      RETURN.
    
      FINALLY:
      END FINALLY.
  END METHOD. 
  
END CLASS.
