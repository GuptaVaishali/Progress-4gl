
/*------------------------------------------------------------------------
    File        : Common/v1/Service/serviceSICatch.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : JAlcazar
    Created     : Thu Jun 25 13:59:32 EDT 2020
    Notes       :
    Updated     : 02/09/2021 - RG - V010001 - RTB Task# 15301; RFGun API Execution Time Analysis   
                                            - Updated code for Execution Time
  ----------------------------------------------------------------------*/ 
  
  CATCH appErr AS Progress.Lang.AppError:  
        viEndTime   = TIME.
        viEndTimeMS = ETIME(no). 
        MESSAGE "Time Consumption Statistics:ServiceInterface "             
                " API: " PROGRAM-NAME(1) 
                " ServiceSICatch: Progress.Lang.AppError " 
                " Start time: " STRING(viStartTime, "hh:mm:ss")  
                " End time: " STRING(viEndTime, "hh:mm:ss") 
                " Total time consumed: " viEndTime - viStartTime " Seconds " viEndTimeMS - viStartTimeMS " milliseconds ".
  
      ASSIGN 
        ResponseCode   = appErr:GetMessageNum(1)
        ResponseBody   = createError(appErr:GetMessageNum(1), appErr:GetMessage(1)).            
  END CATCH.   
  
  
  CATCH err AS Progress.Lang.Error:  
        viEndTime   = TIME.
        viEndTimeMS = ETIME(no).
        MESSAGE "Time Consumption Statistics:ServiceInterface "             
                " API: " PROGRAM-NAME(1) 
                " ServiceSICatch: Progress.Lang.Error " 
                " Start time: " STRING(viStartTime, "hh:mm:ss")  
                " End time: " STRING(viEndTime, "hh:mm:ss") 
                " Total time consumed: " viEndTime - viStartTime " Seconds " viEndTimeMS - viStartTimeMS " milliseconds ".
      
      ASSIGN 
        ResponseCode   = 500
        ResponseBody   = createError(err:GetMessageNum(1), err:GetMessage(1)).  
      MESSAGE PROGRAM-NAME(1) err:GetMessage(1).           
  END CATCH.
  
