
/*------------------------------------------------------------------------
    File        : Common/v1/Service/serviceBECatch.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : JAlcazar
    Created     : Thu Jun 25 13:59:32 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/
  CATCH appErr AS Progress.Lang.AppError:
      UNDO, THROW appErr.
  END CATCH.    
  CATCH err AS Progress.Lang.Error:
      MESSAGE SUBSTITUTE("BE Error: &1 &2(&3).", PROGRAM-NAME(1), err:GetMessage(1), err:GetMessageNum(1)).
      UNDO, THROW err.
  END CATCH.
  