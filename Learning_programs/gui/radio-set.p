DEFINE VARIABLE hist-date AS DATE NO-UNDO
  FORMAT "99/99/9999" INITIAL 07/04/1776
  VIEW-AS RADIO-SET RADIO-BUTTONS 
    "Declaration of Independence", 07/04/1776,
    "Lee Surrenders to Grant", 04/07/1865,
    "Man Walks on Moon", 07/11/1969.

FORM hist-date
  WITH FRAME main-frame NO-LABELS TITLE "Dates in US History".

ON VALUE-CHANGED OF hist-date DO:
  ASSIGN hist-date.
  DISPLAY "This event occurred on " + STRING(hist-date) FORMAT "x(60)"
    WITH FRAME main-frame.
END.

ENABLE hist-date WITH FRAME main-frame.
APPLY "VALUE-CHANGED" TO hist-date.
WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
