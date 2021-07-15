FUNCTION SetPartName RETURNS INTEGER (INPUT a AS CHARACTER) FORWARD.
DEFINE VARIABLE h AS HANDLE.
DEFINE VARIABLE localPartName AS CHARACTER.
/* Add a super procedure */
RUN D:\vaishali\BlockProperties\super_func\driver_super.p PERSISTENT SET h.
THIS-PROCEDURE:ADD-SUPER-PROCEDURE (h).
SetPartName("2000 Calendar").

PROCEDURE sample1:
    DEFINE INPUT-OUTPUT PARAMETER a AS CHARACTER.
    a = a + "proc: Part name is: ".
    /* Invoke procedure sample1 in the super procedure. */
    RUN SUPER (INPUT-OUTPUT a).
END PROCEDURE.

FUNCTION sample2 RETURNS CHARACTER (INPUT-OUTPUT a AS CHARACTER).
    a = a + "func: Part name is: ".
    /* Invoke function sample2 in the super procedure. */
    SUPER (INPUT-OUTPUT a).
    RETURN a.
END FUNCTION.

FUNCTION GetPartName RETURNS CHARACTER ():
    RETURN localPartName.
END FUNCTION.

FUNCTION SetPartName RETURNS INTEGER (INPUT partname AS CHARACTER):
    localPartName = partname.
END FUNCTION.
