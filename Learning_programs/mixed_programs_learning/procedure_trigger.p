DEFINE VARIABLE phandle AS HANDLE.
phandle  = THIS-PROCEDURE.
ON RETURN OF phandle MESSAGE "Hii".  //procedure trigger execution when "return" event is applied.
APPLY "RETURN" TO phandle.        //applying "return" event to procedure-handle
