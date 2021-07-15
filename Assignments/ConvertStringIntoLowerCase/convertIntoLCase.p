/******Defining Variables*************************/
DEFINE VAR upperStr AS CHAR FORMAT "x(16)" NO-UNDO.

/****Input string from user****/
SET upperStr.

/****Converts String into lowercase*****/
DISP LC(upperStr) FORMAT "x(16)".
