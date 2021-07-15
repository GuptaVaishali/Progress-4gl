DEFINE VARIABLE str AS CHARACTER INITIAL "  VAiShali".
DEFINE VARIABLE a AS INTEGER INITIAL 2.
DEFINE VARIABLE i AS INTEGER INITIAL -6.
DEFINE VARIABLE ch AS CHARACTER INITIAL "B".
DEFINE VARIABLE arr AS INTEGER EXTENT 5 INITIAL [2,4,6,1,45].
DEFINE VARIABLE arr1 AS CHARACTER EXTENT 5 INITIAL ["3","7","2","8","5"].

DISP str WITH FRAME f.
DISP(SUBSTRING(str,4,-1)) WITH FRAME f1.
DISP(SUBSTRING(str,2,4)) WITH FRAME f2.
DISP(ABSOLUTE(i)) WITH FRAME f3.
DISP "lowercase" (LC(str)) WITH FRAME f4.
DISP(TRIM(str)) WITH FRAME f5.
DISP "ascii of b " CHR(ASC(ch)) WITH FRAME f6.
DISP "Ascill of B" (ASC("B")) WITH FRAME f90.
//DISP(MAXIMUM(arr)) WITH FRAME f7.
//DISP(ENTRY(a,arr[a],",")) WITH FRAME f8.
DISP(FILL("*",3)) WITH FRAME f9.
//DISP(NUM-ENTRIES(arr1)) with frame f10.

DEFINE VARIABLE regions AS CHARACTER NO-UNDO 
  INITIAL "north,south,west,Northwest,east".
DISP(NUM-ENTRIES(regions)) WITH FRAME f11.

DISP  "indexinarr func = " INDEX(regions,"south") WITH FRAME f12.       //7

DISP "index func = " INDEX("hello","l") WITH FRAME f13.
DISP "index func = " INDEX("hello","l",4) WITH FRAME f13.
DISP "index func = " INDEX("hello","m",4) WITH FRAME f14.
DISP "indexInint func = " INDEX("12345","1") WITH FRAME f15.

DISP "lookup function " LOOKUP("south",regions) WITH FRAME f16.
DISP "lookup function " LOOKUP("kljh", regions) WITH FRAME f16.

DEFINE VARIABLE datein AS DATE      NO-UNDO.
DEFINE VARIABLE daynum AS INTEGER   NO-UNDO.
DEFINE VARIABLE daynam AS CHARACTER NO-UNDO INITIAL "Sunday,
  Monday, Tuesday, Wednesday, Thursday, Friday, Saturday".

SET datein LABEL "Enter a date (mm/dd/yy)".
daynum = WEEKDAY(datein).
DISPLAY ENTRY(daynum,daynam) FORMAT "x(9)" LABEL "is a" WITH SIDE-LABELS WITH FRAME f17.







