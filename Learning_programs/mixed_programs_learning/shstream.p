DEFINE NEW SHARED BUFFER xrep FOR salesrep.
DEFINE NEW SHARED STREAM phonelist.
OUTPUT STREAM phonelist TO phonefile.
PAUSE 2 BEFORE-HIDE.
FOR EACH xrep:
DISPLAY xrep WITH FRAME repname
TITLE "Creating report for " 2 COLUMNS CENTERED ROW 10.
DISPLAY STREAM phonelist xrep WITH 2 COLUMNS STREAM-IO.
RUN dispstream.p.
END.
