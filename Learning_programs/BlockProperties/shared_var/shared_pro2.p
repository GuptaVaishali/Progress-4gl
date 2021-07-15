DEFINE NEW GLOBAL SHARED VARIABLE globvar AS CHARACTER INITIAL "Global".
DEFINE SHARED VARIABLE sharedvar AS CHARACTER .
sharedvar = "changed shared".
DISPLAY sharedvar + " in shared_pro2.p." FORMAT "x(22)" WITH NO-LABELS.



