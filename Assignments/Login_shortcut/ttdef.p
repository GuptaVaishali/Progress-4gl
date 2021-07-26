DEFINE TEMP-TABLE ttLoginTable NO-UNDO LIKE LoginTable.

DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttLoginTable.

FOR EACH LoginTable:
    CREATE ttLoginTable.
    BUFFER-COPY LoginTable TO ttLoginTable.
END.

