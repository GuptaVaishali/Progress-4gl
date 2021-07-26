DEFINE TEMP-TABLE ttUserRole NO-UNDO LIKE UserRole.

DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttUserRole.

FOR EACH UserRole:
    CREATE ttUserRole.
    BUFFER-COPY UserRole TO ttUserRole.
END.

