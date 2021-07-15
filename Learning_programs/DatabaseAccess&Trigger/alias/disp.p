DEFINE SHARED BUFFER mybuf FOR myalias.customer.
FOR EACH mybuf:
    DISP mybuf.
END.
