       01 (PRFX)-ERROR-TBL.
          05 (PRFX)-ERROR-MSG OCCURS 4 TIMES INDEXED BY PO-IDX.
              10  MSG-NO         PIC X(05) VALUE SPACES.
              10  MSG-TEXT       PIC X(50) VALUE SPACES.

       01 (PRFX)-ERROR-NUM    PIC 9(01) VALUE 0.