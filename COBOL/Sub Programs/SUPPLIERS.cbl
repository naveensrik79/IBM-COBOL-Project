       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUPPLIERS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  SUPPLIERS-FIELDS.
           *> not  empty
           05 WS-SUPPLIER-CODE     PIC X(05) VALUE SPACES.
           *> is in (S,D,M,I) and not  empty
           05 WS-SUPPLIER-TYPE     PIC X(01) VALUE SPACES.
           *> not  empty
           05 WS-SUPPLIER-NAME     PIC X(15) VALUE SPACES.
           *> is number and not  empty
           05 WS-SUPPLIER-PERF     PIC 9(03) VALUE ZERO.
           *> is numeric an not  empty
           05 WS-SUPPLIER-RATING   PIC X(01) VALUE SPACES.
           *> is in (1,2,3)  numeric an not  empty
           05 WS-SUPPLIER-STATUS   PIC X(01) VALUE SPACES.
           *> is datum and not  empty
           05 WS-SUPPLIER-ACT-DATE PIC 9(08) VALUE ZERO.

       77  ERR-COUNTER              PIC 9(1) VALUE ZERO.


       LINKAGE SECTION.
       COPY SUPPLIERS.
       01 LS-SUPPLIER-CODE        PIC X(10).
       01 LS-SUPPLIER-TYPE        PIC X(01).
       01 LS-SUPPLIER-NAME        PIC X(15).
       01 LS-SUPPLIER-PERF        PIC 9(03).
       01 LS-SUPPLIER-RATING      PIC X(01).
       01 LS-SUPPLIER-STATUS      PIC X(01).
       01 LS-SUPPLIER-ACT-DATE    PIC 9(08).

       01 LS-PICSTR-IN.
          10 LS-PICSTR-LTH-IN     PIC S9(4) COMP VALUE 8.
          10 LS-PICSTR-LTH-IN     PIC X(8)  VALUE 'YYYYMMDD'.
       01 LS-DATE-IN-CEE.
          10 LS-DATE-IN-LTH-CEE   PIC S9(4) COMP VALUE 8.
          10 LS-DATE-IN-STR-CEE   PIC X(8).
       01 LS-FC.
          10 FC-SEV               PIC S9(4) COMP.
          10 FC-MSG               PIC S9(4) COMP.
          10 FC-CTW               PIC X.
          10 FC-FAC               PIC X(3).
          10 FC-ISI               PIC S9(8) COMP.

       01 LS-SUPPLIER-RTN-CODE    PIC X(01).

       PROCEDURE DIVISION USING LS-SUPPLIER-CODE, LS-SUPPLIER-TYPE,
                                LS-SUPPLIER-NAME, LS-SUPPLIER-PERF,
                                LS-SUPPLIER-RATING, LS-SUPPLIER-STATUS,
                                LS-SUPPLIER-ACT-DATE.

            *> CODE
            IF NOT (LS-SUPPLIER-CODE = LOW-VALUE) THEN
               INSPECT LS-SUPPLIER-CODE REPLACING SPACES BY ZEROS
               MOVE LS-SUPPLIER-CODE TO WS-SUPPLIER-CODE
            ELSE
               ADD +1 TO ERR-COUNTER
               MOVE 8 TO LS-SUPPLIER-RTN-CODE
            END-IF.

            *> TYPE
            IF NOT (LS-SUPPLIER-TYPE = SPACE OR LS-SUPPLIER-CODE =
                 LOW-VALUE) THEN  *> not empty
               IF (LS-SUPPLIER-TYPE = "S" OR LS-SUPPLIER-TYPE = "D"
                OR LS-SUPPLIER-TYPE = "M" OR LS-SUPPLIER-TYPE = "I")
                   THEN  *> defined value
                 MOVE LS-SUPPLIER-TYPE TO WS-SUPPLIER-TYPE
               ELSE
                 ADD +1 TO ERR-COUNTER
                 MOVE 8 TO LS-SUPPLIER-RTN-CODE
               END-IF
            ELSE
               ADD +1 TO ERR-COUNTER
               MOVE 8 TO LS-SUPPLIER-RTN-CODE
            END-IF.

            *> NAME
            IF NOT (LS-SUPPLIER-NAME = SPACE OR LS-SUPPLIER-NAME =
                LOW-VALUE) THEN  *> not empty
               MOVE LS-SUPPLIER-NAME TO WS-SUPPLIER-NAME
            ELSE
               ADD +1 TO ERR-COUNTER
               MOVE 8 TO LS-SUPPLIER-RTN-CODE
            END-IF.

            *> PERF
            IF NOT (LS-SUPPLIER-PERF = SPACE OR LS-SUPPLIER-PERF =
               LOW-VALUE) THEN  *> not empty
               IF (LS-SUPPLIER-PERF IS NUMERIC) THEN  *> is numeric
                 MOVE LS-SUPPLIER-PERF TO WS-SUPPLIER-PERF
               ELSE
                 ADD +1 TO ERR-COUNTER
                 MOVE 8 TO LS-SUPPLIER-RTN-CODE
               END-IF
            ELSE
               ADD +1 TO ERR-COUNTER
               MOVE 8 TO LS-SUPPLIER-RTN-CODE
            END-IF.

            *> RATING
            IF NOT (LS-SUPPLIER-RATING = SPACE OR LS-SUPPLIER-RATING
                   = LOW-VALUE) THEN  *> not empty
               IF (LS-SUPPLIER-RATING IS NUMERIC) THEN  *> is numeric
                  IF (LS-SUPPLIER-RATING = "1" OR LS-SUPPLIER-RATING =
                      "2" OR LS-SUPPLIER-RATING = "3") THEN
                     *> if subcontractor,rating must be higher quality
                     IF (LS-SUPPLIER-TYPE = "S")
                        IF (LS-SUPPLIER-RATING = "3")
                           MOVE LS-SUPPLIER-RATING TO WS-SUPPLIER-RATING
                        ELSE
                           ADD +1 TO ERR-COUNTER
                           MOVE 8 TO LS-SUPPLIER-RTN-CODE
                        END-IF
                     ELSE
                        MOVE LS-SUPPLIER-RATING TO WS-SUPPLIER-RATING
                     END-IF
                  ELSE
                     ADD +1 TO ERR-COUNTER
                     MOVE 8 TO LS-SUPPLIER-RTN-CODE
                  END-IF
                ELSE
                  ADD +1 TO ERR-COUNTER
                  MOVE 8 TO LS-SUPPLIER-RTN-CODE
                END-IF
            ELSE
               ADD +1 TO ERR-COUNTER
               MOVE 8 TO LS-SUPPLIER-RTN-CODE
            END-IF.

            *> STATUS
            IF NOT (LS-SUPPLIER-STATUS = SPACE OR LS-SUPPLIER-STATUS =
                   LOW-VALUE) THEN  *> not empty
               IF (LS-SUPPLIER-STATUS IS NUMERIC) THEN  *> is numeric
                  IF (LS-SUPPLIER-STATUS = "1" OR LS-SUPPLIER-STATUS =
                 "2" OR LS-SUPPLIER-STATUS = "3") THEN  *> defined value
                     MOVE LS-SUPPLIER-STATUS TO WS-SUPPLIER-STATUS
                  ELSE
                     ADD +1 TO ERR-COUNTER
                     MOVE 8 TO LS-SUPPLIER-RTN-CODE
                  END-IF
                ELSE
                  ADD +1 TO ERR-COUNTER
                  MOVE 8 TO LS-SUPPLIER-RTN-CODE
                END-IF
            ELSE
               ADD +1 TO ERR-COUNTER
               MOVE 8 TO LS-SUPPLIER-RTN-CODE
            END-IF.


            *> ACT-DATE
            MOVE LS-SUPPLIER-ACT-DATE TO LS-DATE-IN-STR-CEE
            CALL 'CEEDAYS' USING LS-DATE-IN-CEE
                 LS-PICSTR-IN, LS-SUPPLIER-ACT-DATE, FC
            IF FC-SEV = ZERO
               MOVE LS-SUPPLIER-ACT-DATE TO WS-SUPPLIER-ACT-DATE
            ELSE
               ADD +1 TO ERR-COUNTER
               MOVE 8 TO LS-SUPPLIER-RTN-CODE
            END-IF.

           GOBACK. 