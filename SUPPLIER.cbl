       IDENTIFICATION DIVISION.
      ****************************************************************
      **   THIS SUBPROGRAM PERFORMS THE VALIDATIONS ON THE SUPPLIER'S
      **   FIELDS TO SEND THEM TO THE MAIN PROGRAM OR, IF THERE ARE
      **   ERRORS, INFORM THEM
      ****************************************************************
       PROGRAM-ID. SUPPLIER.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       77  ERR-COUNTER            PIC 9(1) VALUE ZERO.
       77 TABLE-MAX               PIC 9(01) VALUE 5.
       77 PROJ-IDX                PIC 9(01) VALUE ZERO.

       01  SUPPLIERS-FIELDS.
           05 WS-SUPPLIER-CODE     PIC X(10) VALUE SPACES.
           05 WS-SUPPLIER-TYPE     PIC X(01) VALUE SPACES.
           05 WS-SUPPLIER-NAME     PIC X(15) VALUE SPACES.
           05 WS-SUPPLIER-PERF     PIC 9(03) VALUE ZERO.
           05 WS-SUPPLIER-RATING   PIC X(01) VALUE SPACES.
           05 WS-SUPPLIER-STATUS   PIC X(01) VALUE SPACES.
           05 WS-SUPPLIER-ACT-DATE PIC 9(08) VALUE ZERO.
       01 WS-PICSTR-IN.
          10 WS-PICSTR-LTH-IN     PIC S9(4) COMP VALUE 8.
          10 WS-PICSTR-LTH-IN     PIC X(8)  VALUE 'YYYYMMDD'.
       01 WS-DATE-IN-CEE.
          10 WS-DATE-IN-LTH-CEE   PIC S9(4) COMP VALUE 8.
          10 WS-DATE-IN-STR-CEE   PIC X(8).
       01 WS-FC.
          10 FC-SEV               PIC S9(4) COMP.
          10 FC-MSG               PIC S9(4) COMP.
          10 FC-CTW               PIC X.
          10 FC-FAC               PIC X(3).
          10 FC-ISI               PIC S9(8) COMP.
       LINKAGE SECTION.
       01 LS-SUPPLIERS.
              10  LS-SUPPLIER-CODE     PIC X(10).
              10  LS-SUPPLIER-TYPE     PIC X(01).
              10  LS-SUPPLIER-NAME     PIC X(15).
              10  LS-SUPPLIER-PERF     PIC 9(03).
              10  LS-SUPPLIER-RATING   PIC X(01).
              10  LS-SUPPLIER-STATUS   PIC X(01).
              10  LS-SUPPLIER-ACT-DATE PIC 9(08).
       01 LS-SUPPLIER-RTN-CODE    PIC 9(01) VALUE ZERO.
       COPY ERRORSUB REPLACING ==(PRFX)== BY ==LS-SUPPLIER==.

       PROCEDURE DIVISION USING LS-SUPPLIERS,
                                LS-SUPPLIER-RTN-CODE,
                                LS-SUPPLIER-ERROR-TBL,
                                LS-SUPPLIER-ERROR-NUM.

           PERFORM 100-HOUSEKEEPING.
           PERFORM 200-DATACHECK.


       100-HOUSEKEEPING.
            *> VARIABLES INITIALIZATION
            MOVE ZERO TO LS-SUPPLIER-RTN-CODE.
            MOVE ZERO TO LS-SUPPLIER-ERROR-NUM.
            MOVE ZERO TO  ERR-COUNTER.
            INITIALIZE LS-SUPPLIER-ERROR-TBL.

       200-DATACHECK.
            *> CHEK OF EACH FIELD OF THE REGISTER
            *> NOT EMPTY CODE
            IF NOT ((LS-SUPPLIER-CODE = LOW-VALUE)  OR
                    (LS-SUPPLIER-CODE = SPACES)) THEN
               MOVE LS-SUPPLIER-CODE TO WS-SUPPLIER-CODE
            ELSE
               *> ERROR HANDLING
               ADD +1 TO ERR-COUNTER
               MOVE 8 TO LS-SUPPLIER-RTN-CODE
               MOVE "S001E" TO MSG-NO(ERR-COUNTER)
               MOVE "SUPPLIER-CODE can not be empty." TO
                      MSG-TEXT(ERR-COUNTER)
            END-IF.
            *> IF SUBCONTRACTOR, SUPPLIER TYPE MUST BE 3
            EVALUATE LS-SUPPLIER-TYPE
                WHEN "S"
                  EVALUATE LS-SUPPLIER-RATING
                    WHEN "3"
                      MOVE LS-SUPPLIER-TYPE TO WS-SUPPLIER-TYPE
                    WHEN  OTHER
                    *> ERROR HANDLING
                     ADD +1 TO ERR-COUNTER
                     MOVE 8 TO LS-SUPPLIER-RTN-CODE
                     MOVE "S003E" TO MSG-NO(ERR-COUNTER)
                     MOVE "S-Rating for S-Type S has to be 3" TO
                           MSG-TEXT(ERR-COUNTER)
                  END-EVALUATE
                WHEN "D" MOVE LS-SUPPLIER-TYPE TO WS-SUPPLIER-TYPE
                WHEN "M" MOVE LS-SUPPLIER-TYPE TO WS-SUPPLIER-TYPE
                WHEN "I" MOVE LS-SUPPLIER-TYPE TO WS-SUPPLIER-TYPE
                WHEN OTHER
                *> ERROR HANDLING
                     ADD +1 TO ERR-COUNTER
                      MOVE 8 TO LS-SUPPLIER-RTN-CODE
                      MOVE "S002E" TO MSG-NO(ERR-COUNTER)
                      MOVE "SUPPLIER-TYPE has to be S,D,M or I." TO
                            MSG-TEXT(ERR-COUNTER)

           END-EVALUATE.
            *> NOT EMPTY NAME
            IF NOT (LS-SUPPLIER-NAME = SPACE OR LS-SUPPLIER-NAME =
                LOW-VALUE) THEN  *> not empty
               MOVE LS-SUPPLIER-NAME TO WS-SUPPLIER-NAME
            ELSE
            *> ERROR HANDLING
               ADD +1 TO ERR-COUNTER
               MOVE 8 TO LS-SUPPLIER-RTN-CODE
               IF ERR-COUNTER < 4
                 MOVE  "S004E" TO MSG-NO(ERR-COUNTER)
                 MOVE  "SUPPLIER-NAME can not be empty."
                        TO MSG-TEXT(ERR-COUNTER)
               ELSE
                 MOVE  "S099E" TO MSG-NO(ERR-COUNTER)
                 MOVE  "More than 3 fields have errors."
                       TO MSG-TEXT(ERR-COUNTER)
                END-IF

            END-IF.

            *> NOT EMPTY PERF
            IF NOT (LS-SUPPLIER-PERF = SPACE OR LS-SUPPLIER-PERF =
               LOW-VALUE) THEN  *> not empty
               IF (LS-SUPPLIER-PERF IS NUMERIC) THEN  *> is numeric
                 MOVE LS-SUPPLIER-PERF TO WS-SUPPLIER-PERF
               ELSE
               *> ERROR HANDLING
                 ADD +1 TO ERR-COUNTER
                 MOVE 8 TO LS-SUPPLIER-RTN-CODE
                 IF ERR-COUNTER < 4
                   MOVE  "S006E" TO MSG-NO(ERR-COUNTER)
                   MOVE  "SUPPLIER-PERF has to be a number"
                        TO MSG-TEXT(ERR-COUNTER)
                 ELSE
                  IF ERR-COUNTER = 4 THEN
                   MOVE  "S099E" TO MSG-NO(ERR-COUNTER)
                   MOVE  "More than 3 fields have errors."
                       TO MSG-TEXT(ERR-COUNTER)
                   END-IF
                 END-IF
               END-IF
            ELSE
            *> ERROR HANDLING
               ADD +1 TO ERR-COUNTER
               MOVE 8 TO LS-SUPPLIER-RTN-CODE
               IF ERR-COUNTER < 4
                 MOVE  "S005E" TO MSG-NO(ERR-COUNTER)
                 MOVE  "SUPPLIER-PERF can not be empty."
                        TO MSG-TEXT(ERR-COUNTER)
               ELSE
                 IF ERR-COUNTER = 4 THEN
                    MOVE  "S099E" TO MSG-NO(ERR-COUNTER)
                    MOVE  "More than 3 fields have errors."
                         TO MSG-TEXT(ERR-COUNTER)
                 END-IF
                END-IF
            END-IF.

            *> RATING EVALUATION
            EVALUATE LS-SUPPLIER-RATING
                WHEN "3" MOVE LS-SUPPLIER-RATING  TO WS-SUPPLIER-RATING
                WHEN "2" MOVE LS-SUPPLIER-RATING TO WS-SUPPLIER-RATING
                WHEN "1" MOVE LS-SUPPLIER-RATING TO WS-SUPPLIER-RATING
                WHEN OTHER
                *> ERROR HANDLING
                     ADD +1 TO ERR-COUNTER
                     MOVE 8 TO LS-SUPPLIER-RTN-CODE
                     IF ERR-COUNTER < 4
                       MOVE  "S007E" TO MSG-NO(ERR-COUNTER)
                       MOVE
                       "SUPPLIER-RATING can only contain 1,2 or 3."
                              TO MSG-TEXT(ERR-COUNTER)
                     ELSE
                       IF ERR-COUNTER = 4 THEN
                          MOVE  "S099E" TO MSG-NO(ERR-COUNTER)
                          MOVE  "More than 3 fields have errors."
                               TO MSG-TEXT(ERR-COUNTER)
                       END-IF
                     END-IF
           END-EVALUATE.
            *> STATUS EVALUATION
            EVALUATE LS-SUPPLIER-STATUS
                WHEN "1" MOVE LS-SUPPLIER-STATUS TO WS-SUPPLIER-STATUS
                WHEN "2" MOVE LS-SUPPLIER-STATUS TO WS-SUPPLIER-STATUS
                WHEN "3" MOVE LS-SUPPLIER-STATUS TO WS-SUPPLIER-STATUS
                WHEN OTHER
                *> ERROR HANDLING
                     ADD +1 TO ERR-COUNTER
                     MOVE 8 TO LS-SUPPLIER-RTN-CODE
                     IF ERR-COUNTER < 4
                       MOVE  "S008E" TO MSG-NO(ERR-COUNTER)
                       MOVE
                       "SUPPLIER-STATUS can only contain 1,2 or 3."
                              TO MSG-TEXT(ERR-COUNTER)
                     ELSE
                       IF ERR-COUNTER = 4 THEN
                          MOVE  "S099E" TO MSG-NO(ERR-COUNTER)
                          MOVE  "More than 3 fields have errors."
                               TO MSG-TEXT(ERR-COUNTER)
                       END-IF
                     END-IF
           END-EVALUATE.

            *> ACT-DATE VALIDATION
            MOVE LS-SUPPLIER-ACT-DATE TO WS-DATE-IN-STR-CEE
            CALL 'CEEDAYS' USING WS-DATE-IN-CEE
                WS-PICSTR-IN, WS-SUPPLIER-ACT-DATE, WS-FC
            IF FC-SEV = ZERO
               MOVE LS-SUPPLIER-ACT-DATE TO WS-SUPPLIER-ACT-DATE
            ELSE
            *> ERROR HANDLING
               ADD +1 TO ERR-COUNTER
               MOVE 8 TO LS-SUPPLIER-RTN-CODE
               IF ERR-COUNTER < 4
                 MOVE  "S009E" TO MSG-NO(ERR-COUNTER)
                 MOVE
                 "Invalid Date format in  SUPPLIER-ACT-DATE"
                        TO MSG-TEXT(ERR-COUNTER)
               ELSE
                 IF ERR-COUNTER = 4 THEN
                    MOVE  "S099E" TO MSG-NO(ERR-COUNTER)
                    MOVE  "More than 3 fields have errors."
                         TO MSG-TEXT(ERR-COUNTER)
                 END-IF
               END-IF
            END-IF.
           MOVE ERR-COUNTER TO LS-SUPPLIER-ERROR-NUM .
           GOBACK.

