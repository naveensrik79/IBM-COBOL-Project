       IDENTIFICATION DIVISION.
      ****************************************************************
      **   THIS SUBPROGRAM PERFORMS THE VALIDATIONS ON THE SUPPLIER´S
      **   ADDRESS FIELDS TO SEND THEM TO THE MAIN PROGRAM OR,
      **   IF THERE ARE ERRORS, INFORM THEM
      ****************************************************************
       PROGRAM-ID. SUPPADDR.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       77  ERR-COUNTER            PIC 9(1) VALUE ZERO.
       77  TABLE-MAX              PIC 9(01) VALUE 5.
       77  ZIP-TABLE-MAX          PIC 9(02) VALUE 72.
       77  PROJ-IDX               PIC 9(01) VALUE ZERO.
       77  IDX                    PIC 9(02) VALUE ZERO.
       77  IDXNEXT                PIC 9(02) VALUE ZERO.
       77  MATCH                  PIC x(01) VALUE 'N'.
       77  MATCH1                 PIC x(01) VALUE 'N'.
       77  STATE-AC               PIC X(02) VALUE SPACES.
       77  ZIP-LOW                PIC X(05) VALUE SPACES.
       77  ZIP-HIGH               PIC X(05) VALUE SPACES.


       01  WS-SUPP-ADDRESS.
           05 WS-ADDRESS-TYPE     PIC X(01) VALUE SPACES.
           05 WS-ADDRESS-1        PIC X(15) VALUE SPACES.
           05 WS-ADDRESS-2        PIC X(15) VALUE SPACES.
           05 WS-ADDRESS-3        PIC X(15) VALUE SPACES.
           05 WS-CITY             PIC X(15) VALUE SPACES.
           05 WS-ADDR-STATE       PIC X(02) VALUE SPACES.
           05 ZIP-CODE            PIC X(05) VALUE SPACES.




       LINKAGE SECTION.
       01 LS-SUPP-ADDRESS.
          10 LS-ADDRESS-TYPE          PIC X(01).
          10 LS-ADDRESS-1             PIC X(15).
          10 LS-ADDRESS-2             PIC X(15).
          10 LS-ADDRESS-3             PIC X(15).
          10 LS-CITY                  PIC X(15).
          10 LS-ADDR-STATE            PIC X(02).
          10 LS-ZIP-CODE              PIC X(05).

       01 LS-SUPP-ADDRESS-RTN-CODE    PIC 9(01) VALUE ZERO.
       COPY ERRORSUB REPLACING ==(PRFX)== BY ==LS-SUPPADDR==.

       01 LS-ZIP-TBL.
          02 LS-ZIP-ROW OCCURS 72.
               10 LS-STATE         PIC X(16).
               10 LS-STATE-AC      PIC X(4).
               10 LS-ZIP-LOW       PIC X(8).
               10 LS-ZIP-HIGH      PIC X(5).



       PROCEDURE DIVISION USING LS-SUPP-ADDRESS,
                                LS-ZIP-TBL,
                                LS-SUPP-ADDRESS-RTN-CODE,
                                LS-SUPPADDR-ERROR-TBL,
                                LS-SUPPADDR-ERROR-NUM.


           PERFORM 100-HOUSEKEEPING.
           PERFORM 200-DATACHECK.



       100-HOUSEKEEPING.
            *> VARIABLES INITIALIZATION
           MOVE ZERO TO LS-SUPP-ADDRESS-RTN-CODE .
           MOVE ZERO TO LS-SUPPADDR-ERROR-NUM .
           MOVE ZERO TO  ERR-COUNTER.
           INITIALIZE LS-SUPPADDR-ERROR-TBL .



       200-DATACHECK.
           *> CHEK OF EACH FIELD OF THE REGISTER
           IF ((LS-ADDRESS-TYPE = SPACE) OR
               (LS-ADDRESS-TYPE = LOW-VALUES))
           AND
             ((LS-ADDRESS-1 = SPACES) OR (LS-ADDRESS-1 = LOW-VALUES))
           AND
             ((LS-CITY = SPACES ) OR (LS-CITY= LOW-VALUES ))
             AND
             ((LS-ZIP-CODE =  SPACES ) OR (LS-ZIP-CODE = LOW-VALUES) )
             THEN
            *> EMPTY ADDRESS
             ADD +1 TO ERR-COUNTER
             MOVE 8 TO LS-SUPP-ADDRESS-RTN-CODE
             MOVE  "SA00E" TO MSG-NO(ERR-COUNTER)
             MOVE "No ADDRESS ." TO
                  MSG-TEXT(ERR-COUNTER)
             MOVE ERR-COUNTER TO LS-SUPPADDR-ERROR-NUM
             GOBACK
           ELSE
              *> ADDRESS-TYPE EVALUATION
             EVALUATE LS-ADDRESS-TYPE
                WHEN "1" MOVE LS-ADDRESS-TYPE TO WS-ADDRESS-TYPE
                WHEN "2" MOVE LS-ADDRESS-TYPE TO WS-ADDRESS-TYPE
                WHEN "3" MOVE LS-ADDRESS-TYPE TO WS-ADDRESS-TYPE
                WHEN OTHER
                *> ERROR HANDLING
                     ADD +1 TO ERR-COUNTER
                     MOVE 8 TO LS-SUPP-ADDRESS-RTN-CODE
                     MOVE  "SA01E" TO MSG-NO(ERR-COUNTER)
                     MOVE "ADDRESS-TYPE has to be 1,2or 3." TO
                          MSG-TEXT(ERR-COUNTER)
             END-EVALUATE.

            *> ADDRESS-1
            IF NOT ((LS-ADDRESS-1 = LOW-VALUE) OR
                   (LS-ADDRESS-1 = SPACES)) THEN
               MOVE LS-ADDRESS-1 TO WS-ADDRESS-1
               MOVE LS-ADDRESS-2 TO WS-ADDRESS-2
               MOVE LS-ADDRESS-3 TO WS-ADDRESS-3
            ELSE
            *> ERROR HANDLING
               ADD +1 TO ERR-COUNTER
               MOVE 8 TO LS-SUPP-ADDRESS-RTN-CODE
               MOVE  "SA02E" TO MSG-NO(ERR-COUNTER)
               MOVE "ADDRESS-1 can not be empty  " TO
                     MSG-TEXT(ERR-COUNTER)
            END-IF.

            *> NOT EMPTY CITY
            IF NOT ((LS-CITY = LOW-VALUE) OR
                   (LS-CITY = SPACES)) THEN
               MOVE LS-CITY TO WS-CITY
            ELSE
            *> ERROR HANDLING
               ADD +1 TO ERR-COUNTER
               MOVE 8 TO LS-SUPP-ADDRESS-RTN-CODE
               MOVE  "SA02E" TO MSG-NO(ERR-COUNTER)
               MOVE "CITY can not be empty  " TO
                     MSG-TEXT(ERR-COUNTER)
            END-IF.

            *> ADDR-STATE /ZIP CODE VALIDATION
            IF NOT (((LS-ADDR-STATE = LOW-VALUE) OR
                   (LS-ADDR-STATE = SPACES)) AND
                   ((LS-ZIP-CODE = LOW-VALUE) OR
                   (LS-ZIP-CODE = SPACES))) THEN

            MOVE 'N' to MATCH.
            MOVE 'N' to MATCH1.
            PERFORM VARYING IDX FROM 1 BY 1
               UNTIL IDX >= ZIP-TABLE-MAX OR MATCH = 'Y'
               *> INDEXED SEARCH CODE PATTERN
                 IF  LS-ADDR-STATE = LS-STATE-AC(IDX)
                  IF NOT LS-ADDR-STATE  = LS-STATE-AC(IDX + 1)  THEN
                            MOVE 'Y' to MATCH
                  END-IF
                  IF LS-ZIP-CODE  >= LS-ZIP-LOW(IDX) AND
                      LS-ZIP-CODE <= LS-ZIP-HIGH(IDX) THEN
                        MOVE 'Y' to MATCH1
                        MOVE 'Y' TO MATCH
                   ELSE
                       IF NOT LS-ADDR-STATE  = LS-STATE-AC(IDX + 1)
                         *> WE HAVE NO MORE ZIPS IN STATE
                       THEN
                        MOVE 'N' to MATCH1
                       END-IF
                   END-IF
                 END-IF
               END-PERFORM

            If MATCH = 'N' AND MATCH1 = 'N'  then
               *> STATE CAN NOT  BE FOUND
                 ADD +1 TO ERR-COUNTER
                 MOVE 8 TO LS-SUPP-ADDRESS-RTN-CODE
                 IF ERR-COUNTER < 4
                    MOVE  "SA03E" TO MSG-NO(ERR-COUNTER)
                    MOVE  "Incorrect State "
                         TO MSG-TEXT(ERR-COUNTER)
                  ELSE
                    IF ERR-COUNTER = 4 THEN
                       MOVE  "SA99E" TO MSG-NO(ERR-COUNTER)
                       MOVE  "More than 3 fields have errors."
                          TO MSG-TEXT(ERR-COUNTER)
                    END-IF
                  END-IF
            ELSE
                 If MATCH =  'Y' and MATCH1 = 'N'
                    ADD +1 TO ERR-COUNTER
                    MOVE 8 TO LS-SUPP-ADDRESS-RTN-CODE
                    IF ERR-COUNTER < 4
                       MOVE  "SA04E" TO MSG-NO(ERR-COUNTER)
                       MOVE  "ZIP CODE  is  incorrect ."
                            TO MSG-TEXT(ERR-COUNTER)
                     ELSE
                       IF ERR-COUNTER = 4 THEN
                          MOVE  "SA99E" TO MSG-NO(ERR-COUNTER)
                          MOVE  "More than 3 fields have errors."
                             TO MSG-TEXT(ERR-COUNTER)
                       END-IF
                     END-IF
            END-IF.
           MOVE ERR-COUNTER TO  LS-SUPPADDR-ERROR-NUM .
           GOBACK.
