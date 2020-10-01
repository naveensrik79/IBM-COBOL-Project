       IDENTIFICATION DIVISION.
      ****************************************************************
      **   THIS SUBPROGRAM PERFORMS THE VALIDATIONS ON THE PART'S
      **   FIELDS TO SEND THEM TO THE MAIN PROGRAM OR, IF THERE ARE
      **   ERRORS, INFORM THEM
      ****************************************************************
       PROGRAM-ID. PARTS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       77  ERR-COUNTER            PIC 9(1) VALUE ZERO.
       77 TABLE-MAX              PIC 9(01) VALUE 5.
       77 PROJ-IDX             PIC 9(01) VALUE ZERO.

       01  PARTS-FIELDS.
           05 WS-PART-NUMBER       PIC X(23) VALUE SPACES.
           05 WS-PART-NAME         PIC X(14) VALUE SPACES.
           05 WS-SPEC-NUMBER       PIC X(07) VALUE SPACES.
           05 WS-GOVT-COMML-CODE   PIC X(01) VALUE SPACES.
           05 WS-BLUEPRINT-NUMBER  PIC X(05) VALUE SPACES.
           05 WS-UNIT-OF-MEASURE   PIC X(03) VALUE SPACES.
           05 WS-WEEKS-LEAD-TIME   PIC S9(03) COMP VALUE ZEROS.
           05 WS-VEHICLE-MAKE      PIC X(03) VALUE SPACES.
           05 WS-VEHICLE-MODEL     PIC X(10) VALUE SPACES.
      *    05 WS-VEHICLE-YEAR      PIC X(04) VALUE '0000'.
           05 WS-VEHICLE-YEAR      PIC X(04) VALUE SPACES.

       01 WS-YEAR-NUM      PIC 9(4).
       01 WS-VEHICLE-YEAR-DIS      PIC 9(4).

       LINKAGE SECTION.
       01 LS-PARTS.
              10  LS-PART-NUMBER       PIC X(23).
              10  LS-PART-NAME         PIC X(14).
              10  LS-SPEC-NUMBER       PIC X(07).
              10  LS-GOVT-COMML-CODE   PIC X(01).
              10  LS-BLUEPRINT-NUMBER  PIC X(10).
              10  LS-UNIT-OF-MEASURE   PIC X(03).
              10  LS-WEEKS-LEAD-TIME   PIC S9(3).
              10  LS-VEHICLE-MAKE      PIC X(03).
              10  LS-VEHICLE-MODEL     PIC X(10).
              10  LS-VEHICLE-YEAR      PIC X(04).

       01 LS-PARTS-RTN-CODE    PIC 9(01) VALUE ZERO.

       COPY ERRORSUB REPLACING ==(PRFX)== BY ==LS-PARTS==.

       PROCEDURE DIVISION USING LS-PARTS,
                                LS-PARTS-RTN-CODE,
                                LS-PARTS-ERROR-TBL,
                                LS-PARTS-ERROR-NUM.

           PERFORM 100-HOUSEKEEPING.
           PERFORM 200-DATACHECK.


       100-HOUSEKEEPING.
            *> VARIABLES INITIALIZATION
            MOVE ZERO TO LS-PARTS-RTN-CODE.
            MOVE ZERO TO LS-PARTS-ERROR-NUM .
            MOVE ZERO TO  ERR-COUNTER.
            INITIALIZE LS-PARTS-ERROR-TBL.

       200-DATACHECK.
            *> CHEK OF EACH FIELD OF THE REGISTER
            *> PART-NUMBER ERROR HANDLING
            IF ((LS-PART-NUMBER = LOW-VALUE) OR
                   (LS-PART-NUMBER = SPACES)) THEN
               ADD +1 TO ERR-COUNTER
               MOVE 8 TO LS-PARTS-RTN-CODE
               MOVE  "P001E" TO MSG-NO(ERR-COUNTER)
               MOVE "PART-NUMBER can not be empty." TO
                      MSG-TEXT(ERR-COUNTER)
            END-IF.

            *> PART-NAME ERROR HANDLING
            IF  ((LS-PART-NAME = LOW-VALUE) OR
                   (LS-PART-NAME = SPACES)) THEN
               ADD +1 TO ERR-COUNTER
               MOVE 8 TO LS-PARTS-RTN-CODE
               MOVE  "P002E" TO MSG-NO(ERR-COUNTER)
               MOVE "PART-Name can not be empty." TO
                      MSG-TEXT(ERR-COUNTER)
            END-IF.

            *> WEEKS-LEAD-TIME EVALUATION
            EVALUATE LS-WEEKS-LEAD-TIME
                WHEN 1  CONTINUE
                WHEN 2  CONTINUE
                WHEN 3  CONTINUE
                WHEN 4  CONTINUE
                WHEN OTHER
                *> ERROR HANDLING
                     ADD +1 TO ERR-COUNTER
                     MOVE 8 TO LS-PARTS-RTN-CODE
                     MOVE  "P003E" TO MSG-NO(ERR-COUNTER)
                     MOVE
                     "WEEKS-LEAD-TIME can only contain 1,2,3 or 4."
                      TO MSG-TEXT(ERR-COUNTER)

            END-EVALUATE.

            *> VEHICLE-MAKE EVALUATION
           EVALUATE FUNCTION UPPER-CASE(LS-VEHICLE-MAKE)
                WHEN "CHR" CONTINUE
                WHEN "FOR" CONTINUE
                WHEN "GM " CONTINUE
                WHEN "VW " CONTINUE
                WHEN "TOY" CONTINUE
                WHEN "JAG" CONTINUE
                WHEN "PEU" CONTINUE
                WHEN "BMW" CONTINUE
                WHEN OTHER
                *> ERROR HANDLING
                     ADD +1 TO ERR-COUNTER
                     MOVE 8 TO LS-PARTS-RTN-CODE
                     If ERR-COUNTER < 4 THEN
                       MOVE  "P004E" TO MSG-NO(ERR-COUNTER)
                       MOVE  "Invalid value for VEHICLE-MAKE."
                        TO MSG-TEXT(ERR-COUNTER)
                     ELSE
                       MOVE  "P099E" TO MSG-NO(ERR-COUNTER)
                       MOVE  "More than 3 fields have errors."
                        TO MSG-TEXT(ERR-COUNTER)
                     END-IF
           END-EVALUATE.

            *> VEHICLE-MODEL ERROR HANDLING
            IF ((LS-VEHICLE-MODEL = LOW-VALUE) OR
                   (LS-VEHICLE-MODEL = SPACES)) THEN
               ADD +1 TO ERR-COUNTER
               MOVE 8 TO LS-PARTS-RTN-CODE
               If ERR-COUNTER < 4 THEN
                 MOVE  "P005E" TO MSG-NO(ERR-COUNTER)
                 MOVE "VEHICLE-MODEL can not be empty." TO
                      MSG-TEXT(ERR-COUNTER)
               END-IF
               IF ERR-COUNTER = 4 THEN
                       MOVE  "P099E" TO MSG-NO(ERR-COUNTER)
                       MOVE  "More than 3 fields have errors."
                        TO MSG-TEXT(ERR-COUNTER)
                END-IF
            END-IF.

            *> VEHICLE-YEAR EVALUATION
            IF NOT ((LS-VEHICLE-YEAR = LOW-VALUE) OR
                   (LS-VEHICLE-YEAR = SPACES)) THEN
               IF (LS-VEHICLE-YEAR >= "1990"
                  AND LS-VEHICLE-YEAR <= "2019") THEN
                    CONTINUE
               ELSE
               *> ERROR HANDLING
                 ADD +1 TO ERR-COUNTER
                 MOVE 8 TO LS-PARTS-RTN-CODE
                 IF ERR-COUNTER < 4 THEN
                 MOVE  "P007E" TO MSG-NO(ERR-COUNTER)
                 MOVE "VEHICLE-YEAR has to be between 1990 and 2019." TO
                      MSG-TEXT(ERR-COUNTER)
                 END-IF
                 IF ERR-COUNTER = 4 THEN
                       MOVE  "P099E" TO MSG-NO(ERR-COUNTER)
                       MOVE  "More than 3 fields have errors."
                        TO MSG-TEXT(ERR-COUNTER)
                 END-IF
            ELSE
               *> ERROR HANDLING
               ADD +1 TO ERR-COUNTER
               MOVE 8 TO LS-PARTS-RTN-CODE
               IF ERR-COUNTER < 4 THEN
                  MOVE  "P006E" TO MSG-NO(ERR-COUNTER)
                  MOVE "VEHICLE-YEAR can not be empty." TO
                        MSG-TEXT(ERR-COUNTER)
               END-IF
               IF ERR-COUNTER = 4 THEN
                       MOVE  "P099E" TO MSG-NO(ERR-COUNTER)
                       MOVE  "More than 3 fields have errors."
                        TO MSG-TEXT(ERR-COUNTER)
               END-IF
            END-IF.
           MOVE ERR-COUNTER TO LS-PARTS-ERROR-NUM .

           GOBACK.

