       IDENTIFICATION DIVISION.
      ****************************************************************
      **   THIS SUBPROGRAM PERFORMS THE VALIDATIONS ON THE PURCHASE´S
      **   ORDERS FIELDS TO SEND THEM TO THE MAIN PROGRAM OR,
      **   IF THERE ARE ERRORS, INFORM THEM
      ****************************************************************
       PROGRAM-ID. PURCHORN.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       77  ERR-COUNTER            PIC 9(1) VALUE ZERO.
       77  TABLE-MAX              PIC 9(01) VALUE 5.
       77  PROJ-IDX               PIC 9(01) VALUE ZERO.

       01  WS-PURCHSE-ORDER.
           05 WS-PO-NUMBER            PIC X(06) VALUE SPACES.
           05 WS-BUYYER-CODE          PIC X(03) VALUE SPACES.
           05 WS-QUANTITY             PIC S9(7) VALUE ZEROS.
           05 WS-UNIT-PRICE           PIC S9(07)V99 VALUE ZERO.
           05 WS-ORDER-DATE           PIC X(08) VALUE SPACES.
           05 WS-DELIVERY-DATE        PIC X(08) VALUE SPACES.

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
       01 LS-PURCHASE-ORDER.
          10 LS-PO-NUMBER         PIC X(06).
          10 LS-BUYER-CODE        PIC X(03).
          10 LS-QUANTITY          PIC S9(7).
          10 LS-UNIT-PRICE        PIC S9(7)V99.
          10 LS-ORDER-DATE        PIC 9(08).
          10 LS-DELIVERY-DATE      PIC 9(08).


       01 LS-PURCHRDS-RTN-CODE    PIC 9(01) VALUE ZERO.
       COPY ERRORSUB REPLACING ==(PRFX)== BY ==LS-PURCHRDS==.



       PROCEDURE DIVISION USING LS-PURCHASE-ORDER,
                                LS-PURCHRDS-RTN-CODE,
                                LS-PURCHRDS-ERROR-TBL,
                                LS-PURCHRDS-ERROR-NUM.

           PERFORM 100-HOUSEKEEPING.
           PERFORM 200-DATACHECK.


       100-HOUSEKEEPING.
           *> VARIABLES INITIALIZATION
           MOVE ZERO TO LS-PURCHRDS-RTN-CODE.
           MOVE ZERO TO LS-PURCHRDS-ERROR-NUM.
           MOVE ZERO TO  ERR-COUNTER.
           INITIALIZE LS-PURCHRDS-ERROR-TBL.

        200-DATACHECK.
           *> CHEK OF EACH FIELD OF THE REGISTER
           *> EMPTY FIELDS
           IF LS-PO-NUMBER = SPACES AND
               LS-BUYER-CODE= SPACES AND
               LS-QUANTITY = ZERO AND
               LS-ORDER-DATE = SPACES
           THEN
           *> ERROR HANDLING
                 ADD +1 TO ERR-COUNTER
                 MOVE 8 TO LS-PURCHRDS-RTN-CODE
                 MOVE  "PO00E" TO MSG-NO(ERR-COUNTER)
                 MOVE "NO Purchas Order ." TO
                      MSG-TEXT(ERR-COUNTER)
                 MOVE ERR-COUNTER TO LS-PURCHRDS-ERROR-NUM
                 GOBACK
           ELSE
            *> NOT EMPTY PO-NUMBER
            IF NOT ((LS-PO-NUMBER = LOW-VALUE) OR
                   (LS-PO-NUMBER = SPACES)) THEN
               MOVE LS-PO-NUMBER TO WS-PO-NUMBER
            ELSE
            *> ERROR HANDLING
               ADD +1 TO ERR-COUNTER
               MOVE 8 TO LS-PURCHRDS-RTN-CODE
               MOVE "PO01E" TO MSG-NO(ERR-COUNTER)
               MOVE "PO-NUMBER can not be  empty " TO
                          MSG-TEXT(ERR-COUNTER)
            END-IF.

            *> NOT EMPTY BUYER-CODE-NUMBER
            IF NOT ((LS-BUYER-CODE = LOW-VALUE) OR
                   (LS-BUYER-CODE = SPACES)) THEN
               MOVE LS-BUYER-CODE TO WS-BUYYER-CODE
            ELSE
            *> ERROR HANDLING
               ADD +1 TO ERR-COUNTER
               MOVE 8 TO LS-PURCHRDS-RTN-CODE
               MOVE "PO02E" TO MSG-NO(ERR-COUNTER)
               MOVE "BUYER CODE can not be  empt.y" TO
                          MSG-TEXT(ERR-COUNTER)
            END-IF.


            *> QUANTITY VALIDATIONS
            IF (LS-QUANTITY IS NUMERIC) THEN
               IF (LS-QUANTITY > 0) AND (LS-QUANTITY < 999999) THEN
                   MOVE LS-QUANTITY TO WS-QUANTITY
               ELSE
               *> ERROR HANDLING
                  ADD +1 TO ERR-COUNTER
                  MOVE 8 TO LS-PURCHRDS-RTN-CODE
                  MOVE "PO03E" TO MSG-NO(ERR-COUNTER)
                  MOVE "QUANTITY needs to be betweenn1 and 999999. " TO
                             MSG-TEXT(ERR-COUNTER)
               END-IF
            ELSE
            *> ERROR HANDLING
               ADD +1 TO ERR-COUNTER
               MOVE 8 TO LS-PURCHRDS-RTN-CODE
               MOVE "PO04E" TO MSG-NO(ERR-COUNTER)
               MOVE "QUANTITY is numberic. " TO
                          MSG-TEXT(ERR-COUNTER)
            END-IF.

            *> UNIT-PRICE VALIDATION
            IF (LS-UNIT-PRICE IS NUMERIC) THEN
               IF ((LS-QUANTITY > 0 ) AND
                  (LS-UNIT-PRICE > 1) AND
                  (LS-UNIT-PRICE < 1000000))  THEN
                    MOVE LS-UNIT-PRICE TO WS-UNIT-PRICE
               ELSE
               *> ERROR HANDLING
                 ADD +1 TO ERR-COUNTER
                 MOVE 8 TO LS-PURCHRDS-RTN-CODE
                 IF ERR-COUNTER < 4
                    MOVE  "PO05E" TO MSG-NO(ERR-COUNTER)
                    MOVE
                    "UNIT-PRICE has to be between $1 and $1000000."
                         TO MSG-TEXT(ERR-COUNTER)
                  ELSE
                    MOVE "PO99E" TO MSG-NO(ERR-COUNTER)
                    MOVE  "More than 3 fields have errors."
                          TO MSG-TEXT(ERR-COUNTER)
                  END-IF
               END-IF
            ELSE
            *> ERROR HANDLING
               ADD +1 TO ERR-COUNTER
               MOVE 8 TO LS-PURCHRDS-RTN-CODE
               IF ERR-COUNTER < 4
                  MOVE  "PO06E" TO MSG-NO(ERR-COUNTER)
                  MOVE   "UNIT-PRICE is numeric."
                       TO MSG-TEXT(ERR-COUNTER)
                ELSE
                  IF ERR-COUNTER = 4 THEN
                     MOVE  "PO99E" TO MSG-NO(ERR-COUNTER)
                     MOVE  "More than 3 fields have errors."
                        TO MSG-TEXT(ERR-COUNTER)
                  END-IF
                END-IF
            END-IF.

            *> NOT EMPTY ORDER-DATE AND ORDER-DATE VALIDATION
            IF NOT ((LS-ORDER-DATE = LOW-VALUES) OR
                   (LS-ORDER-DATE = SPACES )) THEN
               MOVE LS-ORDER-DATE   TO WS-DATE-IN-STR-CEE
               CALL 'CEEDAYS' USING WS-DATE-IN-CEE
                                    WS-PICSTR-IN, WS-ORDER-DATE, WS-FC
               IF FC-SEV = ZERO
                   MOVE LS-ORDER-DATE TO WS-ORDER-DATE
               ELSE
               *> ERROR HANDLING
                  ADD +1 TO ERR-COUNTER
                  MOVE 8 TO LS-PURCHRDS-RTN-CODE
                  IF ERR-COUNTER < 4
                     MOVE  "PO07E" TO MSG-NO(ERR-COUNTER)
                     MOVE  "Invalid Order date."
                          TO MSG-TEXT(ERR-COUNTER)
                   ELSE
                     IF ERR-COUNTER = 4 THEN
                        MOVE  "PO99E" TO MSG-NO(ERR-COUNTER)
                        MOVE  "More than 3 fields have errors."
                           TO MSG-TEXT(ERR-COUNTER)
                     END-IF
                   END-IF
                  END-IF
            ELSE

              ADD +1 TO ERR-COUNTER
              MOVE 8 TO LS-PURCHRDS-RTN-CODE
                  IF ERR-COUNTER < 4
                     MOVE  "PO08E" TO MSG-NO(ERR-COUNTER)
                     MOVE  "Order date can not be empty."
                          TO MSG-TEXT(ERR-COUNTER)
                   ELSE
                     IF ERR-COUNTER = 4 THEN
                        MOVE  "PO99E" TO MSG-NO(ERR-COUNTER)
                        MOVE  "More than 3 fields have errors."
                           TO MSG-TEXT(ERR-COUNTER)
                     END-IF
                   END-IF
            END-IF

            *> NOT EMPTY DELIVERY-DATE AND DELIVERY-DATE VALIDATION
            IF NOT ((LS-DELIVERY-DATE  = LOW-VALUES) OR
                    (LS-DELIVERY-DATE  = SPACES)) THEN
               MOVE LS-DELIVERY-DATE   TO WS-DATE-IN-STR-CEE
               CALL 'CEEDAYS' USING WS-DATE-IN-CEE
                                    WS-PICSTR-IN, WS-DELIVERY-DATE,
                                    WS-FC
               IF FC-SEV = ZERO
                  IF (LS-DELIVERY-DATE > LS-ORDER-DATE) THEN
                      MOVE LS-DELIVERY-DATE TO WS-DELIVERY-DATE
                  ELSE
                  *> ERROR HANDLING
                     ADD +1 TO ERR-COUNTER
                     MOVE 8 TO LS-PURCHRDS-RTN-CODE
                     IF ERR-COUNTER < 4
                        MOVE  "PO09E" TO MSG-NO(ERR-COUNTER)
                        MOVE  "Delivery Date is befor Order date."
                             TO MSG-TEXT(ERR-COUNTER)
                      ELSE
                        IF ERR-COUNTER = 4 THEN
                           MOVE  "PO99E" TO MSG-NO(ERR-COUNTER)
                           MOVE  "More than 3 fields have errors."
                              TO MSG-TEXT(ERR-COUNTER)
                        END-IF
                      END-IF
                  END-IF
               ELSE
                  ADD +1 TO ERR-COUNTER
                  MOVE 8 TO LS-PURCHRDS-RTN-CODE
                  IF ERR-COUNTER < 4
                     MOVE  "PO11E" TO MSG-NO(ERR-COUNTER)
                     MOVE  "Invalid Delivery Date."
                          TO MSG-TEXT(ERR-COUNTER)
                   ELSE
                     IF ERR-COUNTER = 4 THEN
                        MOVE  "PO99E" TO MSG-NO(ERR-COUNTER)
                        MOVE  "More than 3 fields have errors."
                           TO MSG-TEXT(ERR-COUNTER)
                     END-IF
                   END-IF
               END-IF
            END-IF.  *> Delivery date is  empty  - valid
           MOVE ERR-COUNTER TO LS-PURCHRDS-ERROR-NUM .
           GOBACK.

