       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PRTSUPP.
      ****************************************************************
      *   Main Driver program
      *   ***********************
      *   INPUT :  -table containing  data for Parts / supplier/
      *            supplier address and  purchase order , sorted  and
      *             duplicates  removed
      *             PARTSUPP  DD
      *             -table containing   States and zipcods
      *             ZIPCODE DD
      *   OUTPUT :  JES : PARTS,  SUPPLIER , SUPPLIER ADDRESS ,
      *                   PURCHASE ORDERS for  good records
      *                   ERROR for records with missing on invalid
      *                   data .
      *                   PARTS  DD SYSOUT=*
      *                   PURCHRDS  DD SYSOUT=*
      *                   SUPPLIER  DD SYSOUT=*
      *                   SUPPADDR DD SYSOUT=*
      *                   ERROR   DD SYSOUT=*
      *              DS   Report of all good parts  (LRCL 480,FB)
      *                   GPARTSUP DD SYSOUT =*
      * *************************************************************
      * The main program will  read in  the  zip code data in to a
      * table , stores is .   We will than read in 1 record at the time
      *  of the PARTSUPP input file .  We analyze  each section of the
      *  data based on preset requirements:
      *  PARTS
      * Required fields PART NUMBER/PART NAME/VEHICLE MAKE,
      * VEHICLE MODEL VEHICLE YEAR
      * VEHICLE MAKE must be one of the listed 88 level fields
      * VEHICLE YEAR must be between 1990 and 2019
      * WEEKS LEAD TIME must be numeric and between 1 and 4
      *
      * SUPPLIERS
      * Required fields: SUPPLIER CODE, SUPPLIER TYPE, SUPPLIER NAME,
      * SUPPLIER PERF
      * SUPPLIER TYPE, SUPPLIER RATING, SUPPLIER STATUS must be one of
      * the listed 88 level values
      * SUPPLIER ACT DATE Optional but if a value exists in the field
      * ensure that the date is valid
      * If SUPPLIER TYPE is SUBCONTRACTOR SUPPLIER RATING must be
      * Highest Quality
      * SUPPLIER STATUS must be one of the listed 88 level fields
      *
      * SUPP ADDRESS
      * Required fields: ADDRESS 1, CITY, ADDR STATE and ZIP CODE
      * ADDRESS TYPE must be one of the 88 level fields
      * ZIP CODE and ADDR STATE must match in the STATEZIP file (next
      * empty address are ok as long 1 good address exists
      *
      * PURCHASE ORDER
      * Required fields: PO NUMBER, BUYER CODE, ORDER --,DATE, QUANTITY
      * QUANTITY must be between 0 and 999,999
      * If QUANTITY is > 0, UNIT PRICE must be > 0.
      * UNIT PRICE must be between $1 and $1,000,000.00
      * ORDER DATE must be a valid date
      * DELIVERY DATE is optional but if there is data, it must be a
      * valid date and the date must be later than ORDER DATE
      * emtpy purchas orders are ok as long 1 good purchase order
      * exists
      *
      * Error handling
      * If there are < 4 errors:
      * The bad record must include error messages in the
      * output file
      * If there are > 3 errors:
  T   * he bad record is written to the output file with a single
      * message that the record is completely invalid
      *
      * *************************************************************
       AUTHOR.          COBWO. *>COBOL WORIRER
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARTSUPPFILE ASSIGN TO UT-C-PARTSUPP
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS  IS PARTSUPP-ST.
           SELECT PURCHRDS-FILE ASSIGN TO UT-C-PURCHRDS
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS  IS PURCHRDS-ST.
           SELECT SUPPLIER-FILE ASSIGN TO UT-C-SUPPLIER
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS  IS SUPPLIER-ST.
           SELECT PARTS-FILE ASSIGN TO UT-C-PARTS
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS  IS PARTS-ST.
           SELECT SUPPADDR-FILE ASSIGN TO UT-C-SUPPADDR
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS  IS SUPPADDR-ST.
           SELECT ERROR-FILE ASSIGN TO UT-C-ERROR
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS  IS ERROR-ST.
           SELECT ZIPCODE-FILE ASSIGN TO UT-C-ZIPCODE
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS  IS ZIPCODE-ST.
           SELECT GPART-SUPP-FILE ASSIGN TO UT-C-GPARTSUP
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS  IS GPARTSUPP-ST.

       DATA DIVISION.
       FILE SECTION.
       FD PARTSUPPFILE
           RECORD CONTAINS 480 CHARACTERS
           RECORDING MODE IS F.
       01 PARTSUPP-RECORD      PIC X(480).

       FD PURCHRDS-FILE
           RECORD CONTAINS 41 CHARACTERS
           RECORDING MODE IS F.
       01 PURCHRDS-RECORD      PIC X(41).

       FD SUPPLIER-FILE
           RECORD CONTAINS 80 CHARACTERS
           RECORDING MODE IS F.
       01 SUPPLIER-RECORD      PIC X(80).

       FD PARTS-FILE
           RECORD CONTAINS 80 CHARACTERS
           RECORDING MODE IS F.
       01 PARTS-RECORD          PIC X(80).

       FD SUPPADDR-FILE
           RECORD CONTAINS 80 CHARACTERS
           RECORDING MODE IS F.
       01 SUPPADDR-RECORD      PIC X(80).

       FD ZIPCODE-FILE
           RECORD CONTAINS 40 CHARACTERS
           RECORDING MODE IS F.
       01  ZIPCODE-RECORD      PIC X(40).

       FD ERROR-FILE
           RECORD CONTAINS 80 CHARACTERS
           RECORDING MODE IS F.
       01 ERROR-RECORD      PIC X(80).

       FD GPART-SUPP-FILE
           RECORD CONTAINS 480 CHARACTERS
           RECORDING MODE IS F.
       01 GPART-SUPP-RECORD      PIC X(480).

       WORKING-STORAGE SECTION.

       77 ALLOWED-AMT          PIC S9(7)V99   VALUE 9999999.99.
       77 TABLE-MAX            PIC 9(01) VALUE 4.
       77 TABLE-MAX-ADDR       PIC 9(01) VALUE 4.
       77 PROJ-IDX             PIC 9(01) VALUE ZERO.
       77 TBL-IDX              PIC 9(02) VALUE ZERO.
       77 SADDR-IDX            PIC 9(01) VALUE ZERO.
       77 ERR-IDX              PIC 9(01) VALUE ZERO.
       77 WS-REC-CNT           PIC 9(8) VALUE ZERO.
       77 WS-ALL-ERROR-CNT     PIC 9(2) VALUE ZERO.
       77 WS-ZIP-CODE10        PIC X(10) VALUE SPACES.
       77 WS-VEHICLE-MAKE      PIC x(10) VALUE SPACES.

       01  WS01-PURCHSE-ORDER.
           05 WS01-PO-NUMBER            PIC X(06) VALUE SPACES.
           05 WS01-BUYER-CODE           PIC X(03) VALUE SPACES.
           05 WS01-QUANTITY             PIC S9(7).
           05 WS01-UNIT-PRICE           PIC S9(07)V99.
           05 WS01-ORDER-DATE           PIC X(08) VALUE SPACES.
           05 WS01-DELIVERY-DATE        PIC X(08) VALUE SPACES.

       01 WS-SUPP-ADDRESS.
           05 WS-ADDRESS-TYPE      PIC X(01) VALUE SPACES.
           05 WS-ADDRESS-1         PIC X(15) VALUE SPACES.
           05 WS-ADDRESS-2         PIC X(15) VALUE SPACES.
           05 WS-ADDRESS-3         PIC X(15) VALUE SPACES.
           05 WS-CITY              PIC X(15) VALUE SPACES.
           05 WS-ADDR-STATE        PIC X(02) VALUE SPACES.
           05 WS-ZIP-CODE          PIC X(05) VALUE SPACES.

      * Error handling variables
        COPY ERROR REPLACING ==(PRFX)== BY ==WS-PARTS==.
        COPY ERROR REPLACING ==(PRFX)== BY ==WS-SUPPLIER==.
        COPY ERROR REPLACING ==(PRFX)== BY ==WS-SUPPADDR==.
        COPY ERROR REPLACING ==(PRFX)== BY ==WS-SUPPADDR1==.
        COPY ERROR REPLACING ==(PRFX)== BY ==WS-SUPPADDR2==.
        COPY ERROR REPLACING ==(PRFX)== BY ==WS-SUPPADDR3==.
        COPY ERROR REPLACING ==(PRFX)== BY ==WS-PURCHRDS==.
        COPY ERROR REPLACING ==(PRFX)== BY ==WS-PURCHRDS1==.
        COPY ERROR REPLACING ==(PRFX)== BY ==WS-PURCHRDS2==.
        COPY ERROR REPLACING ==(PRFX)== BY ==WS-PURCHRDS3==.

        01 WS-PURCHRDS-RTN-CODE  PIC 9(01) VALUE 0.
        01 WS-SUPPLIER-RTN-CODE  PIC 9(01) VALUE 0.
        01 WS-PARTS-RTN-CODE     PIC 9(01) VALUE 0.
        01 WS-SUPPADDR-RTN-CODE  PIC 9(01) VALUE 0.

        01 PROGRAM-SWITCHES.
           05 PARTSUPPFILE-EOF          PIC X(1)   VALUE 'N'.
               88 NO-MORE-PARTSUPPFILE  VALUE 'Y'.
           05 ZIP-CODE-EOF              PIC X(1)   VALUE 'N'.
               88 NO-MORE-ZIP-CODE      VALUE 'Y'.
           05 PARTSUPP-ST               PIC X(2).
               88 PARTSUPPFILE-OK       VALUE '00'.
           05 PURCHRDS-ST               PIC X(2).
               88 PURCHRDS-OK           VALUE '00'.
           05 SUPPLIER-ST               PIC X(2).
               88 SUPPLIER-OK           VALUE '00'.
           05 PARTS-ST                  PIC X(2).
               88 PARTS-OK              VALUE '00'.
           05 SUPPADDR-ST                PIC X(2).
               88 SUPPADDR-OK            VALUE '00'.
           05 ERROR-ST                  PIC X(2).
               88 ERROR-OK              VALUE '00'.
           05 ZIPCODE-ST                PIC X(2).
               88 ZIPCODE-OK            VALUE '00'.
           05 GPARTSUPP-ST               PIC X(2).
               88 GPARTSUPP-OK            VALUE '00'.
           COPY PRTSUBAD.
           COPY SUPPLIER.
           COPY PURCHRDS.
           COPY PARTS.
           COPY SUPPADDR.
           COPY ZIPCODE.

       PROCEDURE DIVISION.
           PERFORM 100-HOUSEKEEPING.
           PERFORM 200-PROCESS-DATA UNTIL NO-MORE-PARTSUPPFILE.
           PERFORM 900-WRAP-UP.
           GOBACK.
       100-HOUSEKEEPING.
           MOVE ZERO TO WS-ALL-ERROR-CNT.
           PERFORM 300-OPEN-FILES.
           PERFORM 410-READ-ZIP.
           PERFORM 400-READ-DATA.

       300-OPEN-FILES.
           OPEN INPUT PARTSUPPFILE
           IF NOT PARTSUPPFILE-OK
              DISPLAY 'Input File PARTSUPPFILE File Error'
              GO TO 999-ERR-RTN.
           OPEN OUTPUT PURCHRDS-FILE
           IF NOT PURCHRDS-OK
              DISPLAY 'Output PURCHRDSFILE File Error'
              GO TO 999-ERR-RTN.
           OPEN OUTPUT SUPPLIER-FILE
           IF NOT SUPPLIER-OK
              DISPLAY 'Output SUPPLIER File Error'
              GO TO 999-ERR-RTN.
           OPEN OUTPUT PARTS-FILE
           IF NOT PARTS-OK
              DISPLAY 'Output PARTS Error'
              GO TO 999-ERR-RTN.
           OPEN OUTPUT SUPPADDR-FILE
           IF NOT SUPPADDR-OK
              DISPLAY 'Output SUPPADDR File Error'
              GO TO 999-ERR-RTN.
           OPEN OUTPUT ERROR-FILE
           IF NOT ERROR-OK
              DISPLAY 'Output ERROR File Error'
              GO TO 999-ERR-RTN.
           OPEN OUTPUT GPART-SUPP-FILE
           IF NOT GPARTSUPP-OK
              DISPLAY 'Output GPART SUPPLIER File Error'
              GO TO 999-ERR-RTN.
           OPEN INPUT ZIPCODE-FILE
           IF NOT ZIPCODE-OK
              DISPLAY 'Input ZIPCODE File Error'
              GO TO 999-ERR-RTN.

           *> parts , supplier, supplier address, purchase order, error
           PERFORM 610-PRINT-PAGE-HEADERS.



       400-READ-DATA.
               READ PARTSUPPFILE  INTO  PART-SUPP-ADDR-PO
                AT END MOVE "Y" TO PARTSUPPFILE-EOF
               END-READ.

       410-READ-ZIP.
            PERFORM VARYING TBL-IDX FROM 1 BY 1
            UNTIL ZIP-CODE-EOF = "Y"
               READ ZIPCODE-FILE  INTO  ZIP-ROW(TBL-IDX)
                  AT END MOVE "Y" TO ZIP-CODE-EOF

               END-READ
            END-PERFORM.

       200-PROCESS-DATA.
           MOVE ZERO TO WS-ALL-ERROR-CNT. *>Reset Error for new record
      *>collect Number of record
              ADD +1 TO WS-REC-CNT.

              PERFORM 210-PROCESS-PART.
              PERFORM 220-PROCESS-SUPPLIER.
              PERFORM 230-PROCESS-SUP-ADDR.
              PERFORM 240-PROCESS-PURCH-ORD.
              IF  WS-ALL-ERROR-CNT > 0 THEN
                   PERFORM 800-ERROR_WRITING
              ELSE
                   PERFORM 600-PRINTING
                   *> good record  - save in output file
                   PERFORM 615-WRITE-REC-GPARTSUPP
              END-IF.
              PERFORM 400-READ-DATA.
       210-PROCESS-PART.
              INITIALIZE   WS-PARTS-ERROR-TBL.
              MOVE ZERO TO WS-PARTS-ERROR-NUM .
              *> run check on parts data
              CALL 'PARTS' USING IN-PARTS,
                                 WS-PARTS-RTN-CODE,
                                 WS-PARTS-ERROR-TBL,
                                 WS-PARTS-ERROR-NUM.
              ADD WS-PARTS-ERROR-NUM TO WS-ALL-ERROR-CNT.

       220-PROCESS-SUPPLIER.
              INITIALIZE   WS-SUPPLIER-ERROR-TBL.
              MOVE ZERO TO WS-SUPPLIER-ERROR-NUM .
              *> run check on supplier data
              CALL 'SUPPLIER' USING IN-SUPPLIERS,
                                    WS-SUPPLIER-RTN-CODE,
                                    WS-SUPPLIER-ERROR-TBL,
                                    WS-SUPPLIER-ERROR-NUM.
              ADD WS-SUPPLIER-ERROR-NUM TO WS-ALL-ERROR-CNT.

       230-PROCESS-SUP-ADDR.
          *> making sure they do not contain info from previous run
           INITIALIZE   WS-SUPPADDR-ERROR-TBL WS-SUPPADDR1-ERROR-TBL,
                        WS-SUPPADDR2-ERROR-TBL, WS-SUPP-ADDRESS,
                        WS-SUPPADDR3-ERROR-TBL.
           MOVE ZERO TO SADDR-IDX.
           MOVE ZERO TO WS-SUPPADDR1-ERROR-NUM.
           MOVE ZERO TO WS-SUPPADDR2-ERROR-NUM.
           MOVE ZERO TO WS-SUPPADDR3-ERROR-NUM.
           *> check each of the  3 addresses , save error tabel  from
           *> each run for further error handling
           PERFORM VARYING SADDR-IDX FROM 1 BY 1
                    UNTIL SADDR-IDX >= TABLE-MAX-ADDR

              MOVE ZERO TO WS-SUPPADDR-ERROR-NUM

              MOVE IN-ADDRESS-TYPE(SADDR-IDX)  TO WS-ADDRESS-TYPE
              MOVE IN-ADDRESS-1(SADDR-IDX)     TO WS-ADDRESS-1
              MOVE IN-ADDRESS-2(SADDR-IDX)     TO WS-ADDRESS-2
              MOVE IN-ADDRESS-3(SADDR-IDX)     TO WS-ADDRESS-3
              MOVE IN-CITY(SADDR-IDX)          TO WS-CITY
              MOVE IN-ADDR-STATE(SADDR-IDX)    TO WS-ADDR-STATE
              MOVE IN-ZIP-CODE(SADDR-IDX)      TO WS-ZIP-CODE10
              MOVE WS-ZIP-CODE10(1:5)         TO WS-ZIP-CODE
              CALL 'SUPPADDR' USING WS-SUPP-ADDRESS,
                                    ZIP-CODE-TBL,
                                    WS-SUPPADDR-RTN-CODE,
                                    WS-SUPPADDR-ERROR-TBL,
                                    WS-SUPPADDR-ERROR-NUM

             DISPLAY 'SUPPADDR RECORD DETAILS: ', WS-SUPP-ADDRESS
            *> save ERROR table for each address
            MOVE ZERO TO ERR-IDX
            EVALUATE SADDR-IDX
                WHEN 1
                   If  WS-SUPPADDR-ERROR-NUM > 0 then
                     PERFORM VARYING ERR-IDX FROM 1 BY 1
                     UNTIL ERR-IDX > WS-SUPPADDR-ERROR-NUM
                         MOVE WS-SUPPADDR-MSG-NO(ERR-IDX)  TO
                              WS-SUPPADDR1-MSG-NO(ERR-IDX)
                         MOVE WS-SUPPADDR-MSG-TEXT(ERR-IDX) TO
                              WS-SUPPADDR1-MSG-TEXT(ERR-IDX)
                         MOVE  WS-SUPPADDR-ERROR-NUM  TO
                               WS-SUPPADDR1-ERROR-NUM
                      END-PERFORM
                   END-IF
                WHEN 2
                   If  WS-SUPPADDR-ERROR-NUM > 0 then
                     PERFORM VARYING ERR-IDX FROM 1 BY 1
                     UNTIL ERR-IDX > WS-SUPPADDR-ERROR-NUM
                         MOVE WS-SUPPADDR-MSG-NO(ERR-IDX)  TO
                              WS-SUPPADDR2-MSG-NO(ERR-IDX)
                         MOVE WS-SUPPADDR-MSG-TEXT(ERR-IDX) TO
                              WS-SUPPADDR2-MSG-TEXT(ERR-IDX)
                         MOVE  WS-SUPPADDR-ERROR-NUM  TO
                               WS-SUPPADDR2-ERROR-NUM
                      END-PERFORM
                   END-IF
                WHEN 3
                   If  WS-SUPPADDR-ERROR-NUM > 0 then
                     PERFORM VARYING ERR-IDX FROM 1 BY 1
                     UNTIL ERR-IDX > WS-SUPPADDR-ERROR-NUM
                         MOVE WS-SUPPADDR-MSG-NO(ERR-IDX)  TO
                              WS-SUPPADDR3-MSG-NO(ERR-IDX)
                         MOVE WS-SUPPADDR-MSG-TEXT(ERR-IDX) TO
                              WS-SUPPADDR3-MSG-TEXT(ERR-IDX)
                         MOVE  WS-SUPPADDR-ERROR-NUM  TO
                               WS-SUPPADDR3-ERROR-NUM
                      END-PERFORM
                   END-IF
                 WHEN OTHER    DISPLAY "ADDRESS 4 ERROR"
             END-EVALUATE
           END-PERFORM.

         *> Begin Error handling
            MOVE ZERO to   WS-SUPPADDR-ERROR-NUM . *> reset value
         *> check if  any errors at all
            IF WS-SUPPADDR3-ERROR-NUM = ZERO  AND
               WS-SUPPADDR2-ERROR-NUM = ZERO  AND
               WS-SUPPADDR1-ERROR-NUM = ZERO
              THEN *> we have at least 1 good  address
                 MOVE ZERO TO  WS-SUPPADDR-ERROR-NUM
            ELSE
              *> there are errors
              *> ALL 3 are empty
              IF  WS-SUPPADDR3-MSG-NO(1) = "SA00E" AND
                  WS-SUPPADDR2-MSG-NO(1) = "SA00E" AND
                  WS-SUPPADDR1-MSG-NO(1) = "SA00E"
              THEN
                  *> all 3  addresses are  empty   - 1 ERROR
                  MOVE 1  to  WS-SUPPADDR-ERROR-NUM
                  MOVE "SA00E" to WS-SUPPADDR-MSG-NO(1)
                  MOVE SPACES to  WS-SUPPADDR-MSG-TEXT(1)
                  MOVE "Supplier has no addresses" TO
                      WS-SUPPADDR-MSG-TEXT(1)
              ELSE
                *> check if we have empty and good ones
                  IF  (((WS-SUPPADDR3-MSG-NO(1) = "SA00E" OR
                       WS-SUPPADDR3-ERROR-NUM = zero)  AND
                      (WS-SUPPADDR2-MSG-NO(1) = "SA00E" OR
                       WS-SUPPADDR2-ERROR-NUM = zero)  AND
                      (WS-SUPPADDR1-MSG-NO(1) = "SA00E" OR
                      WS-SUPPADDR1-ERROR-NUM = ZERO  ) )
                      AND NOT  *> previous condition
                      (WS-SUPPADDR3-MSG-NO(1) = "SA00E" AND
                       WS-SUPPADDR2-MSG-NO(1) = "SA00E" AND
                       WS-SUPPADDR1-MSG-NO(1) = "SA00E" ) )
                  THEN   *> 1 or 2  are good
                      MOVE 0  to  WS-SUPPADDR-ERROR-NUM
                  ELSE
                 *>ERRORS AND NOT  EMPTY
                 IF (WS-SUPPADDR3-ERROR-NUM > 0  AND
                     NOT WS-SUPPADDR3-MSG-NO(1) = "SA00E"  ) OR
                    (WS-SUPPADDR2-ERROR-NUM > 0   AND
                    NOT  WS-SUPPADDR2-MSG-NO(1) = "SA00E"  ) OR
                    (WS-SUPPADDR1-ERROR-NUM > 0   AND
                     NOT WS-SUPPADDR1-MSG-NO(1) = "SA00E" )
                  THEN   *> OK THERE ARE ERRORS  -
                      if  WS-SUPPADDR3-ERROR-NUM > zero THEN
                         add 1 TO   WS-SUPPADDR-ERROR-NUM
                      END-IF
                      if WS-SUPPADDR2-ERROR-NUM > zero  then
                         add 1 TO   WS-SUPPADDR-ERROR-NUM
                      END-IF
                      if  WS-SUPPADDR1-ERROR-NUM > zero  then
                         add 1 TO   WS-SUPPADDR-ERROR-NUM
                      END-IF
                       *> all 3 addresses have errors  and are not
                            *> Empty
                      iF WS-SUPPADDR-ERROR-NUM = 3 THEN
                        MOVE "SA01E" to WS-SUPPADDR-MSG-NO(1)
                        MOVE SPACES to  WS-SUPPADDR-MSG-TEXT(1)
                        MOVE "All Supplier addresses contain errors" TO
                            WS-SUPPADDR-MSG-TEXT(1)
                      end-if  *> 2 have errors
                      iF WS-SUPPADDR-ERROR-NUM = 2  THEN
                        MOVE "SA01E" to WS-SUPPADDR-MSG-NO(1)
                        MOVE SPACES to  WS-SUPPADDR-MSG-TEXT(1)
                        MOVE "2 Supplier addresses contain errors" TO
                            WS-SUPPADDR-MSG-TEXT(1)
                      end-if   *> 1 has an error
                      iF WS-SUPPADDR-ERROR-NUM = 1  THEN
                        MOVE "SA01E" to WS-SUPPADDR-MSG-NO(1)
                        MOVE SPACES to  WS-SUPPADDR-MSG-TEXT(1)
                        MOVE "1 Supplier addresses contain errors" TO
                            WS-SUPPADDR-MSG-TEXT(1)
                      end-if
                 END-IF

            END-IF.  *> check if  any errors at all
            ADD WS-SUPPADDR-ERROR-NUM TO WS-ALL-ERROR-CNT .

       240-PROCESS-PURCH-ORD.
           INITIALIZE   WS-PURCHRDS-ERROR-TBL,
                        WS-PURCHRDS1-ERROR-TBL,
                        WS-PURCHRDS2-ERROR-TBL,
                        WS-PURCHRDS3-ERROR-TBL, WS01-PURCHSE-ORDER ,
                        WS01-PURCHSE-ORDER.
              MOVE ZERO  TO WS-PURCHRDS1-ERROR-NUM .
              MOVE ZERO  TO WS-PURCHRDS2-ERROR-NUM .
              MOVE ZERO  TO WS-PURCHRDS3-ERROR-NUM .
           MOVE ZERO  TO PROJ-IDX
           *> check each of the  3 purch ord , save error tabel  from
           *> each run for further error handling
           PERFORM VARYING PROJ-IDX FROM 1 BY 1
                    UNTIL PROJ-IDX >= TABLE-MAX
           MOVE ZERO  TO WS-PURCHRDS-ERROR-NUM
              MOVE IN-PO-NUMBER(PROJ-IDX)     TO WS01-PO-NUMBER
              MOVE IN-BUYER-CODE(PROJ-IDX)    TO WS01-BUYER-CODE
              MOVE IN-QUANTITY(PROJ-IDX)      TO WS01-QUANTITY
              MOVE IN-UNIT-PRICE(PROJ-IDX)    TO WS01-UNIT-PRICE
              MOVE IN-ORDER-DATE(PROJ-IDX)    TO WS01-ORDER-DATE
              MOVE IN-DELIVERY-DATE(PROJ-IDX) TO WS01-DELIVERY-DATE
              CALL 'PURCHORN' USING WS01-PURCHSE-ORDER,
                                    WS-PURCHRDS-RTN-CODE,
                                    WS-PURCHRDS-ERROR-TBL,
                                    WS-PURCHRDS-ERROR-NUM
              DISPLAY 'PURCHAGE ORDER DETAILS:', WS01-PURCHSE-ORDER
              MOVE ZERO TO ERR-IDX
              EVALUATE PROJ-IDX
                WHEN 1
                     If  WS-PURCHRDS-ERROR-NUM > 0 then
                       PERFORM VARYING ERR-IDX FROM 1 BY 1
                       UNTIL ERR-IDX > WS-PURCHRDS-ERROR-NUM
                          MOVE WS-PURCHRDS-MSG-NO(ERR-IDX)  TO
                               WS-PURCHRDS1-MSG-NO(ERR-IDX)
                          MOVE WS-PURCHRDS-MSG-TEXT(ERR-IDX) TO
                               WS-PURCHRDS1-MSG-TEXT(ERR-IDX)
                          MOVE  WS-PURCHRDS-ERROR-NUM  TO
                                WS-PURCHRDS1-ERROR-NUM
                      END-PERFORM
                     END-IF
                WHEN 2
                     If  WS-PURCHRDS-ERROR-NUM > 0 then
                      PERFORM VARYING ERR-IDX FROM 1 BY 1
                       UNTIL ERR-IDX > WS-PURCHRDS-ERROR-NUM
                         MOVE WS-PURCHRDS-MSG-NO(ERR-IDX)  TO
                              WS-PURCHRDS2-MSG-NO(ERR-IDX)
                         MOVE WS-PURCHRDS-MSG-TEXT(ERR-IDX) TO
                              WS-PURCHRDS2-MSG-TEXT(ERR-IDX)
                         MOVE  WS-PURCHRDS-ERROR-NUM  TO
                               WS-PURCHRDS2-ERROR-NUM
                     END-PERFORM
                    END-IF
                WHEN 3
                     If  WS-PURCHRDS-ERROR-NUM > 0 then
                     PERFORM VARYING ERR-IDX FROM 1 BY 1
                       UNTIL ERR-IDX > WS-PURCHRDS-ERROR-NUM
                         MOVE WS-PURCHRDS-MSG-NO(ERR-IDX)  TO
                              WS-PURCHRDS3-MSG-NO(ERR-IDX)
                         MOVE WS-PURCHRDS-MSG-TEXT(ERR-IDX) TO
                              WS-PURCHRDS3-MSG-TEXT(ERR-IDX)
                         MOVE  WS-PURCHRDS-ERROR-NUM  TO
                               WS-PURCHRDS3-ERROR-NUM
                      END-PERFORM
                     END-IF
                 WHEN OTHER    DISPLAY "PUrchas Order in ERROR"
              END-EVALUATE
           END-PERFORM.
         *> BEegin rror handling
            MOVE ZERO to   WS-PURCHRDS-ERROR-NUM  *> reset value
         *> check if  any errors at all
            IF WS-PURCHRDS3-ERROR-NUM = ZERO  AND
               WS-PURCHRDS2-ERROR-NUM = ZERO  AND
               WS-PURCHRDS1-ERROR-NUM = ZERO
              THEN *> we have at least 1 good  address
                 MOVE ZERO TO  WS-PURCHRDS-ERROR-NUM
            ELSE
              *> there are errors
              *> ALL 3 are empty
              IF  WS-PURCHRDS3-MSG-NO(1) = "PO00E" AND
                  WS-PURCHRDS2-MSG-NO(1) = "PO00E" AND
                  WS-PURCHRDS1-MSG-NO(1) = "PO00E"
               THEN
                  *> all 3  PO are  empty   - 1 ERROR
                  MOVE 1  to  WS-PURCHRDS-ERROR-NUM
                  MOVE "PO00E" to WS-PURCHRDS-MSG-NO(1)
                  MOVE SPACES to  WS-PURCHRDS-MSG-TEXT(1)
                  MOVE "No PURCHASE ORDERs - at least 1 is needed." TO
                      WS-PURCHRDS-MSG-TEXT(1)
              ELSE
                *> check if we have empty and good ones
                  IF  (((WS-PURCHRDS3-MSG-NO(1) = "PO00E" OR
                       WS-PURCHRDS3-ERROR-NUM = zero)  AND
                      (WS-PURCHRDS2-MSG-NO(1) = "PO00E" OR
                       WS-PURCHRDS2-ERROR-NUM = zero)  AND
                      (WS-PURCHRDS1-MSG-NO(1) = "PO00E" OR
                      WS-PURCHRDS1-ERROR-NUM = ZERO  ) )
                      AND NOT  *> previous condition
                      (WS-PURCHRDS3-MSG-NO(1) = "PO00E" AND
                       WS-PURCHRDS2-MSG-NO(1) = "PO00E" AND
                       WS-PURCHRDS1-MSG-NO(1) = "PO00E" ) )
                  THEN   *> 1 or 2  are good
                      MOVE 0  to  WS-PURCHRDS-ERROR-NUM
                  END-IF
              END-IF
              *>ERRORS AND NOT  EMPTY
              IF (WS-PURCHRDS3-ERROR-NUM > 0  AND
                  NOT WS-PURCHRDS3-MSG-NO(1) = "PO00E"  ) OR
                 (WS-PURCHRDS2-ERROR-NUM > 0   AND
                 NOT  WS-PURCHRDS2-MSG-NO(1) = "PO00E"  ) OR
                 (WS-PURCHRDS1-ERROR-NUM > 0   AND
                  NOT WS-PURCHRDS1-MSG-NO(1) = "PO00E" )
               THEN   *> OK THERE ARE ERRORS  -
                   if  WS-PURCHRDS3-ERROR-NUM > zero THEN
                      add 1 TO   WS-PURCHRDS-ERROR-NUM
                   END-IF
                   if WS-PURCHRDS2-ERROR-NUM > zero  then
                      add 1 TO   WS-PURCHRDS-ERROR-NUM
                   END-IF
                   if  WS-PURCHRDS1-ERROR-NUM > zero  then
                      add 1 TO   WS-PURCHRDS-ERROR-NUM
                   END-IF
                    *> all 3 PO have errors  and are not
                         *> Empty
                   iF WS-PURCHRDS-ERROR-NUM = 3 THEN
                     MOVE "PO01E" to WS-PURCHRDS-MSG-NO(1)
                     MOVE SPACES to  WS-PURCHRDS-MSG-TEXT(1)
                     MOVE "All Purchase Orders contain errors" TO
                         WS-PURCHRDS-MSG-TEXT(1)
                   end-if  *> 2 have errors
                   iF WS-PURCHRDS-ERROR-NUM = 2  THEN
                     MOVE "PO01E" to WS-PURCHRDS-MSG-NO(1)
                     MOVE SPACES to  WS-PURCHRDS-MSG-TEXT(1)
                     MOVE "2 Purchase Orders contain error()" TO
                         WS-PURCHRDS-MSG-TEXT(1)
                   end-if   *> 1 has an error
                   iF WS-PURCHRDS-ERROR-NUM = 1  THEN
                     MOVE "PO01E" to WS-PURCHRDS-MSG-NO(1)
                     MOVE SPACES to  WS-PURCHRDS-MSG-TEXT(1)
                     MOVE "1 Purchase Order contains error(s)" TO
                         WS-PURCHRDS-MSG-TEXT(1)
                   end-if
               END-IF

            END-IF.  *> check if  any errors at all
            ADD WS-PURCHRDS-ERROR-NUM TO WS-ALL-ERROR-CNT.


       600-PRINTING.
            PERFORM 211-WRITE-REC-PARTS.
            PERFORM 221-WRITE-REC-SUPPLIER.
            PERFORM 231-WRITE-REC-SUP-ADDR.
            PERFORM 241-WRITE-REC-PURCHRDS.

       610-PRINT-PAGE-HEADERS.
           *> PARTS
              MOVE SPACES TO PARTS-RECORD.
              MOVE '**********************************************'
                   TO PARTS-RECORD .
              WRITE PARTS-RECORD.
              MOVE SPACES TO PARTS-RECORD.
              MOVE " LIST OF PARTS FROM  GOOD RECORDS"  To PARTS-RECORD.
              WRITE PARTS-RECORD.
              MOVE SPACES TO PARTS-RECORD .
              MOVE '**********************************************'
                   TO PARTS-RECORD .
              WRITE PARTS-RECORD.

           *> SUPPLIER
              MOVE SPACES TO SUPPLIER-RECORD.
              MOVE '**********************************************'
                   TO SUPPLIER-RECORD .
              WRITE SUPPLIER-RECORD.
              MOVE SPACES TO SUPPLIER-RECORD
              MOVE " LIST OF SUPPLIERS FROM  GOOD RECORDS"
              To SUPPLIER-RECORD.
              WRITE SUPPLIER-RECORD.
              MOVE SPACES TO SUPPLIER-RECORD
              MOVE '**********************************************'
                   TO SUPPLIER-RECORD .
              WRITE SUPPLIER-RECORD.

           *> SUPPLIER ADDRESS
              MOVE SPACES TO SUPPADDR-RECORD
              MOVE '**********************************************'
                   TO SUPPADDR-RECORD .
              WRITE SUPPADDR-RECORD.
              MOVE SPACES TO SUPPADDR-RECORD
              MOVE " LIST OF SUPPLIERS  Addresses FROM  GOOD RECORDS"
              To SUPPADDR-RECORD.
              WRITE SUPPADDR-RECORD.
              MOVE SPACES TO SUPPADDR-RECORD
              MOVE '**********************************************'
                   TO SUPPADDR-RECORD .
              WRITE SUPPADDR-RECORD.

              MOVE SPACES TO PURCHRDS-RECORD .
              MOVE '**********************************************'
                   TO PURCHRDS-RECORD .
              WRITE PURCHRDS-RECORD.
              MOVE SPACES TO PURCHRDS-RECORD.
              MOVE " LIST OF PURCHASE ORDERS FROM GOOD RECORDS"
                   TO PURCHRDS-RECORD.
              WRITE PURCHRDS-RECORD.
              MOVE SPACES TO PARTS-RECORD .
              MOVE '**********************************************'
                   TO PURCHRDS-RECORD .
              WRITE PURCHRDS-RECORD.

           *> ERROR
              MOVE SPACES TO ERROR-RECORD.
              MOVE '**********************************************'
                   TO ERROR-RECORD .
              WRITE ERROR-RECORD.
              MOVE SPACES TO ERROR-RECORD.
              MOVE " LIST OF ERROR  RECORDS.`"
               To ERROR-RECORD.
              WRITE ERROR-RECORD.
              MOVE SPACES TO ERROR-RECORD .
              MOVE '**********************************************'
                   TO ERROR-RECORD .
              WRITE ERROR-RECORD.

       615-WRITE-REC-GPARTSUPP.
              MOVE SPACES TO GPART-SUPP-RECORD
              MOVE PART-SUPP-ADDR-PO TO GPART-SUPP-RECORD
              WRITE GPART-SUPP-RECORD.

       211-WRITE-REC-PARTS.
           EVALUATE FUNCTION UPPER-CASE(IN-VEHICLEI-MAKE)
                WHEN "CHR" MOVE "CHRYSLER" TO WS-VEHICLE-MAKE
                WHEN "FOR" MOVE "FORD" TO WS-VEHICLE-MAKE
                WHEN "GM " MOVE "GM" TO WS-VEHICLE-MAKE
                WHEN "VW " MOVE "VOLKSWAGON" TO WS-VEHICLE-MAKE
                WHEN "TOY" MOVE "TOYOTA" TO WS-VEHICLE-MAKE
                WHEN "JAG" MOVE "JAGUAR" TO WS-VEHICLE-MAKE
                WHEN "PEU" MOVE "PEUGEOT" TO WS-VEHICLE-MAKE
                WHEN "BMW" MOVE "BMW" TO WS-VEHICLE-MAKE
            END-EVALUATE.
              MOVE SPACES TO PARTS-RECORD.
              STRING  'Parts info for record :' ,
                            SPACE,
                           WS-REC-CNT
                            DELIMITED BY SIZE
                           INTO PARTS-RECORD
                           END-STRING
              WRITE PARTS-RECORD.
              MOVE SPACES TO PARTS-RECORD.
              STRING IN-PART-NUMBER,  "  " ,
                     IN-PART-NAME, "  " ,
                     IN-SPEC-Number,  "   "
                     IN-GOVT-COMML-CODE, "  "
                     IN-BLUEPRINT-NUMBER, "  "
                     IN-Unit-Of-Measure, "  "
                     IN-WEEKS-LEAD-TIME, "  "
                     WS-VEHICLE-MAKE,  "  " ,
                     IN-VEHICLE-MODEL, "  ",
                     IN-VEHICLE-Year,
                     DELIMITED BY SIZE
                     INTO PARTS-RECORD
               END-STRING
              WRITE PARTS-RECORD.
              MOVE SPACES TO PARTS-RECORD .
              WRITE PARTS-RECORD .

       221-WRITE-REC-SUPPLIER.
              MOVE SPACES TO SUPPLIER-RECORD
              STRING  'SUPPLIER info for record :' ,
                            SPACE,
                           WS-REC-CNT
                            DELIMITED BY SIZE
                           INTO SUPPLIER-RECORD
                           END-STRING
              WRITE SUPPLIER-RECORD.
              MOVE SPACES TO SUPPLIER-RECORD.
              STRING IN-SUPPLIER-CODE,  "  " ,
                     IN-SUPPLIER-TYPE, "  " ,
                     IN-SUPPLIER-NAME,  "   ",
                     IN-SUPPLIER-RATING, "  ",
                     IN-SUPPLIER-STATUS,  "  ",
                     DELIMITED BY SIZE
                     INTO SUPPLIER-RECORD
               END-STRING
              WRITE SUPPLIER-RECORD .


       231-WRITE-REC-SUP-ADDR.
              MOVE SPACES TO SUPPADDR-RECORD.
              STRING  'SUPPLIER address info for record :' ,
                            SPACE,
                           WS-REC-CNT
                            DELIMITED BY SIZE
                           INTO SUPPADDR-RECORD
                           END-STRING
              WRITE SUPPADDR-RECORD.
            MOVE ZERO TO SADDR-IDX.
            PERFORM VARYING SADDR-IDX FROM 1 BY 1
            UNTIL SADDR-IDX >= TABLE-MAX-ADDR
              MOVE IN-ADDRESS-TYPE(SADDR-IDX)  TO ADDRESS-TYPE
              MOVE IN-ADDRESS-1(SADDR-IDX)     TO ADDRESS-1
              MOVE IN-ADDRESS-2(SADDR-IDX)     TO ADDRESS-2
              MOVE IN-ADDRESS-3(SADDR-IDX)     TO ADDRESS-3
              MOVE IN-CITY(SADDR-IDX)          TO CITY
              MOVE IN-ADDR-STATE(SADDR-IDX)    TO ADDR-STATE
              MOVE IN-ZIP-CODE(SADDR-IDX)      TO ZIP-CODE
              WRITE SUPPADDR-RECORD FROM SUPP-ADDRESS
            END-PERFORM.
            MOVE SPACES TO SUPPADDR-RECORD  .
            WRITE SUPPADDR-RECORD .

       241-WRITE-REC-PURCHRDS.
              MOVE SPACES TO PURCHRDS-RECORD.
              STRING  'Purchase Orders for record :' ,
                            SPACE,
                           WS-REC-CNT
                            DELIMITED BY SIZE
                           INTO PURCHRDS-RECORD
                           END-STRING
              WRITE PURCHRDS-RECORD.
           MOVE ZERO  TO PROJ-IDX
           PERFORM VARYING PROJ-IDX FROM 1 BY 1
                    UNTIL PROJ-IDX >= TABLE-MAX
              MOVE IN-PO-NUMBER(PROJ-IDX)     TO PO-NUMBER
              MOVE IN-BUYER-CODE(PROJ-IDX)    TO BUYER-CODE
              MOVE IN-QUANTITY(PROJ-IDX)      TO QUANTITY
              MOVE IN-UNIT-PRICE(PROJ-IDX)    TO UNIT-PRICE
              MOVE IN-ORDER-DATE(PROJ-IDX)    TO ORDER-DATE
              MOVE IN-DELIVERY-DATE(PROJ-IDX) TO DELIVERY-DATE
              WRITE PURCHRDS-RECORD FROM PURCHASE-ORDERS
           END-PERFORM.
            MOVE SPACES TO PURCHRDS-RECORD  .
            WRITE PURCHRDS-RECORD .
      * 241-WRITE-REC-PURCHRDS.
       800-ERROR_WRITING.
       *> we are here because WS-ALL-ERROR-CNT is  not ZERO
              MOVE SPACES TO ERROR-RECORD
              STRING  'Error in record :' ,
                            SPACE,
                           WS-REC-CNT
                            DELIMITED BY SIZE
                           INTO ERROR-RECORD
                           END-STRING
              WRITE ERROR-RECORD.

              MOVE SPACES TO ERROR-RECORD
              STRING  'Number of Errors  :' ,
                            SPACE,
                             WS-ALL-ERROR-CNT
                            DELIMITED BY SIZE
                           INTO ERROR-RECORD
                           END-STRING
              WRITE ERROR-RECORD.

              MOVE SPACES TO ERROR-RECORD
              IF WS-ALL-ERROR-CNT > 3 then
               *> need to  finish else !!!!
                 STRING  'Bad record  - more than 3 errors detected :',
                            SPACE,
                           WS-ALL-ERROR-CNT
                            DELIMITED BY SIZE
                           INTO ERROR-RECORD
                           END-STRING
                 WRITE ERROR-RECORD
              ELSE   *> we have some  errors
                IF WS-PARTS-ERROR-NUM > 0  then
                 PERFORM VARYING PROJ-IDX FROM 1 BY 1
                    UNTIL PROJ-IDX > WS-PARTS-ERROR-NUM
                    MOVE SPACES TO ERROR-RECORD
                    STRING  WS-PARTS-MSG-NO(PROJ-IDX)
                            SPACE,
                            WS-PARTS-MSG-TEXT(PROJ-IDX)
                            DELIMITED BY SIZE
                           INTO ERROR-RECORD
                           END-STRING
                   WRITE ERROR-RECORD
                END-PERFORM
                END-IF  *> PARTS
                IF WS-SUPPLIER-ERROR-NUM > 0  then
                 PERFORM VARYING PROJ-IDX FROM 1 BY 1
                    UNTIL PROJ-IDX > WS-SUPPLIER-ERROR-NUM
                    MOVE SPACES TO ERROR-RECORD
                    STRING  WS-SUPPLIER-MSG-NO(PROJ-IDX)
                            SPACE,
                            WS-SUPPLIER-MSG-TEXT(PROJ-IDX)
                            DELIMITED BY SIZE
                           INTO ERROR-RECORD
                           END-STRING
                   WRITE ERROR-RECORD
                 END-PERFORM
                END-IF *> Supplier

                IF WS-SUPPADDR-ERROR-NUM > 0  then
                 PERFORM VARYING PROJ-IDX FROM 1 BY 1
                    UNTIL PROJ-IDX > WS-SUPPADDR-ERROR-NUM
                    MOVE SPACES TO ERROR-RECORD
                    STRING  WS-SUPPADDR-MSG-NO(PROJ-IDX)
                            SPACE,
                            WS-SUPPADDR-MSG-TEXT(PROJ-IDX)
                            DELIMITED BY SIZE
                           INTO ERROR-RECORD
                           END-STRING
                   WRITE ERROR-RECORD
                 END-PERFORM
                END-IF *> supplier address

                IF WS-PURCHRDS-ERROR-NUM > 0  then
                 PERFORM VARYING PROJ-IDX FROM 1 BY 1
                    UNTIL PROJ-IDX > WS-PURCHRDS-ERROR-NUM
                    MOVE SPACES TO ERROR-RECORD
                    IF NOT WS-PURCHRDS-MSG-NO(PROJ-IDX) = SPACES THEN
                       STRING  WS-PURCHRDS-MSG-NO(PROJ-IDX)
                               SPACE,
                               WS-PURCHRDS-MSG-TEXT(PROJ-IDX)
                               DELIMITED BY SIZE
                              INTO ERROR-RECORD
                              END-STRING
                      WRITE ERROR-RECORD
                    END-IF
                 END-PERFORM
                 MOVE " DETAILS of errors in  Purchase Orders:" to
                 ERROR-RECORD
                 WRITE ERROR-RECORD
                 IF WS-PURCHRDS1-ERROR-NUM > 0
                    MOVE " Purchase Order 1 " to  ERROR-RECORD
                    WRITE ERROR-RECORD
                    PERFORM VARYING PROJ-IDX FROM 1 BY 1
                       UNTIL PROJ-IDX > WS-PURCHRDS1-ERROR-NUM
                       MOVE SPACES TO ERROR-RECORD
                       IF NOT (WS-PURCHRDS1-MSG-NO(PROJ-IDX) = SPACES)
                          STRING  WS-PURCHRDS1-MSG-NO(PROJ-IDX)
                                  SPACE,
                                  WS-PURCHRDS1-MSG-TEXT(PROJ-IDX)
                                  DELIMITED BY SIZE
                                 INTO ERROR-RECORD
                         END-STRING
                         WRITE ERROR-RECORD
                       END-IF
                    END-PERFORM
                 END-IF
                 IF WS-PURCHRDS2-ERROR-NUM > 0
                    MOVE " Purchase Order 2 " to  ERROR-RECORD
                    WRITE ERROR-RECORD
                    PERFORM VARYING PROJ-IDX FROM 1 BY 1
                       UNTIL PROJ-IDX > WS-PURCHRDS2-ERROR-NUM
                       MOVE SPACES TO ERROR-RECORD
                       IF NOT (WS-PURCHRDS2-MSG-NO(PROJ-IDX) = SPACES)
                          STRING  WS-PURCHRDS2-MSG-NO(PROJ-IDX)
                                  SPACE,
                                  WS-PURCHRDS2-MSG-TEXT(PROJ-IDX)
                                  DELIMITED BY SIZE
                                 INTO ERROR-RECORD
                                 END-STRING
                         WRITE ERROR-RECORD
                       END-IF
                    END-PERFORM
                 IF WS-PURCHRDS3-ERROR-NUM > 0 then
                    MOVE " Purchase Order 3 " to  ERROR-RECORD
                    WRITE ERROR-RECORD
                     PERFORM VARYING PROJ-IDX FROM 1 BY 1
                        UNTIL PROJ-IDX > WS-PURCHRDS3-ERROR-NUM
                        MOVE SPACES TO ERROR-RECORD
                           STRING  WS-PURCHRDS3-MSG-NO(PROJ-IDX)
                                   SPACE,
                                   WS-PURCHRDS3-MSG-TEXT(PROJ-IDX)
                                   DELIMITED BY SIZE
                                  INTO ERROR-RECORD
                            END-STRING
                            WRITE ERROR-RECORD
                     END-PERFORM
                 END-IF
                END-IF *> Purchase Orders
              END-IF. *>WS-ALL-ERROR-CNT > 3
              MOVE SPACES TO ERROR-RECORD .
              WRITE ERROR-RECORD .
       900-WRAP-UP.
            CLOSE PARTSUPPFILE, PURCHRDS-FILE, SUPPLIER-FILE,
                 PARTS-FILE, SUPPADDR-FILE, GPART-SUPP-FILE,
                 ERROR-FILE.
       999-ERR-RTN.
           GOBACK.























