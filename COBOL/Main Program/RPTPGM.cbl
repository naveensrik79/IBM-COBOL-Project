       IDENTIFICATION DIVISION.
       PROGRAM-ID.     RPTPGM.
      * **************************************************
      * RPTPGM PROGRAM READ GPARTSUP(WHICH CONTAINS ONLY GOOD
      * RECORD). HAVE ADDED EVALUATE AND OTHER CONDITIONS TO
      * PREPARE THE REQUIRED REPORT AS PER SLIDE # 598.
      * 400-PROCESS-DATA PARA USED TO CALCULATE AND PERFORM VARIOUS
      * STEPS TO PAPULATE PART NAME, WEEKS LEAD TIME, VEHICLE MAKE,
      * SUPPLIER NAME AND SUPPLIER RATING.
      * THERE ARE THREE ADDRESSES IN THE REPORT, HAVE CITY, STATE AND
      * ZIP CODE.
      * TOTAL PURCHASE ORDER FOR ONE PARTNUMBER
      * TOTLA PROCE PURCHASE ORDER AND TOTAL QUNATITY IN PURCHASE ORDER
      * AS WELL TAKING CARE IN THE SAME PARA.
      * UT-C-GPARTSUP = UTILITY SEQUENTIALS FILE GPARTSUP
      * UT-C-RPT = SYSOUT REPORT FILE LANDING IN JES.
      * JCL WILL LOOK ONLY LAST WORD OF THE FILE...HERE UT-S-GPARTSUP
      * JCL WILL LOOK FOR THE GPARTSUP. UT-S ARE IGNORED BY JCL
      *//GPARTSUP DD DSN=USER60.COBOLPRJ.GPARTSUP,DISP=SHR
      * //RPT      DD SYSOUT=*
      * **************************************************
       AUTHOR.         COBWO. *>COBOL WORIRER
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * SORTED GOOD PURCHASE,SUPPLIER AND SUPPADDRESS FILE
           SELECT PART-SUPP-FILE ASSIGN TO UT-C-GPARTSUP
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS  IS PART-SUPP-ST.
      * PURCHASE ORDER REPORT
           SELECT REPORT-FILE ASSIGN TO UT-C-RPT
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS  IS RPT-ST.
       DATA DIVISION.
       FILE SECTION.
       FD PART-SUPP-FILE
           RECORD CONTAINS 480 CHARACTERS
           RECORDING MODE IS F.
       01 PART-SUPP-RECORD      PIC X(480).
       FD REPORT-FILE
           RECORD CONTAINS 132 CHARACTERS
           RECORDING MODE IS F.
       01 REPORT-RECORD      PIC X(132).
       WORKING-STORAGE SECTION.
       77 WS01-REC-CNT              PIC 9(08) VALUE ZERO.
       77 WS01-TABLE-MAX            PIC 9(01) VALUE 4.
       77 WS-SPAD-IDX               PIC 9(01) VALUE ZERO.
       77 WS01-PURO-IDX             PIC 9(01) VALUE ZERO.
       77 WS01-TOTAL-PO             PIC 9(03) VALUE ZERO.
       77 WS01-TOTAL-QUANTITY-PO    PIC 9(8)V99 VALUE 0.
       77 WS01-TOTAL-UNIT-PRICE-PO  PIC 9(7)V99 VALUE 0.
       77 WS01-GTOTAL-QUANTITY-PO   PIC 9(8)V99 VALUE 0.
       77 WS01-GTOTAL-UNIT-PRICE-PO PIC 9(07)V99 VALUE 0.
       77 WS01-LEAD-WEEKS           PIC 9(03) VALUE ZEROS.
       77 WS01-RPT-ZIP-CODE         PIC 9(10) VALUE 0.
       77 WS01-RPT-SCHD-ZIP-CODE    PIC 9(10) VALUE 0.
       77 WS01-RPT-SCHD-ZIP-CODE    PIC 9(10) VALUE 0.
       01 PROGRAM-SWITCHES.
           05 PART-SUPP-FILE-EOF            PIC X(01) VALUE 'N'.
               88 NO-MORE-PART-SUPP-FILE    VALUE 'Y'.
           05 PART-SUPP-ST                  PIC X(02).
               88 PART-SUPP-FILE-OK         VALUE '00'.
               88 PART-SUPP-EOF-OK          VALUE '10'.
           05 RPT-ST                        PIC X(02).
               88 RPT-ST-OK                 VALUE '00'.
            05 PARTSUPPFILE-EOF             PIC X(1) VALUE 'N'.
               88 NO-MORE-PARTSUPPFILE      VALUE 'Y'.
            05 PARTSUPP-ST                  PIC X(02).
               88 PARTSUPPFILE-OK           VALUE '00'.
       01 RPT-LINE-0.
           05 FILLER PIC X(132) VALUE all " ".
       01 RPT-LINE-1.
             10 FILLER    PIC X(15) VALUE '   Part Name   '.
             10 FILLER    PIC X(10) VALUE SPACES.
             10 FILLER    PIC X(15) VALUE 'Weeks Lead Time'.
             10 FILLER    PIC X(9) VALUE SPACES.
             10 FILLER    PIC X(15) VALUE ' Vechile Make  '.
             10 FILLER    PIC X(10) VALUE SPACES.
             10 FILLER    PIC X(15) VALUE ' Supplier Name '.
             10 FILLER    PIC X(11) VALUE SPACES.
             10 FILLER    PIC X(15) VALUE 'Supplier Rating'.
             10 FILLER    PIC X(17) VALUE SPACES.
       01 RPT-LINE-2.
             10 FILLER    PIC X(15) VALUE '==============='.
             10 FILLER    PIC X(10) VALUE SPACES.
             10 FILLER    PIC X(15) VALUE '==============='.
             10 FILLER    PIC X(10) VALUE SPACES.
             10 FILLER    PIC X(15) VALUE '==============='.
             10 FILLER    PIC X(10) VALUE SPACES.
             10 filler    PIC X(15) value "===============".
             10 FILLER    PIC X(10) VALUE SPACES.
             10 FILLER    PIC X(15) VALUE "===============".
             10 FILLER    PIC X(17) VALUE SPACES.
       01 RPT-LINE-3.
             10 RPT-PART-NAME    PIC X(15) VALUE SPACES.
             10 FILLER           PIC X(16) VALUE SPACES.
             10 RPT-WEEKS-LEAD-TIME PIC ZZZ.
             10 FILLER           PIC X(16) VALUE SPACES.
             10 RPT-VEHICLE-MAKE PIC X(10) VALUE SPACES.
             10 FILLER           PIC X(15) VALUE SPACES.
             10 RPT-SUPPIER-NAME PIC X(15) VALUE SPACES.
             10 FILLER           PIC X(10) VALUE SPACES.
             10 RPT-SUPPLIER-RATING PIC X(15) VALUE SPACES.
       01 RPT-LINE-4.
             10 FILLER   PIC X(16) VALUE 'Order Address:  '.
             10 RPT-ORDR-ADDR   PIC X(15) VALUE SPACES.
             10 RPT-ORDR-CITY PIC X(15) VALUE SPACES.
             10 FILLER PIC X VALUE SPACES.
             10 RPT-ORDR-ADDR-STATE PIC X(02) VALUE SPACES.
             10 FILLER PIC X VALUE SPACES.
             10 RPT-ORDR-ZIP-CODE   PIC 9(10) VALUE 0.
       01 RPT-LINE-5.
              10 FILLER   PIC X(16) VALUE 'Sched Address:  '.
              10 RPT-SCHED-ADDR   PIC X(15) VALUE SPACES.
              10 RPT-SCHED-CITY PIC X(15) VALUE SPACES.
              10 FILLER PIC X VALUE SPACES.
              10 RPT-SCHED-ADDR-STATE PIC X(02) VALUE SPACES.
              10 FILLER PIC X VALUE SPACES.
              10 RPT-SCHED-ZIP-CODE   PIC 9(10) VALUE 0.
       01 RPT-LINE-6.
              10 FILLER   PIC X(16) VALUE 'Remit Address:  '.
              10 RPT-REMIT-ADDR   PIC X(15) VALUE SPACES.
              10 RPT-REMT-CITY PIC X(15) VALUE SPACES.
              10 FILLER PIC X VALUE SPACES.
              10 RPT-REMT-ADDR-STATE PIC X(02) VALUE SPACES.
              10 FILLER PIC X VALUE SPACES.
             10 RPT-REMT-ZIP-CODE   PIC 9(10) VALUE 0.
       01 RPT-LINE-7.
              10 FILLER           PIC X(132) VALUE ALL " ".
       01 RPT-LINE-8.
              10 FILLER           PIC X(25) VALUE
               'Total # Purchase Orders: '.
              10 RPT-TOTAL-PO     PIC ZZZ.
       01 RPT-LINE-9.
              10 FILLER PIC X(29) VALUE
                   'Total Price Purchase Orders: '.
              10 RPT-TOTAL-PRICE-PO PIC ZZZ,ZZZ,9.99.
       01 RPT-LINE-10.
             10 FILLER PIC X(35) VALUE
                'Total Quantity in Purchase Orders: '.
             10 RPT-TOT-QTY-PO   PIC $$,$$$,$$9.99.

           COPY PRTSUBAD.

       PROCEDURE DIVISION.
           PERFORM 100-HOUSEKEEPING.
           PERFORM 300-READ-DATA.
           PERFORM 400-PROCESS-DATA UNTIL NO-MORE-PART-SUPP-FILE.
           PERFORM 900-WRAP-UP.
           GOBACK.
       100-HOUSEKEEPING.
           INITIALIZE WS01-REC-CNT, WS01-TOTAL-UNIT-PRICE-PO,
                      WS01-TOTAL-QUANTITY-PO, WS01-TOTAL-PO.
              PERFORM 200-OPEN-FILES.
       200-OPEN-FILES.
              OPEN INPUT PART-SUPP-FILE
                IF NOT PART-SUPP-FILE-OK
                   DISPLAY 'Input File PARTSUPPFILE File Error'
              GO TO 999-ERR-RTN.
              OPEN OUTPUT REPORT-FILE
              IF NOT RPT-ST-OK
                 DISPLAY 'Output REPORT is Error'
                 GO TO 999-ERR-RTN.
       300-READ-DATA.
                  READ PART-SUPP-FILE INTO PART-SUPP-ADDR-PO
                  AT END
                  MOVE "Y" TO PART-SUPP-FILE-EOF.
       400-PROCESS-DATA.
      *collect    Number of record
                ADD +1 TO WS01-REC-CNT.
                MOVE IN-PART-NAME       TO RPT-PART-NAME
                MOVE IN-WEEKS-LEAD-TIME TO WS01-LEAD-WEEKS
                MOVE WS01-LEAD-WEEKS    TO RPT-WEEKS-LEAD-TIME
      *         MOVE IN-VEHICLEI-MAKE   TO RPT-VEHICLE-MAKE
            EVALUATE IN-VEHICLEI-MAKE
             WHEN 'CHR' MOVE "CHRYSLER" TO RPT-VEHICLE-MAKE
             WHEN 'FOR' MOVE "FORD" TO RPT-VEHICLE-MAKE
             WHEN 'GM ' MOVE "GM"   TO RPT-VEHICLE-MAKE
             WHEN 'VW ' MOVE "VOLKSWAGON" TO RPT-VEHICLE-MAKE
             WHEN 'TOY' MOVE "TOYOTA"     TO RPT-VEHICLE-MAKE
             WHEN 'JAG' MOVE "JAGUAR"     TO RPT-VEHICLE-MAKE
             WHEN 'PEU' MOVE "PEUGEOT"    TO RPT-VEHICLE-MAKE
             WHEN 'BMW' MOVE "BMW"        TO RPT-VEHICLE-MAKE
             WHEN OTHER
                 MOVE "INVALID MAKE" TO RPT-VEHICLE-MAKE
           END-EVALUATE
                MOVE IN-SUPPLIER-NAME   TO RPT-SUPPIER-NAME
      *         MOVE IN-SUPPLIER-RATING TO RPT-SUPPLIER-RATING
           EVALUATE IN-SUPPLIER-RATING
             WHEN '3' MOVE "HIGHEST-QUALITY" TO RPT-SUPPLIER-RATING
             WHEN '2' MOVE "AVERAGE-QUALITY" TO RPT-SUPPLIER-RATING
             WHEN '1' MOVE "LOWEST-QUALITY"  TO RPT-SUPPLIER-RATING
             WHEN OTHER
                      MOVE "UN-KNOWN RATING" TO RPT-SUPPLIER-RATING
            MOVE ZERO TO WS-SPAD-IDX
           END-EVALUATE

           PERFORM VARYING WS-SPAD-IDX FROM 1 BY 1
            UNTIL WS-SPAD-IDX >= WS01-TABLE-MAX
             IF IN-ADDRESS-TYPE(WS-SPAD-IDX) = '1'
                MOVE IN-ADDRESS-1(WS-SPAD-IDX)  TO RPT-ORDR-ADDR
                MOVE IN-CITY(WS-SPAD-IDX)       TO RPT-ORDR-CITY
                MOVE IN-ADDR-STATE(WS-SPAD-IDX) TO RPT-ORDR-ADDR-STATE
                MOVE IN-ZIP-CODE(WS-SPAD-IDX)   TO WS01-RPT-ZIP-CODE
                MOVE WS01-RPT-ZIP-CODE          TO RPT-ORDR-ZIP-CODE
              END-IF
             IF IN-ADDRESS-TYPE(WS-SPAD-IDX) = '2'
                MOVE IN-ADDRESS-1(WS-SPAD-IDX)  TO RPT-SCHED-ADDR
                MOVE IN-CITY(WS-SPAD-IDX)       TO RPT-SCHED-CITY
                MOVE IN-ADDR-STATE(WS-SPAD-IDX) TO RPT-SCHED-ADDR-STATE
                MOVE IN-ZIP-CODE(WS-SPAD-IDX)   TO WS01-RPT-ZIP-CODE
                MOVE WS01-RPT-ZIP-CODE          TO RPT-SCHED-ZIP-CODE
             END-IF
             IF IN-ADDRESS-TYPE(WS-SPAD-IDX) = '3'
                MOVE IN-ADDRESS-1(WS-SPAD-IDX)  TO RPT-REMIT-ADDR
                MOVE IN-CITY(WS-SPAD-IDX)       TO RPT-REMT-CITY
                MOVE IN-ADDR-STATE(WS-SPAD-IDX) TO RPT-REMT-ADDR-STATE
                MOVE IN-ZIP-CODE(WS-SPAD-IDX)   TO WS01-RPT-ZIP-CODE
                MOVE WS01-RPT-ZIP-CODE          TO RPT-REMT-ZIP-CODE
            END-IF
           END-PERFORM.

                MOVE ZERO TO WS01-TOTAL-QUANTITY-PO,
                MOVE ZERO TO WS01-TOTAL-UNIT-PRICE-PO.
                MOVE ZERO TO WS01-PURO-IDX.
                MOVE ZERO TO WS01-TOTAL-PO.
           PERFORM VARYING WS01-PURO-IDX FROM 1 BY 1
                   UNTIL WS01-PURO-IDX >= WS01-TABLE-MAX
              IF IN-PO-NUMBER(WS01-PURO-IDX) NOT EQUAL SPACES
                 ADD +1 TO WS01-TOTAL-PO
              END-IF
              IF IN-QUANTITY(WS01-PURO-IDX) >= 0
               ADD IN-QUANTITY(WS01-PURO-IDX) TO WS01-TOTAL-QUANTITY-PO
              END-IF
              IF IN-UNIT-PRICE(WS01-PURO-IDX) >= 0
                 ADD IN-UNIT-PRICE(WS01-PURO-IDX) TO
                                    WS01-TOTAL-UNIT-PRICE-PO
              END-IF
           END-PERFORM.
                 MOVE WS01-TOTAL-PO            TO RPT-TOTAL-PO.
                 MOVE WS01-TOTAL-QUANTITY-PO   TO RPT-TOT-QTY-PO.
                 MOVE WS01-TOTAL-UNIT-PRICE-PO TO RPT-TOTAL-PRICE-PO.
           PERFORM 500-WRITE-REPORT.
       500-WRITE-REPORT.
              WRITE REPORT-RECORD FROM RPT-LINE-0
              WRITE REPORT-RECORD FROM RPT-LINE-1
              WRITE REPORT-RECORD FROM RPT-LINE-2
              WRITE REPORT-RECORD FROM RPT-LINE-3
              WRITE REPORT-RECORD FROM RPT-LINE-0
              WRITE REPORT-RECORD FROM RPT-LINE-4
              WRITE REPORT-RECORD FROM RPT-LINE-5
              WRITE REPORT-RECORD FROM RPT-LINE-6
              WRITE REPORT-RECORD FROM RPT-LINE-7
              WRITE REPORT-RECORD FROM RPT-LINE-8
              WRITE REPORT-RECORD FROM RPT-LINE-9
              WRITE REPORT-RECORD FROM RPT-LINE-10
              PERFORM 300-READ-DATA.
       900-WRAP-UP.
               CLOSE PART-SUPP-FILE, REPORT-FILE.
       999-ERR-RTN.
            GOBACK.

