       01  PART-SUPP-ADDR-PO.
           05 IN-PARTS.
              10  IN-PART-NUMBER       PIC X(23) VALUE SPACES.
              10  IN-PART-NAME         PIC X(14) VALUE SPACES.
              10  IN-SPEC-NUMBER       PIC X(07) VALUE SPACES.
              10  IN-GOVT-COMML-CODE   PIC X(01) VALUE SPACES.
              10  IN-BLUEPRINT-NUMBER  PIC X(10) VALUE SPACES.
              10  IN-UNIT-OF-MEASURE   PIC X(03) VALUE SPACES.
              10  IN-WEEKS-LEAD-TIME   PIC 9(03) VALUE ZERO.
              10  IN-VEHICLEI-MAKE     PIC X(03) VALUE SPACES.
                  88 IN-CHRYSLER       VALUE 'CHR'.
                  88 IN-FORD           VALUE 'FOR'.
                  88 IN-GM             VALUE 'GM '.
                  88 IN-VOLKSWAGON     VALUE 'VW '.
                  88 IN-TOYOTA         VALUE 'TOY'.
                  88 IN-JAGUAR         VALUE 'JAG'.
                  88 IN-PEUGEOT        VALUE 'PEU'.
                  88 IN-BMW            VALUE 'BMW'.
              10  IN-VEHICLE-MODEL     PIC X(10) VALUE SPACES.
              10  IN-VEHICLE-YEAR      PIC X(04) VALUE '0000'.
              10  FILLER               PIC X(14) VALUE SPACES.
           05 IN-SUPPLIERS.
              10  IN-SUPPLIER-CODE     PIC X(10) VALUE SPACES.
              10  IN-SUPPLIER-TYPE     PIC X(01) VALUE SPACES.
                  88 IN-SUBCONTRACTOR  VALUE 'S'.
                  88 IN-DISTRIBUTOR    VALUE 'D'.
                  88 IN-MANUFACTURER   VALUE 'M'.
                  88 IN-IMPORTER       VALUE 'I'.
              10  IN-SUPPLIER-NAME     PIC X(15) VALUE SPACES.
              10  IN-SUPPLIER-PERF     PIC 9(03) VALUE ZERO.
              10  IN-SUPPLIER-RATING   PIC X(01) VALUE SPACES.
                  88 IN-HIGHEST-QUALITY VALUE '3'.
                  88 IN-AVERAGE-QUALITY VALUE '2'.
                  88 IN-LOWEST-QUALITY  VALUE '1'.
              10  IN-SUPPLIER-STATUS   PIC X(01) VALUE SPACES.
                  88 IN-GOVT-COMM       VALUE '1'.
                  88 IN-GOVT-ONLY       VALUE '2'.
                  88 IN-COMMERCIAL-ONLY VALUE '3'.
              10  IN-SUPPLIER-ACT-DATE PIC 9(08) VALUE ZERO.
           05 IN-SUPP-ADDRESS OCCURS 3 TIMES INDEXED BY ADDR-IDX.
              10 IN-ADDRESS-TYPE      PIC X(01) VALUE SPACES.
                 88 IN-ORDER-ADDRESS           VALUE '1'.
                 88 IN-SCHED-ADDRESS           VALUE '2'.
                 88 IN-REMIT-ADDRESS           VALUE '3'.
              10 IN-ADDRESS-1         PIC X(15) VALUE SPACES.
              10 IN-ADDRESS-2         PIC X(15) VALUE SPACES.
              10 IN-ADDRESS-3         PIC X(15) VALUE SPACES.
              10 IN-CITY              PIC X(15) VALUE SPACES.
              10 IN-ADDR-STATE        PIC X(02) VALUE SPACES.
              10 IN-ZIP-CODE          PIC 9(10) VALUE ZERO.
           05 IN-PURCHASE-ORDER OCCURS 3 TIMES INDEXED BY PO-IDX.
              10  IN-PO-NUMBER         PIC X(06) VALUE SPACES.
              10  IN-BUYER-CODE        PIC X(03) VALUE SPACES.
              10  IN-QUANTITY          PIC S9(7) VALUE ZERO.
              10  IN-UNIT-PRICE        PIC S9(7)V99 VALUE ZERO.
              10  IN-ORDER-DATE        PIC 9(08) VALUE ZERO.
              10  IN-DELIVERY-DATE      PIC 9(08) VALUE ZERO.
