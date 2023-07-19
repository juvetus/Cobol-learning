       IDENTIFICATION DIVISION.
       PROGRAM-ID. PETSTORECHALLENGE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT PETSALESFILE ASSIGN TO "PETSTORESALES.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
       SELECT REPORTFILE ASSIGN TO "SALESREPORT1.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD PETSALESFILE.
       01 SALESDETAILS.
           88 ENDOFSALESFILE VALUE HIGH-VALUES.
           02 CUSTOMER-ID      PIC 9(7).
             02 CUSTOMERNAME.
           05 LASTNAME     PIC X(15).
           05 FIRSTNAME    PIC X(15).
       02 PETITEM OCCURS 3 TIMES.
           05 DESCRIPTION      PIC X(20).
           05 PRICE            PIC 999999V99.
           05 QUANTITY         PIC 99999.
       
       FD REPORTFILE.
       01 REPORT-LINE PIC X(80).
       
       WORKING-STORAGE SECTION.
       01 WS-FIELDS.
           05 WS-TOTAL-QUANT   PIC 999.
           05 WS-ITEM-TOTAL    PIC 9999V99.
           05 WS-TOTAL-SALE    PIC 99999V99.
       
       01 WS-DATE.
           05 WS-YEAR PIC 99.
           05 WS-MONTH PIC 99.
           05 WS-DAY   PIC 99.
       
       01 HEADING-LINE.
           05 FILLER        PIC X(16) VALUE 'CUSTOMER NAME'.
           05 FILLER        PIC X(20) VALUE 'ITEM DESCRIPTION'.
           05 FILLER        PIC X(2) VALUE SPACES.
           05 FILLER        PIC X(11)  VALUE 'PRICE'.
           05 FILLER        PIC X(2) VALUE SPACES.
           05 FILLER        PIC X(11)  VALUE 'QUANTITY'.
           05 FILLER        PIC X(2) VALUE SPACES.
           05 FILLER        PIC X(11)  VALUE 'TOTAL'.
       
       01 DETAIL-LINE.
           05 DET-CUSTOMERNAME  PIC X(30).
           05 FILLER            PIC X(10)  VALUE SPACES.
           05 DET-DESCRIPTION   PIC X(20).
           05 FILLER            PIC X(9)   VALUE SPACES.
           05 DET-PRICE         PIC $,$$9.99.
           05 FILLER            PIC X(8)   VALUE SPACES.
           05 DET-QUANTITY      PIC Z9.
           05 FILLER            PIC X(7)   VALUE SPACES.
           05 DET-ITEM-TOTAL    PIC $$,$$9.99.
       
       01 DETAIL-TOTAL-LINE.
           05 FILLER            PIC X(30) VALUE SPACES.
           05 FILLER            PIC X(19)  VALUE "    TOTAL QUANTITY: ".
           05 DET-TOTAL-QUANT   PIC 999.
           05 FILLER            PIC XX.
           05 FILLER            PIC X(23)  VALUE "TOTAL AMOUNT: ".
           05 FILLER            PIC X(1)   VALUE SPACES.
           05 DET-TOT-SALES     PIC $$,$$$,$$9.99.
           05 FILLER            PIC X(3)   VALUE SPACES.
       
       01 REPORT-TOTAL-LINE.
           05 FILLER            PIC X(30) VALUE SPACES.
           05 FILLER            PIC X(19)  VALUE "CUSTOMER TOTAL: ".
           05 FILLER            PIC X(10)   VALUE SPACES.
           05 REPORT-TOT-SALES  PIC $$,$$$,$$9.99.
           05 FILLER            PIC X(3)   VALUE SPACES.
       
       PROCEDURE DIVISION.
       0100-START.
           OPEN INPUT PETSALESFILE
           OPEN OUTPUT REPORTFILE
             READ PETSALESFILE
           AT END SET ENDOFSALESFILE TO TRUE
             END-READ
           WRITE REPORT-LINE FROM HEADING-LINE.
       
           PERFORM 0200-PROCESS-ITEMS UNTIL ENDOFSALESFILE
           PERFORM 0290-PRINT-TOTAL
           PERFORM 0300-STOP-RUN.
       
       0100-END.
       
       0200-PROCESS-ITEMS.
           MOVE CUSTOMERNAME TO DET-CUSTOMERNAME.
           MOVE DESCRIPTION(1) TO DET-DESCRIPTION.
           MOVE PRICE(1) TO DET-PRICE.
           MOVE QUANTITY(1) TO DET-QUANTITY.
           COMPUTE WS-ITEM-TOTAL = PRICE(1) * QUANTITY(1).
           COMPUTE WS-TOTAL-SALE = WS-TOTAL-SALE + WS-ITEM-TOTAL.
           COMPUTE WS-TOTAL-QUANT = WS-TOTAL-QUANT + QUANTITY(1).
           MOVE WS-ITEM-TOTAL TO DET-ITEM-TOTAL.
           WRITE REPORT-LINE FROM DETAIL-LINE
             READ PETSALESFILE
           AT END SET ENDOFSALESFILE TO TRUE
             END-READ.
       
       0200-END.
       
       0290-PRINT-TOTAL.
           MOVE WS-TOTAL-QUANT TO DET-TOTAL-QUANT.
           MOVE WS-TOTAL-SALE TO DET-TOT-SALES.
           WRITE REPORT-LINE FROM DETAIL-TOTAL-LINE.
           WRITE REPORT-LINE FROM REPORT-TOTAL-LINE.
           MOVE ZEROES TO WS-TOTAL-QUANT WS-TOTAL-SALE.
       
       0290-END.
       
       0300-STOP-RUN.
           CLOSE PETSALESFILE
           CLOSE REPORTFILE
           STOP RUN.
       
       END PROGRAM PETSTORECHALLENGE.
       