       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEEMERGE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ACMEFILE ASSIGN TO "ACME.DAT"
          ORGANIZATION IS LINE SEQUENTIAL
          FILE STATUS IS ACME-FILE-STATUS.

       SELECT FUSESINCFILE ASSIGN TO "FUSESINC.DAT"
          ORGANIZATION IS LINE SEQUENTIAL
          FILE STATUS IS FUSESINC-FILE-STATUS.

       SELECT MERGEDFILE ASSIGN TO "MERGED.DAT"
          ORGANIZATION IS LINE SEQUENTIAL
          FILE STATUS IS MERGED-FILE-STATUS.

       SELECT SORTEDFILE ASSIGN TO "SORTED.DAT"
          ORGANIZATION IS LINE SEQUENTIAL.

       SELECT REPORTFILE ASSIGN TO "EMPLOYEES_REPORT.TXT"
          ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD ACMEFILE.
       01 ACME-EMPLOYEE-RECORD.
          02 ACME-EMPLOYEE-ID PIC 9(7).
          02 ACME-EMPLOYEE-NAME PIC X(20).
          02 ACME-SSN PIC X(9).

       FD FUSESINCFILE.
       01 FUSESINC-EMPLOYEE-RECORD.
          02 FUSESINC-EMPLOYEE-ID PIC 9(7).
          02 FUSESINC-EMPLOYEE-NAME PIC X(20).
          02 FUSESINC-SSN PIC X(9).

       FD MERGEDFILE.
       01 MERGED-EMPLOYEE-RECORD.
          02 MERGED-EMPLOYEE-ID PIC 9(7).
          02 MERGED-EMPLOYEE-NAME PIC X(20).
          02 MERGED-SSN PIC X(9).

       FD SORTEDFILE.
       01 SORTED-EMPLOYEE-RECORD.
          02 SORTED-EMPLOYEE-ID PIC 9(7).
          02 SORTED-EMPLOYEE-NAME PIC X(20).
          02 SORTED-SSN PIC X(9).

       FD REPORTFILE.
       01 REPORT-LINE PIC X(80).

       WORKING-STORAGE SECTION.
       01 WS-EMPLOYEE-COUNT PIC 9(5) VALUE ZERO.
       01 ACME-FILE-STATUS PIC XX.
       01 FUSESINC-FILE-STATUS PIC XX.
       01 MERGED-FILE-STATUS PIC XX.

       PROCEDURE DIVISION.
       0050-START.
          PERFORM 0100-MERGE-EMPLOYEES
          PERFORM 0200-SORT-MERGED-EMPLOYEES
          PERFORM 0300-GENERATE-REPORT
          PERFORM 9000-END-PROGRAM.

       0100-MERGE-EMPLOYEES.
          OPEN INPUT ACMEFILE, FUSESINCFILE
          OPEN OUTPUT MERGEDFILE
          MOVE ZERO TO WS-EMPLOYEE-COUNT
          PERFORM UNTIL ACME-FILE-STATUS = "10" AND FUSESINC-FILE-STATUS = "10"
             READ ACMEFILE
                AT END MOVE "10" TO ACME-FILE-STATUS
                NOT AT END
                   MOVE ACME-EMPLOYEE-RECORD TO MERGED-EMPLOYEE-RECORD
                   WRITE MERGED-EMPLOYEE-RECORD
                   ADD 1 TO WS-EMPLOYEE-COUNT
             END-READ
             READ FUSESINCFILE
                AT END MOVE "10" TO FUSESINC-FILE-STATUS
                NOT AT END
                   MOVE FUSESINC-EMPLOYEE-RECORD TO 
                   MERGED-EMPLOYEE-RECORD
                   WRITE MERGED-EMPLOYEE-RECORD
                   ADD 1 TO WS-EMPLOYEE-COUNT
             END-READ
          END-PERFORM
          CLOSE ACMEFILE, FUSESINCFILE, MERGEDFILE.

       0200-SORT-MERGED-EMPLOYEES.
          SORT MERGEDFILE
             ON ASCENDING KEY MERGED-SSN
             USING MERGEDFILE
             GIVING SORTEDFILE.

       0300-GENERATE-REPORT.
          OPEN OUTPUT REPORTFILE
          MOVE "EMPLOYEES REPORT" TO REPORT-LINE
          WRITE REPORT-LINE
          MOVE "================" TO REPORT-LINE
          WRITE REPORT-LINE
          PERFORM UNTIL MERGED-FILE-STATUS = "10"
             READ SORTEDFILE
                AT END MOVE "10" TO MERGED-FILE-STATUS
                NOT AT END
                   MOVE SORTED-EMPLOYEE-RECORD TO REPORT-LINE
                   WRITE REPORT-LINE
             END-READ
          END-PERFORM
          CLOSE REPORTFILE.

       9000-END-PROGRAM.
          STOP RUN.
