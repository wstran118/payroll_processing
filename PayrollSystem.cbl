IDENTIFICATION DIVISION.
       PROGRAM-ID. PayrollSystem.
       AUTHOR. Grok-Generated.
       DATE-WRITTEN. 21-JUL-2025.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO 'EMPLOYEES.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT REPORT-FILE ASSIGN TO 'PAYROLL.RPT'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYEE-FILE.
       01  EMPLOYEE-RECORD.
           05 EMP-ID           PIC X(5).
           05 EMP-NAME         PIC X(30).
           05 EMP-HOURS        PIC 9(3)V99.
           05 EMP-RATE         PIC 9(3)V99.

       FD  REPORT-FILE.
       01  REPORT-LINE         PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-EMPLOYEE.
           05 WS-EMP-ID        PIC X(5).
           05 WS-EMP-NAME      PIC X(30).
           05 WS-EMP-HOURS     PIC 9(3)V99.
           05 WS-EMP-RATE      PIC 9(3)V99.
           05 WS-GROSS-PAY     PIC 9(5)V99.
           05 WS-TAX           PIC 9(5)V99.
           05 WS-NET-PAY       PIC 9(5)V99.

       01  WS-REPORT-HEADER.
           05 FILLER           PIC X(30) VALUE 'PAYROLL REPORT'.
           05 FILLER           PIC X(50) VALUE SPACES.

       01  WS-REPORT-DETAIL.
           05 FILLER           PIC X(5) VALUE 'ID: '.
           05 DET-EMP-ID       PIC X(5).
           05 FILLER           PIC X(5) VALUE SPACES.
           05 FILLER           PIC X(6) VALUE 'Name: '.
           05 DET-EMP-NAME     PIC X(30).
           05 FILLER           PIC X(5) VALUE SPACES.
           05 FILLER           PIC X(7) VALUE 'Gross: '.
           05 DET-GROSS-PAY    PIC $ZZ,ZZ9.99.
           05 FILLER           PIC X(5) VALUE SPACES.
           05 FILLER           PIC X(5) VALUE 'Net: '.
           05 DET-NET-PAY      PIC $ZZ,ZZ9.99.

       01  WS-EOF              PIC X VALUE 'N'.
       01  WS-TAX-RATE         PIC V99 VALUE 0.20.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT EMPLOYEE-FILE
           OPEN OUTPUT REPORT-FILE

           WRITE REPORT-LINE FROM WS-REPORT-HEADER
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE

           PERFORM PROCESS-EMPLOYEES UNTIL WS-EOF = 'Y'

           CLOSE EMPLOYEE-FILE
           CLOSE REPORT-FILE
           STOP RUN.

       PROCESS-EMPLOYEES.
           READ EMPLOYEE-FILE INTO WS-EMPLOYEE
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   PERFORM CALCULATE-PAY
                   PERFORM WRITE-REPORT
           END-READ.

       CALCULATE-PAY.
           COMPUTE WS-GROSS-PAY = WS-EMP-HOURS * WS-EMP-RATE
           COMPUTE WS-TAX = WS-GROSS-PAY * WS-TAX-RATE
           COMPUTE WS-NET-PAY = WS-GROSS-PAY - WS-TAX.

       WRITE-REPORT.
           MOVE WS-EMP-ID TO DET-EMP-ID
           MOVE WS-EMP-NAME TO DET-EMP-NAME
           MOVE WS-GROSS-PAY TO DET-GROSS-PAY
           MOVE WS-NET-PAY TO DET-NET-PAY
           WRITE REPORT-LINE FROM WS-REPORT-DETAIL.