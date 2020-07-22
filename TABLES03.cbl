       IDENTIFICATION DIVISION.
      ****************************************************************
      **   THIS IS AN EXAMPLE PROGRAM FROM THE COURSE OF COMPLETED
      **     WORKING SEARCH AND SEARCH ALL CODE
      ****************************************************************
       PROGRAM-ID.   TABLES03.
       INSTALLATION.  IBM.
       DATE-WRITTEN.  01-01-2009.
       DATE-COMPILED. 01-01-2009.
       SECURITY.   NONE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   IBM.
       OBJECT-COMPUTER.   IBM.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STATE-NAME-FILE   ASSIGN TO STATES
                  ORGANIZATION IS SEQUENTIAL.
           SELECT STATE-ABBREVIATIONS   ASSIGN TO STABBREV
                  ORGANIZATION IS SEQUENTIAL.
           SELECT REPORT-OUT ASSIGN TO RPTOUT
                  ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  STATE-ABBREVIATIONS
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD.
       01 STATE-ABBREV-REC.
           05 STATE-ABBREV-DATA  PIC X(02).
           05 FILLER             PIC X(78).

       FD  STATE-NAME-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD.
       01 STATE-NAME-REC.
           05 STATE-NAME         PIC X(20).
           05 FILLER             PIC X(60).

       FD  REPORT-OUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD.
       01 REPORT-LINE-OUT        PIC X(80).

       WORKING-STORAGE SECTION.
       01 SWITCHES-IN-PROGRAM.
           05 SW-END-OF-STATES    PIC X       VALUE 'N'.
               88 END-OF-STATES               VALUE 'Y'.
           05 SW-END-OF-ABBREV    PIC X       VALUE 'N'.
               88 END-OF-ABBREV               VALUE 'Y'.

       77  CTR-STATES             PIC S9(02)  VALUE +0.
       77  STATE-ABBREV-SEARCH    PIC X(02).
       77  STATE-NAME-SEARCH      PIC X(20).

       01  STATES-TABLE.
           05 STATE-DATA OCCURS 50 TIMES
                   INDEXED BY ST-NAME-IDX.
               10 ST-NAME-DAT    PIC X(20).
               10 FILLER         PIC X(60).

       01  STATES-TABLE-B.
           05 STATE-DATA-B OCCURS 50 TIMES
                   ASCENDING KEY IS ST-NAME-DAT-B
                   INDEXED BY ST-NAME-IDX-B.
               10 ST-NAME-DAT-B    PIC X(20).
               10 FILLER           PIC X(60).

       01 STATES-ABBREV-TABLE.
           05 STATE-ABBREV-TBL OCCURS 50 TIMES
                   ASCENDING KEY IS ST-ABBREV-DAT
                   INDEXED BY ST-ABBREV-IDX.
               10 ST-ABBREV-DAT  PIC X(02).
               10 FILLER         PIC X(78).

       PROCEDURE DIVISION.
       000-TOP-LEVEL.
           PERFORM 100-INITIALIZATION.
           PERFORM 200-LOAD-TABLES UNTIL END-OF-STATES.
           PERFORM 300-STATE-SEARCH.
           PERFORM 350-STATE-SEARCH-REV.
           PERFORM 400-STATE-SEARCH-ALL.
           PERFORM 450-STATE-SEARCH-ALL-REV.
           PERFORM 900-WRAP-UP.
           GOBACK.

       100-INITIALIZATION.
           OPEN INPUT STATE-NAME-FILE, STATE-ABBREVIATIONS.
           OPEN OUTPUT REPORT-OUT.
           SET ST-ABBREV-IDX TO 1.
           SET ST-NAME-IDX TO 1.
           SET ST-NAME-IDX-B TO 1.
           PERFORM 230-READ-RECORDS.

       200-LOAD-TABLES.
           MOVE STATE-ABBREV-REC TO STATE-ABBREV-TBL(ST-ABBREV-IDX).
           MOVE STATE-NAME-REC TO STATE-DATA(ST-NAME-IDX).
           MOVE STATE-NAME-REC TO STATE-DATA-B(ST-NAME-IDX-B).
           SET ST-ABBREV-IDX UP BY 1.
           SET ST-NAME-IDX UP BY 1.
           SET ST-NAME-IDX-B UP BY 1.
           PERFORM 230-READ-RECORDS.

       230-READ-RECORDS.
           READ STATE-NAME-FILE
           AT END
              MOVE 'Y' TO SW-END-OF-STATES.
           READ STATE-ABBREVIATIONS
           AT END
              MOVE 'Y' TO SW-END-OF-ABBREV.

       300-STATE-SEARCH.
           MOVE 'CT' TO STATE-ABBREV-SEARCH.
           SET ST-ABBREV-IDX TO 1.
           SEARCH STATE-ABBREV-TBL
           AT END
              DISPLAY 'NOT A STATE'
           WHEN ST-ABBREV-DAT(ST-ABBREV-IDX) = STATE-ABBREV-SEARCH
                SET ST-NAME-IDX TO ST-ABBREV-IDX.
           DISPLAY 'FOUND ' ST-NAME-DAT(ST-NAME-IDX).

       350-STATE-SEARCH-REV.
           MOVE 'California'  TO STATE-NAME-SEARCH.
           SET ST-NAME-IDX TO 1.
           SEARCH STATE-DATA
           AT END
              DISPLAY 'NOT A STATE'
           WHEN ST-NAME-DAT(ST-NAME-IDX) = STATE-NAME-SEARCH
                SET ST-ABBREV-IDX TO ST-NAME-IDX
           DISPLAY 'FOUND ' ST-ABBREV-DAT(ST-ABBREV-IDX).

       400-STATE-SEARCH-ALL.
           MOVE 'KS' TO STATE-ABBREV-SEARCH.
           SEARCH ALL STATE-ABBREV-TBL
           AT END
              DISPLAY 'NOT A STATE'
           WHEN ST-ABBREV-DAT(ST-ABBREV-IDX) = STATE-ABBREV-SEARCH
                SET ST-NAME-IDX TO ST-ABBREV-IDX
                DISPLAY 'FOUND ' ST-NAME-DAT(ST-NAME-IDX).

       450-STATE-SEARCH-ALL-REV.
           MOVE 'Utah' TO STATE-NAME-SEARCH
           SEARCH ALL STATE-DATA-B
           AT END
              DISPLAY 'NOT A STATE'
           WHEN ST-NAME-DAT-B(ST-NAME-IDX-B) = STATE-NAME-SEARCH
                SET ST-ABBREV-IDX TO ST-NAME-IDX-B
                DISPLAY 'FOUND ' ST-ABBREV-DAT(ST-ABBREV-IDX).

       900-WRAP-UP.
           CLOSE STATE-NAME-FILE, STATE-ABBREVIATIONS, REPORT-OUT.