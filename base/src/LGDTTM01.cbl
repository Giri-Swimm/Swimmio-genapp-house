      ******************************************************************
      *                                                                *
      * Date and Time Display Program                                  *
      *                                                                *
      * This program demonstrates how to get and display the current   *
      * date and time in COBOL using intrinsic functions and           *
      * ACCEPT statements.                                             *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGDTTM01.
       AUTHOR. COBOL PROGRAMMER.
       DATE-WRITTEN. 2026-01-30.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *----------------------------------------------------------------*
      * Date and Time Variables                                        *
      *----------------------------------------------------------------*
       01  WS-CURRENT-DATE.
           05  WS-CURRENT-YEAR        PIC 9(4).
           05  WS-CURRENT-MONTH       PIC 9(2).
           05  WS-CURRENT-DAY         PIC 9(2).

       01  WS-CURRENT-TIME.
           05  WS-CURRENT-HOUR        PIC 9(2).
           05  WS-CURRENT-MINUTE      PIC 9(2).
           05  WS-CURRENT-SECOND      PIC 9(2).
           05  WS-CURRENT-HUNDREDTH   PIC 9(2).

       01  WS-FULL-DATE-TIME.
           05  WS-FDT-YEAR            PIC 9(4).
           05  WS-FDT-MONTH           PIC 9(2).
           05  WS-FDT-DAY             PIC 9(2).
           05  WS-FDT-HOUR            PIC 9(2).
           05  WS-FDT-MINUTE          PIC 9(2).
           05  WS-FDT-SECOND          PIC 9(2).
           05  WS-FDT-MILLISEC        PIC 9(2).
           05  WS-FDT-UTC-SIGN        PIC X.
           05  WS-FDT-UTC-HOUR        PIC 9(2).
           05  WS-FDT-UTC-MINUTE      PIC 9(2).

      *----------------------------------------------------------------*
      * Formatted Display Variables                                    *
      *----------------------------------------------------------------*
       01  WS-FORMATTED-DATE.
           05  WS-FMT-MONTH           PIC 9(2).
           05  FILLER                 PIC X     VALUE '/'.
           05  WS-FMT-DAY             PIC 9(2).
           05  FILLER                 PIC X     VALUE '/'.
           05  WS-FMT-YEAR            PIC 9(4).

       01  WS-FORMATTED-TIME.
           05  WS-FMT-HOUR            PIC 9(2).
           05  FILLER                 PIC X     VALUE ':'.
           05  WS-FMT-MINUTE          PIC 9(2).
           05  FILLER                 PIC X     VALUE ':'.
           05  WS-FMT-SECOND          PIC 9(2).

       01  WS-DAY-OF-WEEK             PIC 9.
       01  WS-DAY-NAME                PIC X(9).

      *----------------------------------------------------------------*
      * Julian Date Variables                                          *
      *----------------------------------------------------------------*
       01  WS-JULIAN-DATE.
           05  WS-JUL-YEAR            PIC 9(4).
           05  WS-JUL-DAY             PIC 9(3).

      *----------------------------------------------------------------*
      * Display Messages                                               *
      *----------------------------------------------------------------*
       01  WS-HEADER                  PIC X(60) VALUE
           '========== DATE AND TIME DISPLAY =========='.
       01  WS-SEPARATOR               PIC X(60) VALUE ALL '-'.

      *----------------------------------------------------------------*
      * PROCEDURE DIVISION                                             *
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

      *----------------------------------------------------------------*
       MAIN-LOGIC SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-DISPLAY-HEADER.
           PERFORM 2000-GET-DATE-TIME.
           PERFORM 3000-DISPLAY-DATE-TIME.
           PERFORM 4000-DISPLAY-JULIAN-DATE.
           PERFORM 5000-DISPLAY-DAY-OF-WEEK.
           PERFORM 9000-END-PROGRAM.

       MAIN-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       1000-DISPLAY-HEADER SECTION.
      *----------------------------------------------------------------*

           DISPLAY WS-HEADER.
           DISPLAY WS-SEPARATOR.
           DISPLAY ' '.

       1000-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2000-GET-DATE-TIME SECTION.
      *----------------------------------------------------------------*
      * Get current date and time using ACCEPT statements              *
      *----------------------------------------------------------------*

      *    Get current date (YYYYMMDD format)
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD.

      *    Get current time (HHMMSSss format)
           ACCEPT WS-CURRENT-TIME FROM TIME.

      *    Get complete date-time with timezone
           ACCEPT WS-FULL-DATE-TIME FROM DATE-TIME.

      *    Get day of week (1=Monday, 7=Sunday)
           ACCEPT WS-DAY-OF-WEEK FROM DAY-OF-WEEK.

      *    Get Julian date (YYYYDDD format)
           ACCEPT WS-JULIAN-DATE FROM DAY YYYYDDD.

       2000-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       3000-DISPLAY-DATE-TIME SECTION.
      *----------------------------------------------------------------*
      * Display formatted date and time                                *
      *----------------------------------------------------------------*

      *    Prepare formatted date (MM/DD/YYYY)
           MOVE WS-CURRENT-MONTH TO WS-FMT-MONTH.
           MOVE WS-CURRENT-DAY   TO WS-FMT-DAY.
           MOVE WS-CURRENT-YEAR  TO WS-FMT-YEAR.

      *    Prepare formatted time (HH:MM:SS)
           MOVE WS-CURRENT-HOUR   TO WS-FMT-HOUR.
           MOVE WS-CURRENT-MINUTE TO WS-FMT-MINUTE.
           MOVE WS-CURRENT-SECOND TO WS-FMT-SECOND.

      *    Display results
           DISPLAY 'CURRENT DATE (YYYYMMDD): ' WS-CURRENT-DATE.
           DISPLAY 'FORMATTED DATE (MM/DD/YYYY): ' WS-FORMATTED-DATE.
           DISPLAY ' '.
           DISPLAY 'CURRENT TIME (HH:MM:SS): ' WS-FORMATTED-TIME.
           DISPLAY 'TIME WITH HUNDREDTHS: '
                   WS-CURRENT-HOUR ':'
                   WS-CURRENT-MINUTE ':'
                   WS-CURRENT-SECOND '.'
                   WS-CURRENT-HUNDREDTH.
           DISPLAY ' '.

       3000-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       4000-DISPLAY-JULIAN-DATE SECTION.
      *----------------------------------------------------------------*
      * Display Julian date information                                *
      *----------------------------------------------------------------*

           DISPLAY 'JULIAN DATE: ' WS-JULIAN-DATE.
           DISPLAY 'YEAR: ' WS-JUL-YEAR
                   ' DAY OF YEAR: ' WS-JUL-DAY.
           DISPLAY ' '.

       4000-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       5000-DISPLAY-DAY-OF-WEEK SECTION.
      *----------------------------------------------------------------*
      * Display day of week                                            *
      *----------------------------------------------------------------*

           EVALUATE WS-DAY-OF-WEEK
               WHEN 1
                   MOVE 'MONDAY'    TO WS-DAY-NAME
               WHEN 2
                   MOVE 'TUESDAY'   TO WS-DAY-NAME
               WHEN 3
                   MOVE 'WEDNESDAY' TO WS-DAY-NAME
               WHEN 4
                   MOVE 'THURSDAY'  TO WS-DAY-NAME
               WHEN 5
                   MOVE 'FRIDAY'    TO WS-DAY-NAME
               WHEN 6
                   MOVE 'SATURDAY'  TO WS-DAY-NAME
               WHEN 7
                   MOVE 'SUNDAY'    TO WS-DAY-NAME
               WHEN OTHER
                   MOVE 'UNKNOWN'   TO WS-DAY-NAME
           END-EVALUATE.

           DISPLAY 'DAY OF WEEK: ' WS-DAY-NAME.
           DISPLAY ' '.
           DISPLAY 'COMPLETE DATE-TIME FROM SYSTEM:'.
           DISPLAY '  DATE: ' WS-FDT-YEAR '-'
                              WS-FDT-MONTH '-'
                              WS-FDT-DAY.
           DISPLAY '  TIME: ' WS-FDT-HOUR ':'
                              WS-FDT-MINUTE ':'
                              WS-FDT-SECOND '.'
                              WS-FDT-MILLISEC.
           DISPLAY '  UTC OFFSET: ' WS-FDT-UTC-SIGN
                                     WS-FDT-UTC-HOUR ':'
                                     WS-FDT-UTC-MINUTE.
           DISPLAY ' '.

       5000-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       9000-END-PROGRAM SECTION.
      *----------------------------------------------------------------*

           DISPLAY WS-SEPARATOR.
           DISPLAY 'PROGRAM COMPLETED SUCCESSFULLY'.
           STOP RUN.

       9000-EXIT.
           EXIT.
