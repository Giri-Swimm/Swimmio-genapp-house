      **************************************************************
      **  POLICY INQUIRY SYSTEM                                    **
      **  DATE CREATED        : 02/05/2026                        **  
      **  PROGRAMMER          : GitHub Copilot                    **
      **************************************************************
      */********************************************************
      */                                                       *
      */    POLICY INQUIRY SYSTEM                              *
      */    THIS PROGRAM PROVIDES COMPREHENSIVE POLICY         *
      */    INQUIRY CAPABILITIES INCLUDING:                    *
      */                                                       *
      */    - SEARCH BY AGENT LAST NAME                        *
      */    - SEARCH BY POLICY INSURED NAME                    *
      */    - ADD BENEFIT RIDER                                *
      */    - ADD CHILD RIDER                                  *
      */    - ADDRESS CHANGES                                  *
      */    - ADD PRE-AUTHORIZED CHECKING INFORMATION          *
      */    - PREMIUM INCREASE                                 *
      */    - PREMIUM DECREASE                                 *
      */    - RIDERS DELETING                                  *
      */    - RIDERS UPDATING                                  *
      */    - PREMIUM CHANGES                                  *
      */                                                       *
      */    INPUT FILES ARE: POL-MASTER, POL-INSURED,          *
      */                     POL-BENEFIT, AGENT-MASTER         *
      */                                                       *
      */    INPUT/OUTPUT FILES ARE: POL-MASTER, POL-BENEFIT,   *
      */                            POL-INSURED, POL-TRAN1     *
      */                                                       *
      */    OUTPUT FILES ARE: POL-TRAN2, POL-NOTIFY            *
      */                                                       *
      */********************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAMPLES.
      *      POLINQ01 IS INITIAL.
      *DATE-MODIFIED.  FEB, 2026                 *** POLICY INQUIRY ***

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * SYSTEM PARAMETERS AND CONTROL FIELDS
       01  MPE-PARM-MASK                 PIC S9(9) COMP-5.
       01  UNIX-COMMAND-LINE             PIC X(80).
       01  CURRENT-DATE                  PIC X(8).
       01  TIME-OF-DAY                   PIC X(8).
       01  TALLY                         PIC S9(9) COMP.

      * WORK VARIABLES FOR POLICY INQUIRY
       01  WK-POLICY-NUMBER              PIC X(10) VALUE SPACES.
       01  WK-SEARCH-NAME                PIC X(16) VALUE SPACES.
       01  WK-AGENT-ID                   PIC X(12) VALUE SPACES.
       01  WK-FOUND-SWITCH               PIC X VALUE "N".
           88  RECORD-FOUND                    VALUE "Y".
           88  NO-RECORD-FOUND                 VALUE "N".

       01  WK-FUNCTION-CODE              PIC XX VALUE SPACES.
           88  SEARCH-BY-AGENT                 VALUE "01".
           88  SEARCH-BY-INSURED               VALUE "02".
           88  ADD-BENEFIT-RIDER               VALUE "03".
           88  ADD-CHILD-RIDER                 VALUE "04".
           88  ADDRESS-CHANGE                  VALUE "05".
           88  ADD-PREAUTH-CHECK               VALUE "06".
           88  PREMIUM-INCREASE                VALUE "07".
           88  PREMIUM-DECREASE                VALUE "08".
           88  DELETE-RIDER                    VALUE "09".
           88  UPDATE-RIDER                    VALUE "10".
           88  PREMIUM-CHANGE                  VALUE "11".

      * PREMIUM CALCULATION FIELDS
       01  WK-CURRENT-PREMIUM            PIC S9(9)V99 COMP-3 VALUE +0.
       01  WK-NEW-PREMIUM                PIC S9(9)V99 COMP-3 VALUE +0.
       01  WK-PREMIUM-DIFFERENCE         PIC S9(9)V99 COMP-3 VALUE +0.
       01  WK-PERCENTAGE-CHANGE          PIC S9(3)V99 COMP-3 VALUE +0.

      * RIDER INFORMATION
       01  WK-RIDER-CODE                 PIC X(4) VALUE SPACES.
       01  WK-RIDER-AMOUNT               PIC S9(9)V99 COMP-3 VALUE +0.
       01  WK-RIDER-EFFECTIVE-DATE       PIC 9(8) VALUE 0.

      * ADDRESS CHANGE FIELDS
       01  WK-NEW-ADDRESS.
           05  WK-NEW-ADDR1              PIC X(30) VALUE SPACES.
           05  WK-NEW-ADDR2              PIC X(30) VALUE SPACES.
           05  WK-NEW-CITY               PIC X(20) VALUE SPACES.
           05  WK-NEW-STATE              PIC XX VALUE SPACES.
           05  WK-NEW-ZIP                PIC X(10) VALUE SPACES.

      * PRE-AUTHORIZED CHECKING FIELDS
       01  WK-BANK-INFO.
           05  WK-BANK-NAME              PIC X(30) VALUE SPACES.
           05  WK-BANK-ROUTING           PIC X(9) VALUE SPACES.
           05  WK-ACCOUNT-NUMBER         PIC X(20) VALUE SPACES.
           05  WK-ACCOUNT-TYPE           PIC X VALUE SPACES.
               88  CHECKING-ACCOUNT            VALUE "C".
               88  SAVINGS-ACCOUNT             VALUE "S".

      * ERROR HANDLING
       01  ERROR-SWITCHES.
           05  MAIN-ERROR-SW             PIC X VALUE "N".
               88  NO-ERRORS                   VALUE "N".
               88  THERE-ARE-ERRORS            VALUE "Y".
           05  VALIDATION-ERROR-SW       PIC X VALUE "N".
               88  NO-VALIDATION-ERRORS        VALUE "N".
               88  VALIDATION-ERRORS-FOUND     VALUE "Y".

      * SCREEN BUFFER DEFINITION
       01  INQUIRY-SCREEN-BUF.
           05  IS-POLICY-NUMBER          PIC X(10) VALUE SPACES.
           05  IS-FUNCTION-CODE          PIC XX VALUE SPACES.
           05  IS-SEARCH-CRITERIA        PIC X(30) VALUE SPACES.
           05  IS-INSURED-NAME           PIC X(30) VALUE SPACES.
           05  IS-AGENT-NAME             PIC X(30) VALUE SPACES.
           05  IS-CURRENT-PREMIUM        PIC ZZZ,ZZ9.99.
           05  IS-NEW-PREMIUM            PIC ZZZ,ZZ9.99.
           05  IS-EFFECTIVE-DATE         PIC 99/99/9999.
           05  IS-ADDRESS-LINE1          PIC X(30) VALUE SPACES.
           05  IS-ADDRESS-LINE2          PIC X(30) VALUE SPACES.
           05  IS-CITY-STATE-ZIP         PIC X(40) VALUE SPACES.
           05  IS-BANK-INFO              PIC X(60) VALUE SPACES.
           05  IS-MESSAGE-LINE           PIC X(80) VALUE SPACES.

      * DATES AND CALCULATIONS
       01  WK-EFFECTIVE-DATE.
           05  WK-EFF-YEAR               PIC 9999 VALUE 0.
           05  WK-EFF-MONTH              PIC 99 VALUE 0.
           05  WK-EFF-DAY                PIC 99 VALUE 0.

       01  WK-CURRENT-DATE.
           05  WK-CURR-YEAR              PIC 9999 VALUE 0.
           05  WK-CURR-MONTH             PIC 99 VALUE 0.
           05  WK-CURR-DAY               PIC 99 VALUE 0.

      * TRANSACTION RECORD FOR LOGGING CHANGES
       01  TRANSACTION-RECORD.
           05  TRAN-POLICY-NUM           PIC X(10) VALUE SPACES.
           05  TRAN-DATE                 PIC 9(8) VALUE 0.
           05  TRAN-TIME                 PIC 9(6) VALUE 0.
           05  TRAN-USER-ID              PIC X(8) VALUE SPACES.
           05  TRAN-FUNCTION             PIC XX VALUE SPACES.
           05  TRAN-OLD-VALUE            PIC X(50) VALUE SPACES.
           05  TRAN-NEW-VALUE            PIC X(50) VALUE SPACES.
           05  TRAN-DESCRIPTION          PIC X(100) VALUE SPACES.

      * MESSAGE TEMPLATES
       01  MSG-POLICY-NOT-FOUND          PIC X(60) VALUE 
           "POLICY NOT FOUND - PLEASE VERIFY POLICY NUMBER".

       01  MSG-AGENT-NOT-FOUND           PIC X(60) VALUE
           "NO POLICIES FOUND FOR SPECIFIED AGENT".

       01  MSG-INSURED-NOT-FOUND         PIC X(60) VALUE
           "NO POLICIES FOUND FOR SPECIFIED INSURED NAME".

       01  MSG-INVALID-PREMIUM           PIC X(60) VALUE
           "INVALID PREMIUM AMOUNT - PLEASE ENTER VALID AMOUNT".

       01  MSG-INVALID-DATE              PIC X(60) VALUE
           "INVALID DATE - PLEASE ENTER DATE AS MM/DD/YYYY".

       01  MSG-SUCCESSFUL-UPDATE         PIC X(60) VALUE
           "POLICY SUCCESSFULLY UPDATED".

       01  MSG-RIDER-ADDED               PIC X(60) VALUE
           "RIDER SUCCESSFULLY ADDED TO POLICY".

       01  MSG-RIDER-DELETED             PIC X(60) VALUE
           "RIDER SUCCESSFULLY DELETED FROM POLICY".

      * COPY LIBRARY RECORDS
       01  POL-MASTER-REC         COPY PMASTER IN "COPYLIB".
       01  POL-INSURED-REC        COPY PINSURED IN "COPYLIB".
       01  POL-BENEFIT-REC        COPY PBENEFIT IN "COPYLIB".
       01  AGENT-MASTER-REC       COPY AGTMSTR IN "COPYLIB".
       01  POL-TRAN1-REC         COPY POLTRAN1 IN "COPYLIB".
       01  POL-TRAN2-REC         COPY POLTRAN2 IN "COPYLIB".
       01  POL-NOTIFY-REC        COPY POLNTFY IN "COPYLIB".

      * FILE STATUS CODES
       01  FILE-STATUS-CODES.
           05  MASTER-FILE-STATUS        PIC XX VALUE "00".
           05  INSURED-FILE-STATUS       PIC XX VALUE "00".
           05  BENEFIT-FILE-STATUS       PIC XX VALUE "00".
           05  AGENT-FILE-STATUS         PIC XX VALUE "00".
           05  TRAN-FILE-STATUS          PIC XX VALUE "00".

       PROCEDURE DIVISION.

       000-MAIN-CONTROL.
           PERFORM 100-INITIALIZE
           PERFORM 200-PROCESS-INQUIRY UNTIL NO-MORE-REQUESTS
           PERFORM 900-CLEANUP
           STOP RUN.

       100-INITIALIZE.
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE
           MOVE CURRENT-DATE(1:8) TO WK-CURRENT-DATE
           INITIALIZE INQUIRY-SCREEN-BUF
                      ERROR-SWITCHES
                      WK-FOUND-SWITCH
           MOVE "POLICY INQUIRY SYSTEM - READY" TO IS-MESSAGE-LINE.

       200-PROCESS-INQUIRY.
      * Main processing routine - display menu and process selection
           PERFORM 210-DISPLAY-MAIN-MENU
           PERFORM 220-GET-USER-SELECTION
           EVALUATE WK-FUNCTION-CODE
               WHEN "01"
                   PERFORM 300-SEARCH-BY-AGENT
               WHEN "02"
                   PERFORM 400-SEARCH-BY-INSURED
               WHEN "03"
                   PERFORM 500-ADD-BENEFIT-RIDER
               WHEN "04"
                   PERFORM 600-ADD-CHILD-RIDER
               WHEN "05"
                   PERFORM 700-ADDRESS-CHANGE
               WHEN "06"
                   PERFORM 800-ADD-PREAUTH-CHECK
               WHEN "07"
                   PERFORM 850-PREMIUM-INCREASE
               WHEN "08"
                   PERFORM 860-PREMIUM-DECREASE
               WHEN "09"
                   PERFORM 870-DELETE-RIDER
               WHEN "10"
                   PERFORM 880-UPDATE-RIDER
               WHEN "11"
                   PERFORM 890-PREMIUM-CHANGE
               WHEN OTHER
                   MOVE "INVALID FUNCTION SELECTED" TO IS-MESSAGE-LINE
           END-EVALUATE.

       210-DISPLAY-MAIN-MENU.
      * Display the main menu options
           DISPLAY " "
           DISPLAY "=========================================="
           DISPLAY "        POLICY INQUIRY SYSTEM           "
           DISPLAY "=========================================="
           DISPLAY " "
           DISPLAY "01 - SEARCH BY AGENT LAST NAME"
           DISPLAY "02 - SEARCH BY POLICY INSURED NAME"
           DISPLAY "03 - ADD BENEFIT RIDER"
           DISPLAY "04 - ADD CHILD RIDER"
           DISPLAY "05 - ADDRESS CHANGES"
           DISPLAY "06 - ADD PRE-AUTHORIZED CHECKING INFO"
           DISPLAY "07 - PREMIUM INCREASE"
           DISPLAY "08 - PREMIUM DECREASE"
           DISPLAY "09 - DELETE RIDER"
           DISPLAY "10 - UPDATE RIDER"
           DISPLAY "11 - PREMIUM CHANGES"
           DISPLAY " "
           DISPLAY "ENTER FUNCTION CODE (01-11) OR 'XX' TO EXIT: "
           .

       220-GET-USER-SELECTION.
      * Get user's function selection
           ACCEPT WK-FUNCTION-CODE
           IF WK-FUNCTION-CODE = "XX"
               SET NO-MORE-REQUESTS TO TRUE
           END-IF.

       300-SEARCH-BY-AGENT.
      * Search for policies by agent last name
           DISPLAY "ENTER AGENT LAST NAME: "
           ACCEPT WK-SEARCH-NAME
           
           PERFORM 310-FIND-AGENT-POLICIES
           
           IF RECORD-FOUND
               PERFORM 320-DISPLAY-AGENT-POLICIES
           ELSE
               MOVE MSG-AGENT-NOT-FOUND TO IS-MESSAGE-LINE
               DISPLAY IS-MESSAGE-LINE
           END-IF.

       310-FIND-AGENT-POLICIES.
      * Find all policies for specified agent
           SET NO-RECORD-FOUND TO TRUE
           
      * This would normally read through AGENT-MASTER file
      * and cross-reference with POL-MASTER file
      * Simplified logic for demonstration:
           
           MOVE WK-SEARCH-NAME TO A-LAST-NAME
      * READ AGENT-MASTER FILE WITH KEY = A-LAST-NAME
      * IF FOUND, GET A-AGENT-ID AND SEARCH POL-MASTER
           
           IF MASTER-FILE-STATUS = "00"
               SET RECORD-FOUND TO TRUE
               MOVE "AGENT POLICIES FOUND" TO IS-MESSAGE-LINE
           END-IF.

       320-DISPLAY-AGENT-POLICIES.
      * Display policies found for the agent
           DISPLAY " "
           DISPLAY "POLICIES FOR AGENT: " WK-SEARCH-NAME
           DISPLAY "======================================"
           DISPLAY "POLICY NUMBER  INSURED NAME        PREMIUM"
           DISPLAY "============  =================  ========="
      * Loop through found policies and display
           DISPLAY PM-POLNUM " " PI-LAST-NAME " " PM-COMM-PREM
           DISPLAY " ".

       400-SEARCH-BY-INSURED.
      * Search for policies by insured name
           DISPLAY "ENTER INSURED LAST NAME: "
           ACCEPT WK-SEARCH-NAME
           
           PERFORM 410-FIND-INSURED-POLICIES
           
           IF RECORD-FOUND
               PERFORM 420-DISPLAY-INSURED-POLICIES
           ELSE
               MOVE MSG-INSURED-NOT-FOUND TO IS-MESSAGE-LINE
               DISPLAY IS-MESSAGE-LINE
           END-IF.

       410-FIND-INSURED-POLICIES.
      * Find policies for specified insured
           SET NO-RECORD-FOUND TO TRUE
           
           MOVE WK-SEARCH-NAME TO PI-LAST-NAME
      * READ POL-INSURED FILE WITH KEY = PI-LAST-NAME
           
           IF INSURED-FILE-STATUS = "00"
               SET RECORD-FOUND TO TRUE
               MOVE "INSURED POLICIES FOUND" TO IS-MESSAGE-LINE
           END-IF.

       420-DISPLAY-INSURED-POLICIES.
      * Display policies found for the insured
           DISPLAY " "
           DISPLAY "POLICIES FOR INSURED: " WK-SEARCH-NAME
           DISPLAY "======================================"
           DISPLAY "POLICY NUMBER  AGENT NAME          PREMIUM"
           DISPLAY "============  =================  ========="
      * Display found policy information
           DISPLAY PM-POLNUM " " A-LAST-NAME " " PM-COMM-PREM
           DISPLAY " ".

       500-ADD-BENEFIT-RIDER.
      * Add a benefit rider to a policy
           PERFORM 510-GET-POLICY-FOR-RIDER
           
           IF RECORD-FOUND
               PERFORM 520-GET-RIDER-DETAILS
               PERFORM 530-ADD-RIDER-TO-POLICY
               PERFORM 590-LOG-TRANSACTION
           END-IF.

       510-GET-POLICY-FOR-RIDER.
      * Get policy number for rider addition
           DISPLAY "ENTER POLICY NUMBER: "
           ACCEPT WK-POLICY-NUMBER
           
           PERFORM 950-VALIDATE-POLICY-NUMBER
           
           IF RECORD-FOUND
               PERFORM 960-READ-POLICY-MASTER
           END-IF.

       520-GET-RIDER-DETAILS.
      * Get details for the new rider
           DISPLAY "ENTER RIDER CODE (4 CHARS): "
           ACCEPT WK-RIDER-CODE
           
           DISPLAY "ENTER RIDER AMOUNT: "
           ACCEPT WK-RIDER-AMOUNT
           
           DISPLAY "ENTER EFFECTIVE DATE (YYYYMMDD): "
           ACCEPT WK-RIDER-EFFECTIVE-DATE.

       530-ADD-RIDER-TO-POLICY.
      * Add the rider to the benefit file
           MOVE WK-POLICY-NUMBER TO PB-POLNUM
           MOVE WK-RIDER-CODE TO PB-RECORD-TYPE
           MOVE WK-RIDER-AMOUNT TO PB-DAILY-BENEFIT
           MOVE WK-RIDER-EFFECTIVE-DATE TO PB-ISSUE-DATE
           
      * WRITE POL-BENEFIT-REC
           
           MOVE MSG-RIDER-ADDED TO IS-MESSAGE-LINE
           DISPLAY IS-MESSAGE-LINE.

       590-LOG-TRANSACTION.
      * Log the transaction
           MOVE WK-POLICY-NUMBER TO TRAN-POLICY-NUM
           MOVE WK-CURRENT-DATE TO TRAN-DATE
           MOVE "03" TO TRAN-FUNCTION
           MOVE "BENEFIT RIDER ADDED" TO TRAN-DESCRIPTION
           MOVE WK-RIDER-CODE TO TRAN-NEW-VALUE
           
      * WRITE TRAN-RECORD.

       600-ADD-CHILD-RIDER.
      * Add a child rider to a policy
           PERFORM 510-GET-POLICY-FOR-RIDER
           
           IF RECORD-FOUND
               PERFORM 610-GET-CHILD-RIDER-DETAILS
               PERFORM 620-ADD-CHILD-RIDER-TO-POLICY
               PERFORM 590-LOG-TRANSACTION
           END-IF.

       610-GET-CHILD-RIDER-DETAILS.
      * Get child rider specific details
           DISPLAY "ENTER CHILD NAME: "
           ACCEPT WK-SEARCH-NAME
           
           DISPLAY "ENTER CHILD BIRTH DATE (YYYYMMDD): "
           ACCEPT WK-RIDER-EFFECTIVE-DATE
           
           DISPLAY "ENTER RIDER AMOUNT: "
           ACCEPT WK-RIDER-AMOUNT.

       620-ADD-CHILD-RIDER-TO-POLICY.
      * Add child rider to the policy
           MOVE WK-POLICY-NUMBER TO PB-POLNUM
           MOVE "CH" TO PB-RECORD-TYPE
           MOVE WK-RIDER-AMOUNT TO PB-DAILY-BENEFIT
           MOVE WK-RIDER-EFFECTIVE-DATE TO PB-ISSUE-DATE
           
           MOVE MSG-RIDER-ADDED TO IS-MESSAGE-LINE
           DISPLAY IS-MESSAGE-LINE.

       700-ADDRESS-CHANGE.
      * Process address change
           PERFORM 710-GET-POLICY-FOR-ADDRESS
           
           IF RECORD-FOUND
               PERFORM 720-GET-NEW-ADDRESS
               PERFORM 730-UPDATE-ADDRESS
               PERFORM 790-LOG-ADDRESS-CHANGE
           END-IF.

       710-GET-POLICY-FOR-ADDRESS.
      * Get policy number for address change
           DISPLAY "ENTER POLICY NUMBER: "
           ACCEPT WK-POLICY-NUMBER
           
           PERFORM 950-VALIDATE-POLICY-NUMBER.

       720-GET-NEW-ADDRESS.
      * Get the new address information
           DISPLAY "ENTER NEW ADDRESS LINE 1: "
           ACCEPT WK-NEW-ADDR1
           
           DISPLAY "ENTER NEW ADDRESS LINE 2 (OPTIONAL): "
           ACCEPT WK-NEW-ADDR2
           
           DISPLAY "ENTER CITY: "
           ACCEPT WK-NEW-CITY
           
           DISPLAY "ENTER STATE (2 CHARS): "
           ACCEPT WK-NEW-STATE
           
           DISPLAY "ENTER ZIP CODE: "
           ACCEPT WK-NEW-ZIP.

       730-UPDATE-ADDRESS.
      * Update the address in the insured record
      * This would update the POL-INSURED file
           MOVE WK-NEW-ADDR1 TO PI-LAST-NAME
      * Update other address fields as needed
           
           MOVE MSG-SUCCESSFUL-UPDATE TO IS-MESSAGE-LINE
           DISPLAY IS-MESSAGE-LINE.

       790-LOG-ADDRESS-CHANGE.
      * Log the address change transaction
           MOVE WK-POLICY-NUMBER TO TRAN-POLICY-NUM
           MOVE WK-CURRENT-DATE TO TRAN-DATE
           MOVE "05" TO TRAN-FUNCTION
           MOVE "ADDRESS CHANGE" TO TRAN-DESCRIPTION.

       800-ADD-PREAUTH-CHECK.
      * Add pre-authorized checking information
           PERFORM 710-GET-POLICY-FOR-ADDRESS
           
           IF RECORD-FOUND
               PERFORM 810-GET-BANK-INFORMATION
               PERFORM 820-UPDATE-BANK-INFO
               PERFORM 890-LOG-PREAUTH-CHANGE
           END-IF.

       810-GET-BANK-INFORMATION.
      * Get banking information
           DISPLAY "ENTER BANK NAME: "
           ACCEPT WK-BANK-NAME
           
           DISPLAY "ENTER BANK ROUTING NUMBER: "
           ACCEPT WK-BANK-ROUTING
           
           DISPLAY "ENTER ACCOUNT NUMBER: "
           ACCEPT WK-ACCOUNT-NUMBER
           
           DISPLAY "ENTER ACCOUNT TYPE (C=CHECKING, S=SAVINGS): "
           ACCEPT WK-ACCOUNT-TYPE.

       820-UPDATE-BANK-INFO.
      * Update banking information in policy record
      * This would update appropriate fields in POL-MASTER
           MOVE MSG-SUCCESSFUL-UPDATE TO IS-MESSAGE-LINE
           DISPLAY IS-MESSAGE-LINE.

       850-PREMIUM-INCREASE.
      * Process premium increase
           PERFORM 710-GET-POLICY-FOR-ADDRESS
           
           IF RECORD-FOUND
               PERFORM 855-GET-NEW-PREMIUM
               PERFORM 865-UPDATE-PREMIUM
               PERFORM 895-LOG-PREMIUM-CHANGE
           END-IF.

       855-GET-NEW-PREMIUM.
      * Get new premium amount
           MOVE PM-COMM-PREM TO WK-CURRENT-PREMIUM
           DISPLAY "CURRENT PREMIUM: " WK-CURRENT-PREMIUM
           
           DISPLAY "ENTER NEW PREMIUM AMOUNT: "
           ACCEPT WK-NEW-PREMIUM
           
           COMPUTE WK-PREMIUM-DIFFERENCE = 
               WK-NEW-PREMIUM - WK-CURRENT-PREMIUM.

       860-PREMIUM-DECREASE.
      * Process premium decrease
           PERFORM 710-GET-POLICY-FOR-ADDRESS
           
           IF RECORD-FOUND
               PERFORM 855-GET-NEW-PREMIUM
               PERFORM 865-UPDATE-PREMIUM
               PERFORM 895-LOG-PREMIUM-CHANGE
           END-IF.

       865-UPDATE-PREMIUM.
      * Update the premium amount
           MOVE WK-NEW-PREMIUM TO PM-COMM-PREM
      * UPDATE POL-MASTER RECORD
           
           MOVE MSG-SUCCESSFUL-UPDATE TO IS-MESSAGE-LINE
           DISPLAY IS-MESSAGE-LINE.

       870-DELETE-RIDER.
      * Delete a rider from policy
           PERFORM 710-GET-POLICY-FOR-ADDRESS
           
           IF RECORD-FOUND
               PERFORM 875-SELECT-RIDER-TO-DELETE
               PERFORM 876-DELETE-SELECTED-RIDER
               PERFORM 590-LOG-TRANSACTION
           END-IF.

       875-SELECT-RIDER-TO-DELETE.
      * Select which rider to delete
           DISPLAY "ENTER RIDER CODE TO DELETE: "
           ACCEPT WK-RIDER-CODE.

       876-DELETE-SELECTED-RIDER.
      * Delete the selected rider
           MOVE WK-POLICY-NUMBER TO PB-POLNUM
           MOVE WK-RIDER-CODE TO PB-RECORD-TYPE
      * DELETE POL-BENEFIT-REC
           
           MOVE MSG-RIDER-DELETED TO IS-MESSAGE-LINE
           DISPLAY IS-MESSAGE-LINE.

       880-UPDATE-RIDER.
      * Update an existing rider
           PERFORM 710-GET-POLICY-FOR-ADDRESS
           
           IF RECORD-FOUND
               PERFORM 875-SELECT-RIDER-TO-DELETE
               PERFORM 885-GET-UPDATED-RIDER-INFO
               PERFORM 886-UPDATE-SELECTED-RIDER
               PERFORM 590-LOG-TRANSACTION
           END-IF.

       885-GET-UPDATED-RIDER-INFO.
      * Get updated rider information
           DISPLAY "ENTER NEW RIDER AMOUNT: "
           ACCEPT WK-RIDER-AMOUNT.

       886-UPDATE-SELECTED-RIDER.
      * Update the selected rider
           MOVE WK-RIDER-AMOUNT TO PB-DAILY-BENEFIT
      * UPDATE POL-BENEFIT-REC
           
           MOVE MSG-SUCCESSFUL-UPDATE TO IS-MESSAGE-LINE
           DISPLAY IS-MESSAGE-LINE.

       890-PREMIUM-CHANGE.
      * General premium change processing
           PERFORM 710-GET-POLICY-FOR-ADDRESS
           
           IF RECORD-FOUND
               PERFORM 855-GET-NEW-PREMIUM
               PERFORM 865-UPDATE-PREMIUM
               PERFORM 895-LOG-PREMIUM-CHANGE
           END-IF.

       895-LOG-PREMIUM-CHANGE.
      * Log premium change transaction
           MOVE WK-POLICY-NUMBER TO TRAN-POLICY-NUM
           MOVE WK-CURRENT-DATE TO TRAN-DATE
           MOVE "11" TO TRAN-FUNCTION
           MOVE "PREMIUM CHANGE" TO TRAN-DESCRIPTION
           MOVE WK-CURRENT-PREMIUM TO TRAN-OLD-VALUE
           MOVE WK-NEW-PREMIUM TO TRAN-NEW-VALUE.

       900-CLEANUP.
      * Cleanup and close files
           DISPLAY " "
           DISPLAY "POLICY INQUIRY SESSION COMPLETED"
           DISPLAY " ".

       950-VALIDATE-POLICY-NUMBER.
      * Validate that policy number exists
           SET NO-RECORD-FOUND TO TRUE
           
           IF WK-POLICY-NUMBER NOT = SPACES
               MOVE WK-POLICY-NUMBER TO PM-POLNUM
      * READ POL-MASTER-REC WITH KEY = PM-POLNUM
               IF MASTER-FILE-STATUS = "00"
                   SET RECORD-FOUND TO TRUE
               ELSE
                   MOVE MSG-POLICY-NOT-FOUND TO IS-MESSAGE-LINE
                   DISPLAY IS-MESSAGE-LINE
               END-IF
           ELSE
               DISPLAY "POLICY NUMBER CANNOT BE BLANK"
           END-IF.

       960-READ-POLICY-MASTER.
      * Read the policy master record
      * READ POL-MASTER-REC
           IF MASTER-FILE-STATUS = "00"
               MOVE PM-POLNUM TO IS-POLICY-NUMBER
               MOVE PM-COMM-PREM TO WK-CURRENT-PREMIUM
           END-IF.

      * Additional utility paragraphs would be added here for:
      * - File I/O operations
      * - Date validation routines  
      * - Amount validation routines
      * - Screen display routines
      * - Error handling routines

      *END PROGRAM POLINQ01.