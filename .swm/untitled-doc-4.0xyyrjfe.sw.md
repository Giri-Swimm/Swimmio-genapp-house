---
title: Sample program report  
---
# Introduction

This document explains the design and main implementation points of the policy inquiry system in <SwmPath>[base/src/Sample.cbl](/base/src/Sample.cbl)</SwmPath>. The program provides a menu-driven interface for policy lookup and maintenance, supporting search, update, and transaction logging for insurance policies.

We will cover:

1. Why the program is structured around a main menu and function codes.
2. How the program manages user input and dispatches to the correct logic.
3. How policy search and update operations are implemented.
4. Why transaction logging is included for all changes.
5. How error handling and messaging are managed.

# Program structure and main menu

<SwmSnippet path="/base/src/Sample.cbl" line="187">

---

The program is organized around a main control loop that initializes the environment, repeatedly processes user requests, and then performs cleanup. This structure ensures that each user action is handled in isolation and the session is properly closed.

```
       PROCEDURE DIVISION.

       000-MAIN-CONTROL.
           PERFORM 100-INITIALIZE
           PERFORM 200-PROCESS-INQUIRY UNTIL NO-MORE-REQUESTS
           PERFORM 900-CLEANUP
           STOP RUN.
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/Sample.cbl" line="234">

---

The main menu is displayed to the user, listing all available operations. This menu-driven approach makes it clear what actions are supported and allows for straightforward expansion if new features are needed.

```
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
```

---

</SwmSnippet>

# Function code dispatch and user input

<SwmSnippet path="/base/src/Sample.cbl" line="57">

---

User input is collected for the function code, which determines what operation to perform. The use of function codes mapped to specific actions (e.g., "01" for search by agent, "03" for add benefit rider) allows for simple and maintainable dispatch logic.

```
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
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/Sample.cbl" line="203">

---

The main processing routine evaluates the function code and calls the corresponding paragraph. This avoids a large, monolithic block of code and keeps each operation isolated.

```
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
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/Sample.cbl" line="256">

---

User selection is handled with a simple ACCEPT statement, and the session can be exited by entering "XX". This keeps the interaction model simple.

```
       220-GET-USER-SELECTION.
      * Get user's function selection
           ACCEPT WK-FUNCTION-CODE
           IF WK-FUNCTION-CODE = "XX"
               SET NO-MORE-REQUESTS TO TRUE
           END-IF.
```

---

</SwmSnippet>

# Policy search operations

Searching by agent or insured name is implemented as a two-step process: prompt for the search key, then attempt to find matching records. If found, the results are displayed; otherwise, an appropriate message is shown.

<SwmSnippet path="/base/src/Sample.cbl" line="263">

---

The agent search logic demonstrates the pattern: prompt, search, display or error.

```
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
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/Sample.cbl" line="281">

---

The actual search is abstracted (in a real system, this would involve file <SwmToken path="/base/src/Sample.cbl" pos="645:7:9" line-data="      * - File I/O operations">`I/O`</SwmToken>), but the structure is in place for cross-referencing agent and policy files.

```
      * This would normally read through AGENT-MASTER file
      * and cross-reference with POL-MASTER file
      * Simplified logic for demonstration:
           
           MOVE WK-SEARCH-NAME TO A-LAST-NAME
      * READ AGENT-MASTER FILE WITH KEY = A-LAST-NAME
      * IF FOUND, GET A-AGENT-ID AND SEARCH POL-MASTER
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/Sample.cbl" line="294">

---

If a match is found, the results are displayed in a formatted table.

```
       320-DISPLAY-AGENT-POLICIES.
      * Display policies found for the agent
           DISPLAY " "
           DISPLAY "POLICIES FOR AGENT: " WK-SEARCH-NAME
           DISPLAY "======================================"
           DISPLAY "POLICY NUMBER  INSURED NAME        PREMIUM"
           DISPLAY "============  =================  ========="
```

---

</SwmSnippet>

# Policy update and maintenance operations

Adding a benefit rider, child rider, or making address changes all follow a similar pattern: prompt for the policy number, validate it, collect additional details, perform the update, and log the transaction.

<SwmSnippet path="/base/src/Sample.cbl" line="342">

---

For example, adding a benefit rider:

```
       500-ADD-BENEFIT-RIDER.
      * Add a benefit rider to a policy
           PERFORM 510-GET-POLICY-FOR-RIDER
           
           IF RECORD-FOUND
               PERFORM 520-GET-RIDER-DETAILS
               PERFORM 530-ADD-RIDER-TO-POLICY
               PERFORM 590-LOG-TRANSACTION
           END-IF.
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/Sample.cbl" line="619">

---

Policy number validation is always performed before any update, ensuring that only valid policies are modified.

```
       950-VALIDATE-POLICY-NUMBER.
      * Validate that policy number exists
           SET NO-RECORD-FOUND TO TRUE
           
           IF WK-POLICY-NUMBER NOT = SPACES
               MOVE WK-POLICY-NUMBER TO PM-POLNUM
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/Sample.cbl" line="636">

---

After validation, the policy master record is read to get current values.

```
       960-READ-POLICY-MASTER.
      * Read the policy master record
      * READ POL-MASTER-REC
           IF MASTER-FILE-STATUS = "00"
               MOVE PM-POLNUM TO IS-POLICY-NUMBER
               MOVE PM-COMM-PREM TO WK-CURRENT-PREMIUM
           END-IF.
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/Sample.cbl" line="363">

---

Details for the rider are collected from the user.

```
       520-GET-RIDER-DETAILS.
      * Get details for the new rider
           DISPLAY "ENTER RIDER CODE (4 CHARS): "
           ACCEPT WK-RIDER-CODE
           
           DISPLAY "ENTER RIDER AMOUNT: "
           ACCEPT WK-RIDER-AMOUNT
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/Sample.cbl" line="374">

---

The update is performed, and a confirmation message is displayed.

```
       530-ADD-RIDER-TO-POLICY.
      * Add the rider to the benefit file
           MOVE WK-POLICY-NUMBER TO PB-POLNUM
           MOVE WK-RIDER-CODE TO PB-RECORD-TYPE
           MOVE WK-RIDER-AMOUNT TO PB-DAILY-BENEFIT
           MOVE WK-RIDER-EFFECTIVE-DATE TO PB-ISSUE-DATE
```

---

</SwmSnippet>

# Transaction logging

<SwmSnippet path="/base/src/Sample.cbl" line="386">

---

Every change to a policy (rider addition, address change, premium update, etc.) is logged in a transaction record. This provides an audit trail and supports troubleshooting and compliance.

```
       590-LOG-TRANSACTION.
      * Log the transaction
           MOVE WK-POLICY-NUMBER TO TRAN-POLICY-NUM
           MOVE WK-CURRENT-DATE TO TRAN-DATE
           MOVE "03" TO TRAN-FUNCTION
           MOVE "BENEFIT RIDER ADDED" TO TRAN-DESCRIPTION
           MOVE WK-RIDER-CODE TO TRAN-NEW-VALUE
```

---

</SwmSnippet>

# Error handling and messaging

<SwmSnippet path="/base/src/Sample.cbl" line="98">

---

Error handling is managed with switches and message templates. When an operation fails (e.g., policy not found, invalid input), a clear message is displayed to the user. This keeps the user informed and avoids silent failures.

```
      * ERROR HANDLING
       01  ERROR-SWITCHES.
           05  MAIN-ERROR-SW             PIC X VALUE "N".
               88  NO-ERRORS                   VALUE "N".
               88  THERE-ARE-ERRORS            VALUE "Y".
           05  VALIDATION-ERROR-SW       PIC X VALUE "N".
               88  NO-VALIDATION-ERRORS        VALUE "N".
               88  VALIDATION-ERRORS-FOUND     VALUE "Y".
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/Sample.cbl" line="145">

---

Message templates are defined for all common error and success cases, ensuring consistency in user feedback.

```
      * MESSAGE TEMPLATES
       01  MSG-POLICY-NOT-FOUND          PIC X(60) VALUE 
           "POLICY NOT FOUND - PLEASE VERIFY POLICY NUMBER".
```

---

</SwmSnippet>

# Cleanup

<SwmSnippet path="/base/src/Sample.cbl" line="613">

---

At the end of the session, a cleanup routine is called to display a completion message and close out the session. This is important for user experience and resource management.

```
       900-CLEANUP.
      * Cleanup and close files
           DISPLAY " "
           DISPLAY "POLICY INQUIRY SESSION COMPLETED"
           DISPLAY " ".
```

---

</SwmSnippet>

# Summary

- The program is menu-driven for clarity and extensibility.
- Function codes and dispatch logic keep operations modular.
- All updates are validated and logged for safety and traceability.
- Error handling and messaging are consistent and user-focused.

This structure makes the code easy to maintain, extend, and audit.

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
