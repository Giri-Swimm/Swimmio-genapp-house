---
title: Untitled doc (5)
---
# Introduction

This document explains the design and implementation of the policy inquiry system in <SwmPath>[base/src/sample.cbl](/base/src/sample.cbl)</SwmPath>. The program provides a menu-driven interface for policy search and maintenance operations. The walkthrough will address:

1. Why the program uses a menu-driven main loop and how user selections are processed.
2. How the code organizes and separates different policy operations (search, add, update, delete).
3. Why transaction logging is implemented for changes.
4. How error handling and user feedback are managed.

# Main control flow and menu handling

<SwmSnippet path="/base/src/sample.cbl" line="187">

---

The program uses a main control loop to repeatedly display a menu, accept user input, and dispatch to the appropriate operation. This structure keeps the user in control and allows multiple operations per session.

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

<SwmSnippet path="/base/src/sample.cbl" line="195">

---

Initialization sets up the environment and screen buffer, ensuring a clean state for each session.

```
       100-INITIALIZE.
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE
           MOVE CURRENT-DATE(1:8) TO WK-CURRENT-DATE
           INITIALIZE INQUIRY-SCREEN-BUF
                      ERROR-SWITCHES
                      WK-FOUND-SWITCH
           MOVE "POLICY INQUIRY SYSTEM - READY" TO IS-MESSAGE-LINE.
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/sample.cbl" line="203">

---

The main processing paragraph displays the menu, gets the user's selection, and dispatches to the correct operation using an EVALUATE statement. This makes it easy to add or modify menu options.

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

<SwmSnippet path="/base/src/sample.cbl" line="234">

---

The menu display is explicit and user-friendly, listing all available operations and the exit option.

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

<SwmSnippet path="/base/src/sample.cbl" line="256">

---

User selection is accepted and checked for the exit code, which cleanly ends the session.

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

The code separates search by agent and search by insured into distinct paragraphs. This keeps logic clear and maintainable.

<SwmSnippet path="/base/src/sample.cbl" line="263">

---

Searching by agent prompts for the agent's last name, finds matching policies, and displays results or an error message.

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

<SwmSnippet path="/base/src/sample.cbl" line="281">

---

The actual search logic is stubbed for demonstration but would normally involve reading and cross-referencing master files.

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

<SwmSnippet path="/base/src/sample.cbl" line="294">

---

If policies are found, they are displayed in a formatted list.

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

<SwmSnippet path="/base/src/sample.cbl" line="305">

---

A similar structure is used for searching by insured name, keeping the interface consistent.

```
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
```

---

</SwmSnippet>

# Policy maintenance operations

Each maintenance operation (add rider, address change, premium change, etc.) is handled in its own paragraph. This modular approach makes the code easier to test and extend.

<SwmSnippet path="/base/src/sample.cbl" line="342">

---

For example, adding a benefit rider involves getting the policy, collecting rider details, updating the benefit file, and logging the transaction.

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

<SwmSnippet path="/base/src/sample.cbl" line="352">

---

Getting the policy for rider addition validates the policy number and loads the master record.

```
       510-GET-POLICY-FOR-RIDER.
      * Get policy number for rider addition
           DISPLAY "ENTER POLICY NUMBER: "
           ACCEPT WK-POLICY-NUMBER
           
           PERFORM 950-VALIDATE-POLICY-NUMBER
           
           IF RECORD-FOUND
               PERFORM 960-READ-POLICY-MASTER
           END-IF.
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/sample.cbl" line="363">

---

Rider details are collected interactively.

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

<SwmSnippet path="/base/src/sample.cbl" line="374">

---

The new rider is added to the benefit file, and a success message is displayed.

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

<SwmSnippet path="/base/src/sample.cbl" line="386">

---

Every change to a policy (rider addition, address change, premium change, etc.) is logged in a transaction record. This provides an audit trail and supports troubleshooting.

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

# Error handling and user feedback

<SwmSnippet path="/base/src/sample.cbl" line="145">

---

The program uses message templates for common errors and success messages. This keeps user feedback consistent and easy to update.

```
      * MESSAGE TEMPLATES
       01  MSG-POLICY-NOT-FOUND          PIC X(60) VALUE 
           "POLICY NOT FOUND - PLEASE VERIFY POLICY NUMBER".
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/sample.cbl" line="98">

---

Error switches and validation routines are used to track and report errors, ensuring the user is informed of any issues.

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

# Validation and utility routines

<SwmSnippet path="/base/src/sample.cbl" line="619">

---

Policy number validation ensures that operations are only performed on valid policies, reducing the risk of data corruption.

```
       950-VALIDATE-POLICY-NUMBER.
      * Validate that policy number exists
           SET NO-RECORD-FOUND TO TRUE
           
           IF WK-POLICY-NUMBER NOT = SPACES
               MOVE WK-POLICY-NUMBER TO PM-POLNUM
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/sample.cbl" line="625">

---

If the policy is not found, a clear error message is displayed.

```
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
```

---

</SwmSnippet>

# Cleanup

<SwmSnippet path="/base/src/sample.cbl" line="613">

---

At the end of the session, the program displays a completion message and closes files, ensuring resources are released properly.

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

- The menu-driven main loop keeps user interaction simple and repeatable.
- Each operation is modular, making the code maintainable and extensible.
- Transaction logging provides accountability for all changes.
- Error handling and user feedback are consistent and clear.
- Validation routines prevent invalid operations.

This structure supports both maintainability and reliability for policy inquiry and maintenance tasks.

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
