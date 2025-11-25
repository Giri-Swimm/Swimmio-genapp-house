---
title: Untitled doc (4)
---
# Introduction

This document explains the implementation of the new customer password insertion logic in the existing COBOL program <SwmPath>[base/src/lgacdb02.cbl](/base/src/lgacdb02.cbl)</SwmPath>. We will cover:

1. How the program distinguishes the new customer request type and triggers the password insertion.
2. How the insertion of customer password data into the <SwmToken path="/base/src/lgacdb02.cbl" pos="167:5:5" line-data="             INSERT INTO CUSTOMER_SECURE">`CUSTOMER_SECURE`</SwmToken> table is performed.
3. How errors during the SQL insertion are handled and reported.

# handling new customer requests

The program evaluates the request type using the field <SwmToken path="/base/src/lgacdb02.cbl" pos="143:3:7" line-data="           Evaluate D2-REQUEST-ID">`D2-REQUEST-ID`</SwmToken>. When it matches the code for a new customer (<SwmToken path="/base/src/lgacdb02.cbl" pos="145:4:4" line-data="             When &#39;02ACUS&#39;">`02ACUS`</SwmToken>), it extracts customer number and password-related counts from the input and prepares them for database insertion. If the request type is not recognized, it sets a return code and terminates the transaction immediately.

<SwmSnippet path="/base/src/lgacdb02.cbl" line="142">

---

This conditional branching ensures that only valid new customer requests proceed to the insertion step, preventing invalid data from being processed.

```
      * Different types of security add
           Evaluate D2-REQUEST-ID
      *      New Customer add
             When '02ACUS'
               Move D2-CUSTOMER-NUM    To DB2-CUSTOMERNUM-INT
               Move D2-CUSTSECR-COUNT  To DB2-CUSTOMERCNT-INT
               Perform INSERT-CUSTOMER-PASSWORD
             When Other
               Move '99' To D2-RETURN-CODE
               Exec CICS Return End-EXEC
           End-Evaluate
```

---

</SwmSnippet>

# inserting customer password data

The insertion logic is encapsulated in the <SwmToken path="/base/src/lgacdb02.cbl" pos="148:3:7" line-data="               Perform INSERT-CUSTOMER-PASSWORD">`INSERT-CUSTOMER-PASSWORD`</SwmToken> paragraph. It constructs an SQL INSERT statement targeting the <SwmToken path="/base/src/lgacdb02.cbl" pos="167:5:5" line-data="             INSERT INTO CUSTOMER_SECURE">`CUSTOMER_SECURE`</SwmToken> table, inserting customer number, password, state indicator, and password change count.

<SwmSnippet path="/base/src/lgacdb02.cbl" line="154">

---

This separation of concerns keeps the SQL logic isolated and reusable. The SQL statement uses host variables populated earlier from the request data, ensuring data consistency between the input and the database operation.

```
      *    Return to caller
           EXEC CICS RETURN END-EXEC.

       MAINLINE-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *================================================================*
       INSERT-CUSTOMER-PASSWORD.
      *================================================================*
      * Insert row into Customer Secure Table                          *
      *================================================================*
           MOVE ' INSERT SECURITY' TO EM-SQLREQ
           EXEC SQL
             INSERT INTO CUSTOMER_SECURE
                       ( customerNumber,
                         customerPass,
                         state_indicator,
                         pass_changes   )
                VALUES ( :DB2-CUSTOMERNUM-INT,
                         :D2-CUSTSECR-PASS,
                         :D2-CUSTSECR-STATE,
                         :DB2-CUSTOMERCNT-INT)
           END-EXEC
```

---

</SwmSnippet>

# error handling after insertion

After executing the SQL INSERT, the program checks the SQLCODE to detect any errors. If an error occurred (SQLCODE not zero), it sets a specific return code ('98'), logs the error message, and terminates the transaction.

<SwmSnippet path="/base/src/lgacdb02.cbl" line="178">

---

This immediate error detection and handling prevents the program from continuing in an inconsistent state and provides feedback for troubleshooting.

```
           IF SQLCODE NOT EQUAL 0
             MOVE '98' TO D2-RETURN-CODE
             PERFORM WRITE-ERROR-MESSAGE
             EXEC CICS RETURN END-EXEC
           END-IF
```

---

</SwmSnippet>

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
