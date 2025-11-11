---
title: Deleting Policy Records (LGDPDB01) - Overview
---
# Overview

This document describes the flow for deleting a policy record. The process validates the request, deletes the policy from DB2, and cleans up related records in VSAM, logging any errors for traceability.

```mermaid
flowchart TD
    node1["Validating input and preparing for DB2 operations
(Validating input and preparing for DB2 operations)"]:::HeadingStyle --> node2{"Is input valid? (commarea length, supported request)
(Validating input and preparing for DB2 operations)"}:::HeadingStyle
    node2 -->|"No"| node5["Log error
(Cleaning up VSAM records and logging errors)"]:::HeadingStyle
    node2 -->|"Yes"| node3["Deleting the policy record in DB2 and handling errors
(Deleting the policy record in DB2 and handling errors)"]:::HeadingStyle
    node3 --> node4{"Did DB2 deletion succeed?
(Deleting the policy record in DB2 and handling errors)"}:::HeadingStyle
    node4 -->|"No"| node5
    node4 -->|"Yes"| node6["Cleaning up VSAM records and logging errors
(Cleaning up VSAM records and logging errors)"]:::HeadingStyle
    node6 --> node7{"Did VSAM deletion succeed?
(Cleaning up VSAM records and logging errors)"}:::HeadingStyle
    node7 -->|"No"| node5
    node7 -->|"Yes"| node6

    click node1 goToHeading "Validating input and preparing for DB2 operations"
    click node2 goToHeading "Validating input and preparing for DB2 operations"
    click node3 goToHeading "Deleting the policy record in DB2 and handling errors"
    click node4 goToHeading "Deleting the policy record in DB2 and handling errors"
    click node5 goToHeading "Cleaning up VSAM records and logging errors"
    click node6 goToHeading "Cleaning up VSAM records and logging errors"
    click node7 goToHeading "Cleaning up VSAM records and logging errors"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Programs

- LGDPDB01 (<SwmPath>[base/src/lgdpdb01.cbl](base/src/lgdpdb01.cbl)</SwmPath>)
- LGDPVS01 (<SwmPath>[base/src/lgdpvs01.cbl](base/src/lgdpvs01.cbl)</SwmPath>)
- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)

### Copybooks

- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)
- SQLCA

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  mlmz4("Delete Policy Business Logic (LGDPOL01)") --> 0tohc("Deleting Policy Records (LGDPDB01)"):::currentEntity
click mlmz4 openCode "base/src/lgdpol01.cbl:1"
  
  
click 0tohc openCode "base/src/lgdpdb01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   mlmz4("Delete Policy Business Logic (LGDPOL01)") --> 0tohc("Deleting Policy Records (LGDPDB01)"):::currentEntity
%% click mlmz4 openCode "<SwmPath>[base/src/lgdpol01.cbl](base/src/lgdpol01.cbl)</SwmPath>:1"
%%   
%%   
%% click 0tohc openCode "<SwmPath>[base/src/lgdpdb01.cbl](base/src/lgdpdb01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
