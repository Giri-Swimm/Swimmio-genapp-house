---
title: LGDPDB01 - Deleting policy records - Overview
---
# Overview

This document describes the process for deleting a policy record from the database based on a request containing customer and policy numbers and a supported request type. The flow validates the request, ensures the request type is supported, and deletes the policy record if all conditions are met. If the deletion fails or the request is invalid, an error code is returned.

```mermaid
flowchart TD
    node1["Validating and preparing request data
(Validating and preparing request data)"]:::HeadingStyle --> node2{"Is commarea length sufficient?
(Validating and preparing request data)"}:::HeadingStyle
    click node1 goToHeading "Validating and preparing request data"
    node2 -->|"No (error '98')"| node5["Completing the request and error handling"]:::HeadingStyle
    click node2 goToHeading "Validating and preparing request data"
    node2 -->|"Yes"| node3{"Is request type supported?
(Validating and preparing request data)"}:::HeadingStyle
    node3 -->|"No (error '99')"| node5
    node3 -->|"Yes"| node4["Deleting policy record in DB2
(Deleting policy record in DB2)"]:::HeadingStyle
    click node3 goToHeading "Validating and preparing request data"
    click node4 goToHeading "Deleting policy record in DB2"
    node4 --> node6{"Did deletion succeed?
(Deleting policy record in DB2)"}:::HeadingStyle
    node6 -->|"Yes"| node5
    node6 -->|"No (error '90' or '81')"| node5
    click node6 goToHeading "Deleting policy record in DB2"
    click node5 goToHeading "Completing the request and error handling"
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
  eee7n("(LGDPOL01) Deleting policy business logic") --> 0986i("LGDPDB01 Delete Policy Records"):::currentEntity
click eee7n openCode "base/src/lgdpol01.cbl:1"
  
  
click 0986i openCode "base/src/lgdpdb01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   eee7n("(LGDPOL01) Deleting policy business logic") --> 0986i("LGDPDB01 Delete Policy Records"):::currentEntity
%% click eee7n openCode "<SwmPath>[base/src/lgdpol01.cbl](base/src/lgdpol01.cbl)</SwmPath>:1"
%%   
%%   
%% click 0986i openCode "<SwmPath>[base/src/lgdpdb01.cbl](base/src/lgdpdb01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
