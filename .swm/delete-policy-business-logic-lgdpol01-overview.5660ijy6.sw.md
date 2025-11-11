---
title: Delete Policy Business Logic (LGDPOL01) - Overview
---
# Overview

This document describes the flow for deleting a policy record. The process validates the request, ensures only supported policy types are processed, and coordinates deletion from both the database and file system, with clear status feedback and error logging.

```mermaid
flowchart TD
  node1["Starting the transaction and preparing context"]:::HeadingStyle --> node2["Validating input and request type"]:::HeadingStyle
  click node1 goToHeading "Starting the transaction and preparing context"
  click node2 goToHeading "Validating input and request type"
  node2 -->|"Valid request"| node3["Delegating to DB2 policy deletion"]:::HeadingStyle
  click node3 goToHeading "Delegating to DB2 policy deletion"
  node3 --> node4["Processing DB2 policy deletion and error reporting"]:::HeadingStyle
  click node4 goToHeading "Processing DB2 policy deletion and error reporting"
  node4 -->|"DB2 deletion successful"| node5["Deleting policy from VSAM and error handling"]:::HeadingStyle
  click node5 goToHeading "Deleting policy from VSAM and error handling"
  node2 -->|"Invalid request"| node6["Outcome reported to caller"]
  node4 -->|"DB2 deletion failed"| node6
  node5 -->|"VSAM deletion failed"| node6
  node5 -->|"VSAM deletion successful"| node6
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Programs

- LGDPOL01 (<SwmPath>[base/src/lgdpol01.cbl](base/src/lgdpol01.cbl)</SwmPath>)
- LGDPDB01 (<SwmPath>[base/src/lgdpdb01.cbl](base/src/lgdpdb01.cbl)</SwmPath>)
- LGDPVS01 (<SwmPath>[base/src/lgdpvs01.cbl](base/src/lgdpvs01.cbl)</SwmPath>)
- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)

### Copybooks

- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)
- SQLCA

# Where is this program used?

This program is used multiple times in the codebase as represented in the following diagram:

```mermaid
graph TD
  eg39h("Managing Commercial Policy Operations (LGTESTP4)") --> w3o3m("Delete Policy Business Logic (LGDPOL01)"):::currentEntity
click eg39h openCode "base/src/lgtestp4.cbl:1"
e7w34("House Policy Menu (LGTESTP3)") --> w3o3m("Delete Policy Business Logic (LGDPOL01)"):::currentEntity
click e7w34 openCode "base/src/lgtestp3.cbl:1"
1bhkq("Motor Policy Menu (LGTESTP1)") --> w3o3m("Delete Policy Business Logic (LGDPOL01)"):::currentEntity
click 1bhkq openCode "base/src/lgtestp1.cbl:1"
s5qxh("Endowment Policy Menu (LGTESTP2)") --> w3o3m("Delete Policy Business Logic (LGDPOL01)"):::currentEntity
click s5qxh openCode "base/src/lgtestp2.cbl:1"
  
  
click w3o3m openCode "base/src/lgdpol01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   eg39h("Managing Commercial Policy Operations (LGTESTP4)") --> w3o3m("Delete Policy Business Logic (LGDPOL01)"):::currentEntity
%% click eg39h openCode "<SwmPath>[base/src/lgtestp4.cbl](base/src/lgtestp4.cbl)</SwmPath>:1"
%% e7w34("House Policy Menu (LGTESTP3)") --> w3o3m("Delete Policy Business Logic (LGDPOL01)"):::currentEntity
%% click e7w34 openCode "<SwmPath>[base/src/lgtestp3.cbl](base/src/lgtestp3.cbl)</SwmPath>:1"
%% 1bhkq("Motor Policy Menu (LGTESTP1)") --> w3o3m("Delete Policy Business Logic (LGDPOL01)"):::currentEntity
%% click 1bhkq openCode "<SwmPath>[base/src/lgtestp1.cbl](base/src/lgtestp1.cbl)</SwmPath>:1"
%% s5qxh("Endowment Policy Menu (LGTESTP2)") --> w3o3m("Delete Policy Business Logic (LGDPOL01)"):::currentEntity
%% click s5qxh openCode "<SwmPath>[base/src/lgtestp2.cbl](base/src/lgtestp2.cbl)</SwmPath>:1"
%%   
%%   
%% click w3o3m openCode "<SwmPath>[base/src/lgdpol01.cbl](base/src/lgdpol01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
