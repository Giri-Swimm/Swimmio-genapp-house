---
title: Updating Policy Records (LGUPVS01) - Overview
---
# Overview

This document explains the flow of updating policy records. The system receives a policy update request, determines the policy type, updates only the relevant fields, and writes the changes to the policy record. Errors encountered during the process are logged for audit and troubleshooting.

```mermaid
flowchart TD
    node1["Starting the Policy Update"]:::HeadingStyle --> node2{"Was policy record retrieved?"}
    node2 -->|"Yes"| node3["Rewriting the Policy Record"]:::HeadingStyle
    node2 -->|"No"| node4["Process terminates (error logged)"]
    node3 --> node5{"Was update successful?"}
    node5 -->|"No"| node4
    click node1 goToHeading "Starting the Policy Update"
    click node3 goToHeading "Rewriting the Policy Record"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Programs

- LGUPVS01 (<SwmPath>[base/src/lgupvs01.cbl](base/src/lgupvs01.cbl)</SwmPath>)
- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)

### Copybook

- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  syvg1("Updating Policy details (LGUPDB01)") --> okjai("Updating Policy Records (LGUPVS01)"):::currentEntity
click syvg1 openCode "base/src/lgupdb01.cbl:1"
  
  
click okjai openCode "base/src/lgupvs01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   syvg1("Updating Policy details (LGUPDB01)") --> okjai("Updating Policy Records (LGUPVS01)"):::currentEntity
%% click syvg1 openCode "<SwmPath>[base/src/lgupdb01.cbl](base/src/lgupdb01.cbl)</SwmPath>:1"
%%   
%%   
%% click okjai openCode "<SwmPath>[base/src/lgupvs01.cbl](base/src/lgupvs01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
