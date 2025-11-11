---
title: Updating Policy details (LGUPDB01) - Overview
---
# Overview

This document explains the flow for updating insurance policy details for endowment, house, and motor products. Incoming requests are validated, routed to the correct policy type, and used to update policy records in both DB2 and VSAM databases. The process ensures data integrity through timestamp checks and provides clear status codes for all outcomes.

```mermaid
flowchart TD
  node1["Validating and updating DB2 policy record
(Validating and updating DB2 policy record)"]:::HeadingStyle
  click node1 goToHeading "Validating and updating DB2 policy record"
  node1 --> node2{"Do timestamps match?
(Validating and updating DB2 policy record)"}:::HeadingStyle
  click node2 goToHeading "Validating and updating DB2 policy record"
  node2 -->|"No"| node5["Finalizing policy record update
(Abort: Concurrency error)
(Finalizing policy record update)"]:::HeadingStyle
  click node5 goToHeading "Finalizing policy record update"
  node2 -->|"Yes"| node3["Updating endowment policy details"]:::HeadingStyle
  click node3 goToHeading "Updating endowment policy details"
  node3 --> node4["Finalizing policy record update
(Finalizing policy record update)"]:::HeadingStyle
  click node4 goToHeading "Finalizing policy record update"
  node4 --> node6{"Was DB2 update successful?
(Finalizing policy record update)"}:::HeadingStyle
  click node6 goToHeading "Finalizing policy record update"
  node6 -->|"Yes"| node7["Updating VSAM policy record"]:::HeadingStyle
  click node7 goToHeading "Updating VSAM policy record"
  node6 -->|"No"| node8["Abort: Error status returned
(Finalizing policy record update)"]:::HeadingStyle
  click node8 goToHeading "Finalizing policy record update"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Programs

- LGUPDB01 (<SwmPath>[base/src/lgupdb01.cbl](base/src/lgupdb01.cbl)</SwmPath>)
- LGUPVS01 (<SwmPath>[base/src/lgupvs01.cbl](base/src/lgupvs01.cbl)</SwmPath>)
- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)

### Copybooks

- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)
- LGPOLICY (<SwmPath>[base/src/lgpolicy.cpy](base/src/lgpolicy.cpy)</SwmPath>)
- SQLCA

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  1zhhm("Updating Policy Details (LGUPOL01)") --> go2lq("Updating Policy details (LGUPDB01)"):::currentEntity
click 1zhhm openCode "base/src/lgupol01.cbl:1"
  
  
click go2lq openCode "base/src/lgupdb01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   1zhhm("Updating Policy Details (LGUPOL01)") --> go2lq("Updating Policy details (LGUPDB01)"):::currentEntity
%% click 1zhhm openCode "<SwmPath>[base/src/lgupol01.cbl](base/src/lgupol01.cbl)</SwmPath>:1"
%%   
%%   
%% click go2lq openCode "<SwmPath>[base/src/lgupdb01.cbl](base/src/lgupdb01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
