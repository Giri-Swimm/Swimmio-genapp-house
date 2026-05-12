---
title: LGAPDB01 - Batch Insurance Policy Processing - Overview
---
# Overview

This document explains the flow for batch processing insurance policy applications. Each record is validated, and commercial policies receive premium calculations and underwriting decisions. Non-commercial or invalid records are output with appropriate status and reasons.

```mermaid
flowchart TD
    node1["Input Record Processing Loop"]:::HeadingStyle --> node2["Input Validation and Error Logging"]:::HeadingStyle
    click node1 goToHeading "Input Record Processing Loop"
    click node2 goToHeading "Input Validation and Error Logging"
    node2 -->|"Invalid"| node6["Error Record Output"]:::HeadingStyle
    click node6 goToHeading "Error Record Output"
    node2 -->|"Valid"| node3{"Policy Type?"}
    node3 -->|"Commercial"| node4["Commercial Policy Underwriting"]:::HeadingStyle
    click node4 goToHeading "Commercial Policy Underwriting"
    node4 --> node7["Output Record"]
    node3 -->|"Non-Commercial"| node5["Non-Commercial Policy Handling"]:::HeadingStyle
    click node5 goToHeading "Non-Commercial Policy Handling"
    node5 --> node7
    node6 --> node7
    node7["Output Record"]

classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Programs

- LGAPDB01 (<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>)
- LGAPDB02 (<SwmPath>[base/src/LGAPDB02.cbl](base/src/LGAPDB02.cbl)</SwmPath>)
- LGAPDB03 (<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>)
- LGAPDB04 (<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>)

### Copybooks

- SQLCA
- INPUTREC2 (<SwmPath>[base/src/INPUTREC2.cpy](base/src/INPUTREC2.cpy)</SwmPath>)
- OUTPUTREC (<SwmPath>[base/src/OUTPUTREC.cpy](base/src/OUTPUTREC.cpy)</SwmPath>)
- WORKSTOR (<SwmPath>[base/src/WORKSTOR.cpy](base/src/WORKSTOR.cpy)</SwmPath>)
- LGAPACT (<SwmPath>[base/src/LGAPACT.cpy](base/src/LGAPACT.cpy)</SwmPath>)

# Where is this program used?

This program is used multiple times in the codebase as represented in the following diagram:

```mermaid
graph TD
  a8lfu("(LGAPJOB) Insurance policy premium calculation batch job") --> u8gdn("(LGAPDB01) Enhanced Policy Premium Calculation"):::currentEntity
click a8lfu openCode "base/cntl/lgapjob.jcl:1"
odtmu("(LGAPOL01) Communication Area Validation and Data Insertion") --> u8gdn("(LGAPDB01) Enhanced Policy Premium Calculation"):::currentEntity
click odtmu openCode "base/src/lgapol01.cbl:1"
  
  
click u8gdn openCode "base/src/LGAPDB01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   a8lfu("(LGAPJOB) Insurance policy premium calculation batch job") --> u8gdn("(LGAPDB01) Enhanced Policy Premium Calculation"):::currentEntity
%% click a8lfu openCode "<SwmPath>[base/cntl/lgapjob.jcl](base/cntl/lgapjob.jcl)</SwmPath>:1"
%% odtmu("(LGAPOL01) Communication Area Validation and Data Insertion") --> u8gdn("(LGAPDB01) Enhanced Policy Premium Calculation"):::currentEntity
%% click odtmu openCode "<SwmPath>[base/src/lgapol01.cbl](base/src/lgapol01.cbl)</SwmPath>:1"
%%   
%%   
%% click u8gdn openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
