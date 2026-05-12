---
title: LGAPDB01 - Insurance Policy Batch Processing - Overview
---
# Overview

This document explains the flow for processing insurance policy applications in batch. The process covers loading configuration values, validating and routing records, calculating risk scores and premiums for commercial policies, and updating summary statistics for reporting.

```mermaid
flowchart TD
    node1["Configuration Loading"]:::HeadingStyle --> node2["Reading Config Values"]:::HeadingStyle
    click node1 goToHeading "Configuration Loading"
    click node2 goToHeading "Reading Config Values"
    node2 --> node3["Record Processing Loop"]:::HeadingStyle
    click node3 goToHeading "Record Processing Loop"
    node3 --> node4{"Valid Record?"}
    node4 -->|"Yes"| node5{"Commercial Policy?"}
    node4 -->|"No"| node6["Updating Policy Processing Statistics"]:::HeadingStyle
    click node6 goToHeading "Updating Policy Processing Statistics"
    node5 -->|"Yes"| node7["Commercial Policy Processing"]:::HeadingStyle
    node5 -->|"No"| node6
    click node7 goToHeading "Commercial Policy Processing"
    node7 --> node6
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
  vd2ol("(LGAPJOB) Insurance policy premium calculation batch job") --> 7jren("(LGAPDB01) Enhanced Policy Premium Calculation"):::currentEntity
click vd2ol openCode "base/cntl/lgapjob.jcl:1"
9xhb7("(LGAPOL01) Communication Area Validation and Data Insertion") --> 7jren("(LGAPDB01) Enhanced Policy Premium Calculation"):::currentEntity
click 9xhb7 openCode "base/src/lgapol01.cbl:1"
  
  
click 7jren openCode "base/src/LGAPDB01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   vd2ol("(LGAPJOB) Insurance policy premium calculation batch job") --> 7jren("(LGAPDB01) Enhanced Policy Premium Calculation"):::currentEntity
%% click vd2ol openCode "<SwmPath>[base/cntl/lgapjob.jcl](base/cntl/lgapjob.jcl)</SwmPath>:1"
%% 9xhb7("(LGAPOL01) Communication Area Validation and Data Insertion") --> 7jren("(LGAPDB01) Enhanced Policy Premium Calculation"):::currentEntity
%% click 9xhb7 openCode "<SwmPath>[base/src/lgapol01.cbl](base/src/lgapol01.cbl)</SwmPath>:1"
%%   
%%   
%% click 7jren openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
