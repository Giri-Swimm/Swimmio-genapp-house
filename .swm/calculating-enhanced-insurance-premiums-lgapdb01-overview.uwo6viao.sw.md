---
title: Calculating Enhanced Insurance Premiums (LGAPDB01) - Overview
---
# Overview

This document describes the flow for processing insurance policy applications. Policy records are validated, commercial policies are evaluated for risk and premium, and underwriting decisions are made. The flow outputs processed records and business statistics.

```mermaid
flowchart TD
    node1["Processing input records"]:::HeadingStyle --> node2{"Is record valid?
(Validating input and logging errors)"}:::HeadingStyle
    node2 -->|"Yes"| node3{"Is policy commercial?
(Handling valid records)"}:::HeadingStyle
    node2 -->|"No"| node5["(Omitted: Error handling)"]
    node3 -->|"Yes"| node4["Calculating commercial policy risk and premiums"]:::HeadingStyle
    node3 -->|"No"| node6["Reject non-commercial policy
(Handling valid records)"]:::HeadingStyle
    node4 --> node7["Making the underwriting decision"]:::HeadingStyle
    node6 --> node7

    click node1 goToHeading "Processing input records"
    click node2 goToHeading "Validating input and logging errors"
    click node3 goToHeading "Handling valid records"
    click node4 goToHeading "Calculating commercial policy risk and premiums"
    click node6 goToHeading "Handling valid records"
    click node7 goToHeading "Making the underwriting decision"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Programs

- LGAPDB01 (<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>)
- LGAPDB02
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
  iqntk("Insurance Policy Premium Calculation Job (LGAPJOB)") --> tww0l("Calculating Enhanced Insurance Premiums (LGAPDB01)"):::currentEntity
click iqntk openCode "base/cntl/lgapjob.jcl:1"
yspzj("Processing and Inserting Policy Data (LGAPOL01)") --> tww0l("Calculating Enhanced Insurance Premiums (LGAPDB01)"):::currentEntity
click yspzj openCode "base/src/lgapol01.cbl:1"
  
  
click tww0l openCode "base/src/LGAPDB01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   iqntk("Insurance Policy Premium Calculation Job (LGAPJOB)") --> tww0l("Calculating Enhanced Insurance Premiums (LGAPDB01)"):::currentEntity
%% click iqntk openCode "<SwmPath>[base/cntl/lgapjob.jcl](base/cntl/lgapjob.jcl)</SwmPath>:1"
%% yspzj("Processing and Inserting Policy Data (LGAPOL01)") --> tww0l("Calculating Enhanced Insurance Premiums (LGAPDB01)"):::currentEntity
%% click yspzj openCode "<SwmPath>[base/src/lgapol01.cbl](base/src/lgapol01.cbl)</SwmPath>:1"
%%   
%%   
%% click tww0l openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
