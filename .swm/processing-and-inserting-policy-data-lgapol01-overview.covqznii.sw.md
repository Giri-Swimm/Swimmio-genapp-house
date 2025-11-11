---
title: Processing and Inserting Policy Data (LGAPOL01) - Overview
---
# Overview

This document explains the flow of processing insurance policy applications. The system validates incoming policy data, classifies policies, calculates premiums for commercial policies, and generates output records with status and rejection reasons.

```mermaid
flowchart TD
    node1["Processing and Validating Policy Records"]:::HeadingStyle --> node2{"Handling Valid Policy Records
(Handling Valid Policy Records)"}:::HeadingStyle
    click node1 goToHeading "Processing and Validating Policy Records"
    click node1 goToHeading "Validating Policy Fields and Logging Errors"
    click node2 goToHeading "Handling Valid Policy Records"
    node2 -->|"Commercial"|node3["Processing Commercial Policy Records"]:::HeadingStyle
    click node3 goToHeading "Processing Commercial Policy Records"
    node2 -->|"Non-Commercial"|node4["Rejecting Non-Commercial Policies"]:::HeadingStyle
    click node4 goToHeading "Rejecting Non-Commercial Policies"
    node2 -->|"Invalid"|node5["Writing Error Records for Invalid Policies"]:::HeadingStyle
    click node5 goToHeading "Writing Error Records for Invalid Policies"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Programs

- LGAPOL01 (<SwmPath>[base/src/lgapol01.cbl](base/src/lgapol01.cbl)</SwmPath>)
- LGAPDB01 (<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>)
- LGAPDB02
- LGAPDB03 (<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>)
- LGAPDB04 (<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>)
- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)

### Copybooks

- SQLCA
- INPUTREC2 (<SwmPath>[base/src/INPUTREC2.cpy](base/src/INPUTREC2.cpy)</SwmPath>)
- OUTPUTREC (<SwmPath>[base/src/OUTPUTREC.cpy](base/src/OUTPUTREC.cpy)</SwmPath>)
- WORKSTOR (<SwmPath>[base/src/WORKSTOR.cpy](base/src/WORKSTOR.cpy)</SwmPath>)
- LGAPACT (<SwmPath>[base/src/LGAPACT.cpy](base/src/LGAPACT.cpy)</SwmPath>)
- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)

# Where is this program used?

This program is used multiple times in the codebase as represented in the following diagram:

```mermaid
graph TD
  zla33("Managing Commercial Policy Operations (LGTESTP4)") --> gufl2("Processing and Inserting Policy Data (LGAPOL01)"):::currentEntity
click zla33 openCode "base/src/lgtestp4.cbl:1"
0dr4p("House Policy Menu (LGTESTP3)") --> gufl2("Processing and Inserting Policy Data (LGAPOL01)"):::currentEntity
click 0dr4p openCode "base/src/lgtestp3.cbl:1"
p6b6i("Motor Policy Menu (LGTESTP1)") --> gufl2("Processing and Inserting Policy Data (LGAPOL01)"):::currentEntity
click p6b6i openCode "base/src/lgtestp1.cbl:1"
kbnhx("Endowment Policy Menu (LGTESTP2)") --> gufl2("Processing and Inserting Policy Data (LGAPOL01)"):::currentEntity
click kbnhx openCode "base/src/lgtestp2.cbl:1"
  
  
click gufl2 openCode "base/src/lgapol01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   zla33("Managing Commercial Policy Operations (LGTESTP4)") --> gufl2("Processing and Inserting Policy Data (LGAPOL01)"):::currentEntity
%% click zla33 openCode "<SwmPath>[base/src/lgtestp4.cbl](base/src/lgtestp4.cbl)</SwmPath>:1"
%% 0dr4p("House Policy Menu (LGTESTP3)") --> gufl2("Processing and Inserting Policy Data (LGAPOL01)"):::currentEntity
%% click 0dr4p openCode "<SwmPath>[base/src/lgtestp3.cbl](base/src/lgtestp3.cbl)</SwmPath>:1"
%% p6b6i("Motor Policy Menu (LGTESTP1)") --> gufl2("Processing and Inserting Policy Data (LGAPOL01)"):::currentEntity
%% click p6b6i openCode "<SwmPath>[base/src/lgtestp1.cbl](base/src/lgtestp1.cbl)</SwmPath>:1"
%% kbnhx("Endowment Policy Menu (LGTESTP2)") --> gufl2("Processing and Inserting Policy Data (LGAPOL01)"):::currentEntity
%% click kbnhx openCode "<SwmPath>[base/src/lgtestp2.cbl](base/src/lgtestp2.cbl)</SwmPath>:1"
%%   
%%   
%% click gufl2 openCode "<SwmPath>[base/src/lgapol01.cbl](base/src/lgapol01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
