---
title: LGAPDB09 - Processing Insurance Policy Requests - Overview
---
# Overview

This document describes the flow for handling new insurance policy requests. The process validates input, determines policy type, inserts records, and, for commercial policies, calculates risk and premiums before saving the policy data.

```mermaid
flowchart TD
    node1["Startup and Input Validation"]:::HeadingStyle --> node2["Request Preparation and Policy Type Selection"]:::HeadingStyle
    click node1 goToHeading "Startup and Input Validation"
    click node2 goToHeading "Request Preparation and Policy Type Selection"
    node2 --> node3{"Policy-Specific Record Handling
(Policy-Specific Record Handling)"}:::HeadingStyle
    click node3 goToHeading "Policy-Specific Record Handling"
    node3 -->|"Endowment"|node4["Endowment Policy Insert"]:::HeadingStyle
    click node4 goToHeading "Endowment Policy Insert"
    node3 -->|"House"|node5["House Record Insert"]:::HeadingStyle
    click node5 goToHeading "House Record Insert"
    node3 -->|"Motor"|node6["Motor Policy Insert"]:::HeadingStyle
    click node6 goToHeading "Motor Policy Insert"
    node3 -->|"Commercial"|node7["Commercial Policy Risk and Premium Calculation"]:::HeadingStyle
    click node7 goToHeading "Commercial Policy Risk and Premium Calculation"
    node7 --> node8["Applying Matrix Override and Rejection Logic"]:::HeadingStyle
    click node8 goToHeading "Applying Matrix Override and Rejection Logic"
    node8 --> node9["Writing Commercial Policy to Database"]:::HeadingStyle
    click node9 goToHeading "Writing Commercial Policy to Database"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Programs

- LGAPDB09 (<SwmPath>[base/src/lgapdb09.cbl](base/src/lgapdb09.cbl)</SwmPath>)
- LGAPVS01 (<SwmPath>[base/src/lgapvs01.cbl](base/src/lgapvs01.cbl)</SwmPath>)
- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)
- LGCOMCAL (<SwmPath>[base/src/lgcomcal.cbl](base/src/lgcomcal.cbl)</SwmPath>)

### Copybooks

- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)
- LGCOMDAT (<SwmPath>[base/src/lgcomdat.cpy](base/src/lgcomdat.cpy)</SwmPath>)
- LGPOLICY (<SwmPath>[base/src/lgpolicy.cpy](base/src/lgpolicy.cpy)</SwmPath>)
- SQLCA

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
