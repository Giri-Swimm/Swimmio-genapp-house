---
title: Database Policy Data Insertion (LGAPDB09) - Overview
---
# Overview

This document describes the flow for processing new insurance policy creation requests. The system validates each request, determines the policy type, inserts the necessary records, and, for commercial policies, calculates risk and premium before finalizing the insurance record.

```mermaid
flowchart TD
    node1["Startup and Input Validation"]:::HeadingStyle
    click node1 goToHeading "Startup and Input Validation"
    node1 --> node2{"Insert Policy Record (by Type)
(Policy-Specific Record Insertion)"}:::HeadingStyle
    click node2 goToHeading "Policy-Specific Record Insertion"
    node2 -->|"Commercial"| node3["Commercial Policy Risk and Premium Calculation"]:::HeadingStyle
    click node3 goToHeading "Commercial Policy Risk and Premium Calculation"
    node3 --> node4["Finalizing and Writing Policy Data"]:::HeadingStyle
    click node4 goToHeading "Finalizing and Writing Policy Data"
    node2 -->|"Endowment, House, Motor"| node4
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
