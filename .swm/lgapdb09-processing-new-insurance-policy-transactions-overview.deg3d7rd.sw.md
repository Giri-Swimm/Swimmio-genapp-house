---
title: LGAPDB09 - Processing New Insurance Policy Transactions - Overview
---
# Overview

This document explains the flow for processing new insurance policy transactions. The system validates transaction data, determines the policy type, and routes each request to the appropriate handler. For commercial policies, it performs risk and premium calculations before inserting records into the database and writing output records to file.

```mermaid
flowchart TD
    node1["Starting the transaction and preparing workspace"]:::HeadingStyle --> node2{"Policy type? (Branching to
policy-specific inserts)
(Branching to policy-specific inserts)"}:::HeadingStyle
    click node1 goToHeading "Starting the transaction and preparing workspace"
    click node1 goToHeading "Preparing policy and customer fields"
    click node2 goToHeading "Branching to policy-specific inserts"
    node2 -->|"Endowment"| node3["Inserting endowment policy details"]:::HeadingStyle
    click node3 goToHeading "Inserting endowment policy details"
    node2 -->|"House"| node4["Inserting house policy details"]:::HeadingStyle
    click node4 goToHeading "Inserting house policy details"
    node2 -->|"Motor"| node5["Inserting motor policy details"]:::HeadingStyle
    click node5 goToHeading "Inserting motor policy details"
    node2 -->|"Commercial"| node6["Preparing commercial policy risk calculation"]:::HeadingStyle
    click node6 goToHeading "Preparing commercial policy risk calculation"
    node3 --> node7["Linking to file write and returning to caller"]:::HeadingStyle
    click node7 goToHeading "Linking to file write and returning to caller"
    node4 --> node7
    node5 --> node7
    node6 --> node7
    node7 --> node8["Writing the policy or customer record to file"]:::HeadingStyle
    click node8 goToHeading "Writing the policy or customer record to file"
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
