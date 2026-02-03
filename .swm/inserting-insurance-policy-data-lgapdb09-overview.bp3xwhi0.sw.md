---
title: Inserting Insurance Policy Data (LGAPDB09) - Overview
---
# Overview

This document explains the flow for processing insurance policy requests. The system validates input, inserts policy records, performs risk assessment for commercial policies, and stores the results for integration.

```mermaid
flowchart TD
    node1["Initializing Transaction and Input Structures"]:::HeadingStyle --> node2["Mapping and Validating Policy Request"]:::HeadingStyle
    click node1 goToHeading "Initializing Transaction and Input Structures"
    click node2 goToHeading "Mapping and Validating Policy Request"
    node2 --> node3["Inserting Main Policy Record"]:::HeadingStyle
    click node3 goToHeading "Inserting Main Policy Record"
    node3 --> node4{"Policy Type?
(Dispatching to Policy-Specific Inserts)"}:::HeadingStyle
    click node4 goToHeading "Dispatching to Policy-Specific Inserts"
    node4 -->|"Endowment/House/Motor"| node5["Insert Policy-Specific Data
(Dispatching to Policy-Specific Inserts)"]:::HeadingStyle
    click node5 goToHeading "Dispatching to Policy-Specific Inserts"
    node4 -->|"Commercial"| node6["Handling Commercial Policy Risk Assessment"]:::HeadingStyle
    click node6 goToHeading "Handling Commercial Policy Risk Assessment"
    node6 --> node7["Saving Commercial Insurance Record"]:::HeadingStyle
    click node7 goToHeading "Saving Commercial Insurance Record"
    node5 --> node8["Passing Results to Integration"]:::HeadingStyle
    node7 --> node8
    click node8 goToHeading "Passing Results to Integration"
    click node8 goToHeading "Writing Policy Data to Storage"
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
