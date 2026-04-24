---
title: LGTESTP3 - House Policy Menu - Overview
---
# Overview

This document explains the flow of handling house policy menu actions. Users interact with a menu-driven interface to inquire about, add, delete, or update house policy records. The flow validates user input, coordinates with backend services to perform the requested operation, and provides immediate feedback through confirmation or error messages.

```mermaid
flowchart TD
    node1["Menu Handler and Input Processing
(Menu Handler and Input Processing)"]:::HeadingStyle
    click node1 goToHeading "Menu Handler and Input Processing"
    node1 --> node2{"Menu Option?
(Menu Handler and Input Processing)"}:::HeadingStyle
    click node2 goToHeading "Menu Handler and Input Processing"
    node2 -->|"Inquiry"| node3["Policy Inquiry Dispatch"]:::HeadingStyle
    click node3 goToHeading "Policy Inquiry Dispatch"
    node2 -->|"Add"| node4["Add Operation Validation and Error Logging"]:::HeadingStyle
    click node4 goToHeading "Add Operation Validation and Error Logging"
    node2 -->|"Delete"| node5["Validating and Dispatching Policy Deletion"]:::HeadingStyle
    click node5 goToHeading "Validating and Dispatching Policy Deletion"
    node2 -->|"Update"| node6["Validating and Dispatching Policy Update"]:::HeadingStyle
    click node6 goToHeading "Validating and Dispatching Policy Update"
    node3 --> node7["Handling Update Results and Menu Reset"]:::HeadingStyle
    node4 --> node7
    node5 --> node7
    node6 --> node7
    click node7 goToHeading "Handling Update Results and Menu Reset"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Programs

- LGTESTP3 (<SwmPath>[base/src/lgtestp3.cbl](base/src/lgtestp3.cbl)</SwmPath>)
- LGIPOL01 (<SwmPath>[base/src/lgipol01.cbl](base/src/lgipol01.cbl)</SwmPath>)
- LGIPDB01 (<SwmPath>[base/src/lgipdb01.cbl](base/src/lgipdb01.cbl)</SwmPath>)
- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)
- LGAPOL01 (<SwmPath>[base/src/lgapol01.cbl](base/src/lgapol01.cbl)</SwmPath>)
- LGAPDB01 (<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>)
- LGAPDB02 (<SwmPath>[base/src/LGAPDB02.cbl](base/src/LGAPDB02.cbl)</SwmPath>)
- LGAPDB03 (<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>)
- LGAPDB04 (<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>)
- LGDPOL01 (<SwmPath>[base/src/lgdpol01.cbl](base/src/lgdpol01.cbl)</SwmPath>)
- LGDPDB01 (<SwmPath>[base/src/lgdpdb01.cbl](base/src/lgdpdb01.cbl)</SwmPath>)
- LGDPVS01 (<SwmPath>[base/src/lgdpvs01.cbl](base/src/lgdpvs01.cbl)</SwmPath>)
- LGUPOL01 (<SwmPath>[base/src/lgupol01.cbl](base/src/lgupol01.cbl)</SwmPath>)
- LGUPDB01 (<SwmPath>[base/src/lgupdb01.cbl](base/src/lgupdb01.cbl)</SwmPath>)
- LGUPVS01 (<SwmPath>[base/src/lgupvs01.cbl](base/src/lgupvs01.cbl)</SwmPath>)
- SSP3

### Copybooks

- SQLCA
- LGPOLICY (<SwmPath>[base/src/lgpolicy.cpy](base/src/lgpolicy.cpy)</SwmPath>)
- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)
- INPUTREC2 (<SwmPath>[base/src/INPUTREC2.cpy](base/src/INPUTREC2.cpy)</SwmPath>)
- OUTPUTREC (<SwmPath>[base/src/OUTPUTREC.cpy](base/src/OUTPUTREC.cpy)</SwmPath>)
- WORKSTOR (<SwmPath>[base/src/WORKSTOR.cpy](base/src/WORKSTOR.cpy)</SwmPath>)
- LGAPACT (<SwmPath>[base/src/LGAPACT.cpy](base/src/LGAPACT.cpy)</SwmPath>)
- SSMAP

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
