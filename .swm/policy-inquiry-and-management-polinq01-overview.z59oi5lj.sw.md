---
title: Policy inquiry and management (POLINQ01) - Overview
---
# Overview

This document describes the interactive flow for managing insurance policies. Users select operations from a menu, such as searching for policies or updating policy details. The system validates each request, updates records, and provides confirmation or error messages.

```mermaid
flowchart TD
    node1["Entry and orchestration"]:::HeadingStyle --> node2["Menu and user selection
(Menu and user selection)"]:::HeadingStyle
    click node1 goToHeading "Entry and orchestration"
    click node2 goToHeading "Menu and user selection"
    node2 --> node3{"Is selection valid and policy exists?
(Menu and user selection)"}:::HeadingStyle
    click node3 goToHeading "Menu and user selection"
    node3 -->|"Yes"| node4["Adding a benefit rider"]:::HeadingStyle
    click node4 goToHeading "Adding a benefit rider"
    node3 -->|"Yes"| node5["Address change and update"]:::HeadingStyle
    click node5 goToHeading "Address change and update"
    node3 -->|"Yes"| node6["Premium increase processing"]:::HeadingStyle
    click node6 goToHeading "Premium increase processing"
    node3 -->|"Yes"| node7["Premium decrease processing"]:::HeadingStyle
    click node7 goToHeading "Premium decrease processing"
    node3 -->|"Yes"| node8["Rider deletion"]:::HeadingStyle
    click node8 goToHeading "Rider deletion"
    node3 -->|"Yes"| node9["Rider update"]:::HeadingStyle
    click node9 goToHeading "Rider update"
    node3 -->|"No"| node2
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Program

- SAMPLES (<SwmPath>[base/src/sample.cbl](base/src/sample.cbl)</SwmPath>)

### Copybooks

- PMASTER
- PINSURED
- PBENEFIT
- AGTMSTR
- POLTRAN1
- POLTRAN2
- POLNTFY

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
