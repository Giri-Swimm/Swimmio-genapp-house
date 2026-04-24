---
title: LGPCALC1 - Property Risk Calculation and Status Assignment - Overview
---
# Overview

This document describes how property cases are processed to determine risk scores and assign risk statuses. Business rules are applied to adjust risk based on property type and location, and each case is categorized for further action.

```mermaid
flowchart TD
    node1["Dispatching the calculation steps"]:::HeadingStyle -->|"Risk calculation"| node2["Adjusting risk based on property and postcode"]:::HeadingStyle
    node1 -->|"Status determination"| node3["Categorizing risk status"]:::HeadingStyle
    node1 -->|"All"| node2
    node2 --> node3

    click node1 goToHeading "Dispatching the calculation steps"
    click node2 goToHeading "Adjusting risk based on property and postcode"
    click node3 goToHeading "Categorizing risk status"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Program

- LGPCALC1 (<SwmPath>[base/src/lgpcalc1.cbl](base/src/lgpcalc1.cbl)</SwmPath>)

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
