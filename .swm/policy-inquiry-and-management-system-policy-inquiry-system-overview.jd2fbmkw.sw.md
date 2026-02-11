---
title: Policy inquiry and management system (POLICY INQUIRY SYSTEM) - Overview
---
# Overview

This document describes a menu-driven policy inquiry and maintenance system. Users select actions such as searching for policies, updating addresses, adjusting premiums, or managing riders. The system processes each request, updates policy records, and provides feedback or confirmation.

```mermaid
flowchart TD
    node1["Main Program Loop"]:::HeadingStyle
    click node1 goToHeading "Main Program Loop"
    node1 --> node2["Menu Handling and User Selection"]:::HeadingStyle
    click node2 goToHeading "Menu Handling and User Selection"
    node2 -->|"Address Change"| node3["Address Update Processing"]:::HeadingStyle
    click node3 goToHeading "Address Update Processing"
    node2 -->|"Premium Increase"| node4["Premium Increase Processing"]:::HeadingStyle
    click node4 goToHeading "Premium Increase Processing"
    node2 -->|"Rider Deletion"| node5["Rider Deletion Processing"]:::HeadingStyle
    click node5 goToHeading "Rider Deletion Processing"
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
