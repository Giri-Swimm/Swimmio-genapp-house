---
title: Inquire Policy (LGIPVS01) - Overview
---
# Overview

This document explains the flow for handling policy inquiries. The system receives a request, retrieves and validates policy information, and returns either the policy details or a standardized error message to the appropriate recipient.

```mermaid
flowchart TD
    node1["Processing the incoming request
(Processing the incoming request)"]:::HeadingStyle --> node2{"Is request from terminal or program?"}
    click node1 goToHeading "Processing the incoming request"
    node2 --> node3{"Is policy valid?
(Processing the incoming request)"}:::HeadingStyle
    click node3 goToHeading "Processing the incoming request"
    node3 -->|"Yes"| node4["Deliver policy details to terminal or program
(Processing the incoming request)"]:::HeadingStyle
    node3 -->|"No"| node4
    click node4 goToHeading "Processing the incoming request"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Program

- LGIPVS01 (<SwmPath>[base/src/lgipvs01.cbl](base/src/lgipvs01.cbl)</SwmPath>)

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
