---
title: Matrix Processing and Risk Evaluation (LGSCMTRX) - Overview
---
# Overview

This document describes the flow for dispatching and processing matrix operations. Requests are routed to the appropriate operation—property, postal, risk, or crypto—based on the function code, and each operation returns a calculated result based on the input data.

```mermaid
flowchart TD
    node1["Dispatching Matrix Operations Based on Request Type
(Dispatching Matrix Operations Based on Request Type)"]:::HeadingStyle
    click node1 goToHeading "Dispatching Matrix Operations Based on Request Type"
    node1 --> node2{"Function code?"}
    node2 -->|"P"| node3["Matching and Scoring Property Types"]:::HeadingStyle
    click node3 goToHeading "Matching and Scoring Property Types"
    node2 -->|"Z"| node4["Postal Code Prefix Lookup and Scoring"]:::HeadingStyle
    click node4 goToHeading "Postal Code Prefix Lookup and Scoring"
    node2 -->|"R"| node5["Risk Matrix Operation
(Dispatching Matrix Operations Based on Request Type)"]:::HeadingStyle
    click node5 goToHeading "Dispatching Matrix Operations Based on Request Type"
    node2 -->|"E"| node6["Cryptographic Matrix Calculation for Risk Score"]:::HeadingStyle
    click node6 goToHeading "Cryptographic Matrix Calculation for Risk Score"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Program

- LGSCMTRX (<SwmPath>[base/src/lgscmtrx.cbl](base/src/lgscmtrx.cbl)</SwmPath>)

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
