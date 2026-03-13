---
title: RISKPROG - Policy Risk Evaluation (LGARSK01) - Overview
---
# Overview

This document describes how policy records are processed to evaluate insurance risk. For each policy, a risk score is calculated and a risk category is assigned based on property and claims data. The results are recorded for further use.

```mermaid
flowchart TD
    node1["Reading, validating, and handling policy records"]:::HeadingStyle
    click node1 goToHeading "Reading, validating, and handling policy records"
    node1 --> node2{"Record valid?"}
    node2 -->|"Yes"| node3["Mapping property type and claim count to risk factors"]:::HeadingStyle
    click node3 goToHeading "Mapping property type and claim count to risk factors"
    node3 --> node4{"Risk score value"}
    node4 -->|"#lt; 3.00: LOW
< 6.00: MEDIUM
>= 6.00:
HIGH"| node5["Writing risk results after calculation"]:::HeadingStyle
    click node5 goToHeading "Writing risk results after calculation"
    node2 -->|"No (skip/log error)"| node5
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Program

- RISKPROG (<SwmPath>[base/src/lgarsk01.cbl](base/src/lgarsk01.cbl)</SwmPath>)

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
