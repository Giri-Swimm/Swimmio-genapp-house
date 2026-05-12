---
title: LGAPDB04 - Advanced Actuarial Premium Calculation - Overview
---
# Overview

This document outlines the flow for calculating insurance premiums and rate factors. The process applies business rules to customer and policy data, including experience modifiers, schedule adjustments, peril-specific premium calculations, catastrophe loadings, discounts, and final rate capping.

```mermaid
flowchart TD
    node1["Adjusting for business experience"]:::HeadingStyle
    click node1 goToHeading "Adjusting for business experience"
    node1 --> node2["Applying schedule adjustments"]:::HeadingStyle
    click node2 goToHeading "Applying schedule adjustments"
    node2 --> node3["Calculating base premium for selected perils"]:::HeadingStyle
    click node3 goToHeading "Calculating base premium for selected perils"
    node3 --> node4["Adding catastrophe loadings"]:::HeadingStyle
    click node4 goToHeading "Adding catastrophe loadings"
    node4 --> node5["Applying discounts and credits"]:::HeadingStyle
    click node5 goToHeading "Applying discounts and credits"
    node5 --> node6["Finalizing premium and rate factor"]:::HeadingStyle
    click node6 goToHeading "Finalizing premium and rate factor"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Program

- LGAPDB04 (<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>)

### Copybook

- SQLCA

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  ywn53("(LGAPDB01) Enhanced Policy Premium Calculation") --> 4fh5s("(LGAPDB04) Advanced Actuarial Premium Calculation"):::currentEntity
click ywn53 openCode "base/src/LGAPDB01.cbl:1"
  
  
click 4fh5s openCode "base/src/LGAPDB04.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   ywn53("(LGAPDB01) Enhanced Policy Premium Calculation") --> 4fh5s("(LGAPDB04) Advanced Actuarial Premium Calculation"):::currentEntity
%% click ywn53 openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%   
%%   
%% click 4fh5s openCode "<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
