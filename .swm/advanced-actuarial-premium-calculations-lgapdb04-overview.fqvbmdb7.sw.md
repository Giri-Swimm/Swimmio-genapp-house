---
title: Advanced Actuarial Premium Calculations (LGAPDB04) - Overview
---
# Overview

This document describes the flow for calculating advanced insurance premiums. Policyholder and property data are processed through a series of actuarial adjustments and business rules to determine the final premium and rate factor.

```mermaid
flowchart TD
    node1["Orchestrating Premium Calculation Steps"]:::HeadingStyle
    click node1 goToHeading "Orchestrating Premium Calculation Steps"
    node1 --> node2["Calculating Experience-Based Adjustment"]:::HeadingStyle
    click node2 goToHeading "Calculating Experience-Based Adjustment"
    node2 --> node3["Adjusting for Property-Specific Risk"]:::HeadingStyle
    click node3 goToHeading "Adjusting for Property-Specific Risk"
    node3 --> node4["Applying Discounts and Credits"]:::HeadingStyle
    click node4 goToHeading "Applying Discounts and Credits"
    node4 --> node5["Finalizing Premium and Rate Factor"]:::HeadingStyle
    click node5 goToHeading "Finalizing Premium and Rate Factor"
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
  143ee("Calculating Enhanced Insurance Premiums (LGAPDB01)") --> 027mc("Advanced Actuarial Premium Calculations (LGAPDB04)"):::currentEntity
click 143ee openCode "base/src/LGAPDB01.cbl:1"
  
  
click 027mc openCode "base/src/LGAPDB04.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   143ee("Calculating Enhanced Insurance Premiums (LGAPDB01)") --> 027mc("Advanced Actuarial Premium Calculations (LGAPDB04)"):::currentEntity
%% click 143ee openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%   
%%   
%% click 027mc openCode "<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
