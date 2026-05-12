---
title: LGAPDB03 - Insurance Application Risk and Premium Processing - Overview
---
# Overview

This document describes the flow for processing insurance applications, including risk assessment and premium calculation. The system retrieves current risk multipliers, determines a risk verdict, and calculates premiums for all covered perils, applying a discount for full coverage. The output includes the risk verdict and calculated premiums.

## Dependencies

### Program

- LGAPDB03 (<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>)

### Copybook

- SQLCA

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  mlq5j("(LGAPDB01) Enhanced Policy Premium Calculation") --> yrfym("(LGAPDB03) Calculating insurance premiums and risk verdicts"):::currentEntity
click mlq5j openCode "base/src/LGAPDB01.cbl:1"
  
  
click yrfym openCode "base/src/LGAPDB03.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   mlq5j("(LGAPDB01) Enhanced Policy Premium Calculation") --> yrfym("(LGAPDB03) Calculating insurance premiums and risk verdicts"):::currentEntity
%% click mlq5j openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%   
%%   
%% click yrfym openCode "<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
