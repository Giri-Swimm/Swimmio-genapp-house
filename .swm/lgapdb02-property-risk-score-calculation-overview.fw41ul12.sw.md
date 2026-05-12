---
title: LGAPDB02 - Property Risk Score Calculation - Overview
---
# Overview

This document describes the flow for calculating property risk scores. Property details such as type, location, and coverage amounts are used to determine a risk score, ensuring risk factors are always available and business rules are applied for accurate assessment.

## Dependencies

### Program

- LGAPDB02 (<SwmPath>[base/src/LGAPDB02.cbl](base/src/LGAPDB02.cbl)</SwmPath>)

### Copybook

- SQLCA

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  q2dv3("(LGAPDB01) Enhanced Policy Premium Calculation") --> uffwf("(LGAPDB02) Calculating property risk scores"):::currentEntity
click q2dv3 openCode "base/src/LGAPDB01.cbl:1"
  
  
click uffwf openCode "base/src/LGAPDB02.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   q2dv3("(LGAPDB01) Enhanced Policy Premium Calculation") --> uffwf("(LGAPDB02) Calculating property risk scores"):::currentEntity
%% click q2dv3 openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%   
%%   
%% click uffwf openCode "<SwmPath>[base/src/LGAPDB02.cbl](base/src/LGAPDB02.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
