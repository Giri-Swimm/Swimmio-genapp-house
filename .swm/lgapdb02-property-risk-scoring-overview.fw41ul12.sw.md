---
title: LGAPDB02 - Property Risk Scoring - Overview
---
# Overview

This document describes the flow for calculating property risk scores. The process gathers risk factors from the database or uses default values if necessary, then applies business rules based on property type, postcode, and coverage amounts to produce a final risk score.

## Dependencies

### Program

- LGAPDB02 (<SwmPath>[base/src/LGAPDB02.cbl](base/src/LGAPDB02.cbl)</SwmPath>)

### Copybook

- SQLCA

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  cca83("(LGAPDB01) Enhanced Policy Premium Calculation") --> kni86("(LGAPDB02) Calculating property risk scores"):::currentEntity
click cca83 openCode "base/src/LGAPDB01.cbl:1"
  
  
click kni86 openCode "base/src/LGAPDB02.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   cca83("(LGAPDB01) Enhanced Policy Premium Calculation") --> kni86("(LGAPDB02) Calculating property risk scores"):::currentEntity
%% click cca83 openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%   
%%   
%% click kni86 openCode "<SwmPath>[base/src/LGAPDB02.cbl](base/src/LGAPDB02.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
