---
title: LGAPDB02 - Property Risk Score Calculation - Overview
---
# Overview

This document describes the flow for calculating property risk scores. The process combines risk factors, property details, coverage levels, location, and customer history to produce a score used in insurance risk assessment.

```mermaid
flowchart TD
  node1["Orchestrating the Risk Calculation"]:::HeadingStyle
  click node1 goToHeading "Orchestrating the Risk Calculation"
  node1 --> node2["Retrieving Risk Factors from Storage"]:::HeadingStyle
  click node2 goToHeading "Retrieving Risk Factors from Storage"
  node2 --> node3["Building the Risk Score"]:::HeadingStyle
  click node3 goToHeading "Building the Risk Score"
  node3 --> node4["Evaluating Coverage Levels"]:::HeadingStyle
  click node4 goToHeading "Evaluating Coverage Levels"
  node4 --> node5["Scoring Location and Customer History"]:::HeadingStyle
  click node5 goToHeading "Scoring Location and Customer History"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Program

- LGAPDB02 (<SwmPath>[base/src/LGAPDB02.cbl](base/src/LGAPDB02.cbl)</SwmPath>)

### Copybook

- SQLCA

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  2e4fd("(LGAPDB01) Enhanced Policy Premium Calculation") --> n1g2o("(LGAPDB02) Calculating property risk scores"):::currentEntity
click 2e4fd openCode "base/src/LGAPDB01.cbl:1"
  
  
click n1g2o openCode "base/src/LGAPDB02.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   2e4fd("(LGAPDB01) Enhanced Policy Premium Calculation") --> n1g2o("(LGAPDB02) Calculating property risk scores"):::currentEntity
%% click 2e4fd openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%   
%%   
%% click n1g2o openCode "<SwmPath>[base/src/LGAPDB02.cbl](base/src/LGAPDB02.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
