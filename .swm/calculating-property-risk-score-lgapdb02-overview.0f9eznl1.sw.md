---
title: Calculating Property Risk Score (LGAPDB02) - Overview
---
# Overview

This document describes the flow for calculating a property risk score. The process uses the latest fire and crime risk factors, property characteristics, and insurance coverage details to determine the risk score for insurance assessment.

```mermaid
flowchart TD
    node1["Starting the risk calculation process"]:::HeadingStyle --> node2{"Fetching risk factors from the database
Are database values available?
(Fetching risk factors from the database)"}:::HeadingStyle
    click node1 goToHeading "Starting the risk calculation process"
    click node2 goToHeading "Fetching risk factors from the database"
    node2 -->|"Yes/No (use defaults)"| node3["Adjusting risk score based on property and location"]:::HeadingStyle
    click node3 goToHeading "Adjusting risk score based on property and location"
    node3 --> node4{"Evaluating coverage impact on risk
Is coverage above threshold?
(Evaluating coverage impact on risk)"}:::HeadingStyle
    click node4 goToHeading "Evaluating coverage impact on risk"
    node4 -->|"Yes/No"| node5["Risk score finalized
(Evaluating coverage impact on risk)"]:::HeadingStyle
    click node5 goToHeading "Evaluating coverage impact on risk"
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
  zvorm("Calculating Enhanced Insurance Premiums (LGAPDB01)") --> pj886("Calculating Property Risk Score (LGAPDB02)"):::currentEntity
click zvorm openCode "base/src/LGAPDB01.cbl:1"
  
  
click pj886 openCode "base/src/LGAPDB02.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   zvorm("Calculating Enhanced Insurance Premiums (LGAPDB01)") --> pj886("Calculating Property Risk Score (LGAPDB02)"):::currentEntity
%% click zvorm openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%   
%%   
%% click pj886 openCode "<SwmPath>[base/src/LGAPDB02.cbl](base/src/LGAPDB02.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
