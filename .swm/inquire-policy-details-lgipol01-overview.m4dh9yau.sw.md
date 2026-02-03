---
title: Inquire Policy Details (LGIPOL01) - Overview
---
# Overview

This document explains the flow for inquiring about insurance policies. Users submit a request with customer and policy identifiers and the desired policy type. The system validates the request, logs errors with context, and returns detailed policy information or an error code.

```mermaid
flowchart TD
    node1["Startup and Input Validation
(Startup and Input Validation)"]:::HeadingStyle --> node2{"Is input valid?
(Startup and Input Validation)"}:::HeadingStyle
    node2 -->|"Yes"| node3["Preparing for Database Inquiry"]:::HeadingStyle
    node3 --> node4["Policy Type Routing and Data Retrieval"]:::HeadingStyle
    node2 -->|"No"| node5["Return error code
(Startup and Input Validation)"]:::HeadingStyle
    click node1 goToHeading "Startup and Input Validation"
    click node2 goToHeading "Startup and Input Validation"
    click node3 goToHeading "Preparing for Database Inquiry"
    click node4 goToHeading "Policy Type Routing and Data Retrieval"
    click node5 goToHeading "Startup and Input Validation"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Programs

- LGIPOL01 (<SwmPath>[base/src/lgipol01.cbl](base/src/lgipol01.cbl)</SwmPath>)
- LGIPDB01 (<SwmPath>[base/src/lgipdb01.cbl](base/src/lgipdb01.cbl)</SwmPath>)
- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)

### Copybooks

- SQLCA
- LGPOLICY (<SwmPath>[base/src/lgpolicy.cpy](base/src/lgpolicy.cpy)</SwmPath>)
- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)

# Where is this program used?

This program is used multiple times in the codebase as represented in the following diagram:

```mermaid
graph TD
  3cuk4("Motor Policy Menu (LGTESTP1)") --> sa36t("Inquire Policy Details (LGIPOL01)"):::currentEntity
click 3cuk4 openCode "base/src/lgtestp1.cbl:1"
tdhqs("House Policy Menu (LGTESTP3)") --> sa36t("Inquire Policy Details (LGIPOL01)"):::currentEntity
click tdhqs openCode "base/src/lgtestp3.cbl:1"
bksm7("Endowment Policy Menu (LGTESTP2)") --> sa36t("Inquire Policy Details (LGIPOL01)"):::currentEntity
click bksm7 openCode "base/src/lgtestp2.cbl:1"
rkpmf("Managing Commercial Policy Data (LGTESTP4)") --> sa36t("Inquire Policy Details (LGIPOL01)"):::currentEntity
click rkpmf openCode "base/src/lgtestp4.cbl:1"
  
  
click sa36t openCode "base/src/lgipol01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   3cuk4("Motor Policy Menu (LGTESTP1)") --> sa36t("Inquire Policy Details (LGIPOL01)"):::currentEntity
%% click 3cuk4 openCode "<SwmPath>[base/src/lgtestp1.cbl](base/src/lgtestp1.cbl)</SwmPath>:1"
%% tdhqs("House Policy Menu (LGTESTP3)") --> sa36t("Inquire Policy Details (LGIPOL01)"):::currentEntity
%% click tdhqs openCode "<SwmPath>[base/src/lgtestp3.cbl](base/src/lgtestp3.cbl)</SwmPath>:1"
%% bksm7("Endowment Policy Menu (LGTESTP2)") --> sa36t("Inquire Policy Details (LGIPOL01)"):::currentEntity
%% click bksm7 openCode "<SwmPath>[base/src/lgtestp2.cbl](base/src/lgtestp2.cbl)</SwmPath>:1"
%% rkpmf("Managing Commercial Policy Data (LGTESTP4)") --> sa36t("Inquire Policy Details (LGIPOL01)"):::currentEntity
%% click rkpmf openCode "<SwmPath>[base/src/lgtestp4.cbl](base/src/lgtestp4.cbl)</SwmPath>:1"
%%   
%%   
%% click sa36t openCode "<SwmPath>[base/src/lgipol01.cbl](base/src/lgipol01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
