---
title: Inquiring Individual Insurance Policy Details (LGIPDB01) - Overview
---
# Overview

This document explains the flow for inquiring individual insurance policy details. The process receives a request with customer and policy identifiers, determines the requested policy type, and returns the corresponding policy data or an error code.

```mermaid
flowchart TD
    node1["Preparing request and routing policy inquiry"]:::HeadingStyle --> node2{"Which policy type is requested?
(Routing to other policy types)"}:::HeadingStyle
    click node1 goToHeading "Preparing request and routing policy inquiry"
    click node2 goToHeading "Routing to other policy types"
    node2 -->|"Endowment"|node3["Fetching and packaging endowment policy data
(Fetching and packaging endowment policy data)"]:::HeadingStyle
    click node3 goToHeading "Fetching and packaging endowment policy data"
    node2 -->|"House"|node4["Fetching and packaging house policy data"]:::HeadingStyle
    click node4 goToHeading "Fetching and packaging house policy data"
    node2 -->|"Motor"|node5["Fetching and packaging motor policy data"]:::HeadingStyle
    click node5 goToHeading "Fetching and packaging motor policy data"
    node2 -->|"Other/Unknown or error"|node6["Return error code
(Routing to other policy types)"]:::HeadingStyle
    click node6 goToHeading "Routing to other policy types"
    node3 --> node7{"Was data retrieval successful and response area sufficient?
(Fetching and packaging endowment policy data)"}:::HeadingStyle
    node4 --> node7
    node5 --> node7
    node7 -->|"Yes"|node8["Return policy data
(Fetching and packaging endowment policy data)"]:::HeadingStyle
    node7 -->|"No"|node6
    click node7 goToHeading "Fetching and packaging endowment policy data"
    click node8 goToHeading "Fetching and packaging endowment policy data"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Programs

- LGIPDB01 (<SwmPath>[base/src/lgipdb01.cbl](base/src/lgipdb01.cbl)</SwmPath>)
- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)

### Copybooks

- SQLCA
- LGPOLICY (<SwmPath>[base/src/lgpolicy.cpy](base/src/lgpolicy.cpy)</SwmPath>)
- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  u60eo("Inquiring Policy Details (LGIPOL01)") --> rcchx("Inquiring Individual Insurance Policy Details (LGIPDB01)"):::currentEntity
click u60eo openCode "base/src/lgipol01.cbl:1"
  
  
click rcchx openCode "base/src/lgipdb01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   u60eo("Inquiring Policy Details (LGIPOL01)") --> rcchx("Inquiring Individual Insurance Policy Details (LGIPDB01)"):::currentEntity
%% click u60eo openCode "<SwmPath>[base/src/lgipol01.cbl](base/src/lgipol01.cbl)</SwmPath>:1"
%%   
%%   
%% click rcchx openCode "<SwmPath>[base/src/lgipdb01.cbl](base/src/lgipdb01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
