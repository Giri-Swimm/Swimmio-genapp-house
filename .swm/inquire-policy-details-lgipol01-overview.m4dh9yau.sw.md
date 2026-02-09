---
title: Inquire Policy Details (LGIPOL01) - Overview
---
# Overview

This document explains the flow for handling insurance policy inquiries. When a user requests details for an endowment, house, or motor policy, the system validates the request, determines the policy type, and retrieves the relevant information from the database.

```mermaid
flowchart TD
    node1{"Which policy type is requested?
(Setting Up Inquiry and Policy Type Routing)"}:::HeadingStyle
    click node1 goToHeading "Setting Up Inquiry and Policy Type Routing"
    node1 -->|"Endowment"|node2["Fetching Endowment Policy Data"]:::HeadingStyle
    click node2 goToHeading "Fetching Endowment Policy Data"
    node1 -->|"House"|node3["Fetching House Policy Data"]:::HeadingStyle
    click node3 goToHeading "Fetching House Policy Data"
    node1 -->|"Motor"|node4["Fetching Motor Policy Data"]:::HeadingStyle
    click node4 goToHeading "Fetching Motor Policy Data"
    node1 -->|"Other"|node5["Routing Other Policy Types"]:::HeadingStyle
    click node5 goToHeading "Routing Other Policy Types"
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
  iae71("Motor Policy Menu (LGTESTP1)") --> c4y3b("Inquire Policy Details (LGIPOL01)"):::currentEntity
click iae71 openCode "base/src/lgtestp1.cbl:1"
9gbew("House Policy Menu (LGTESTP3)") --> c4y3b("Inquire Policy Details (LGIPOL01)"):::currentEntity
click 9gbew openCode "base/src/lgtestp3.cbl:1"
ny1xe("Endowment Policy Menu (LGTESTP2)") --> c4y3b("Inquire Policy Details (LGIPOL01)"):::currentEntity
click ny1xe openCode "base/src/lgtestp2.cbl:1"
63u0o("Managing Commercial Policy Data (LGTESTP4)") --> c4y3b("Inquire Policy Details (LGIPOL01)"):::currentEntity
click 63u0o openCode "base/src/lgtestp4.cbl:1"
  
  
click c4y3b openCode "base/src/lgipol01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   iae71("Motor Policy Menu (LGTESTP1)") --> c4y3b("Inquire Policy Details (LGIPOL01)"):::currentEntity
%% click iae71 openCode "<SwmPath>[base/src/lgtestp1.cbl](base/src/lgtestp1.cbl)</SwmPath>:1"
%% 9gbew("House Policy Menu (LGTESTP3)") --> c4y3b("Inquire Policy Details (LGIPOL01)"):::currentEntity
%% click 9gbew openCode "<SwmPath>[base/src/lgtestp3.cbl](base/src/lgtestp3.cbl)</SwmPath>:1"
%% ny1xe("Endowment Policy Menu (LGTESTP2)") --> c4y3b("Inquire Policy Details (LGIPOL01)"):::currentEntity
%% click ny1xe openCode "<SwmPath>[base/src/lgtestp2.cbl](base/src/lgtestp2.cbl)</SwmPath>:1"
%% 63u0o("Managing Commercial Policy Data (LGTESTP4)") --> c4y3b("Inquire Policy Details (LGIPOL01)"):::currentEntity
%% click 63u0o openCode "<SwmPath>[base/src/lgtestp4.cbl](base/src/lgtestp4.cbl)</SwmPath>:1"
%%   
%%   
%% click c4y3b openCode "<SwmPath>[base/src/lgipol01.cbl](base/src/lgipol01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
