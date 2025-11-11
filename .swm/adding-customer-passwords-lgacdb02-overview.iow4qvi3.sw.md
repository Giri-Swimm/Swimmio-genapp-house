---
title: Adding Customer Passwords (LGACDB02) - Overview
---
# Overview

This document describes how customer credentials are securely added to the system. When a new customer request is received, the flow stores the customer's password and related details in the secure table, with error handling and logging to ensure traceability.

```mermaid
flowchart TD
    node1["Processing Customer Security Requests
(Processing Customer Security Requests)"]:::HeadingStyle --> node2{"Is request to add new customer?"}
    click node1 goToHeading "Processing Customer Security Requests"
    node2 -->|"Yes"| node3["Adding Credentials to Secure Table"]:::HeadingStyle
    click node3 goToHeading "Adding Credentials to Secure Table"
    node2 -->|"No"| node4["Return outcome to caller
(Processing Customer Security Requests)"]:::HeadingStyle
    node3 -->|"Success or failure"| node4
    click node4 goToHeading "Processing Customer Security Requests"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Programs

- LGACDB02 (<SwmPath>[base/src/lgacdb02.cbl](base/src/lgacdb02.cbl)</SwmPath>)
- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)

### Copybooks

- LGPOLICY (<SwmPath>[base/src/lgpolicy.cpy](base/src/lgpolicy.cpy)</SwmPath>)
- SQLCA

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  osr45("Adding Customer Details (LGACDB01)") --> u6eec("Adding Customer Passwords (LGACDB02)"):::currentEntity
click osr45 openCode "base/src/lgacdb01.cbl:1"
  
  
click u6eec openCode "base/src/lgacdb02.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   osr45("Adding Customer Details (LGACDB01)") --> u6eec("Adding Customer Passwords (LGACDB02)"):::currentEntity
%% click osr45 openCode "<SwmPath>[base/src/lgacdb01.cbl](base/src/lgacdb01.cbl)</SwmPath>:1"
%%   
%%   
%% click u6eec openCode "<SwmPath>[base/src/lgacdb02.cbl](base/src/lgacdb02.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
