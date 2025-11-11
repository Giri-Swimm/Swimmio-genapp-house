---
title: Deleting Policy Records (LGDPVS01) - Overview
---
# Overview

This document describes the flow for deleting a policy record. The process receives a request with policy and customer identifiers, attempts to remove the corresponding policy from the system, and, if unsuccessful, logs all relevant error details to both temporary and permanent queues for traceability and diagnostics.

## Dependencies

### Programs

- LGDPVS01 (<SwmPath>[base/src/lgdpvs01.cbl](base/src/lgdpvs01.cbl)</SwmPath>)
- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)

### Copybook

- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  yinrt("Deleting Policy Records (LGDPDB01)") --> 2a3lk("Deleting Policy Records (LGDPVS01)"):::currentEntity
click yinrt openCode "base/src/lgdpdb01.cbl:1"
  
  
click 2a3lk openCode "base/src/lgdpvs01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   yinrt("Deleting Policy Records (LGDPDB01)") --> 2a3lk("Deleting Policy Records (LGDPVS01)"):::currentEntity
%% click yinrt openCode "<SwmPath>[base/src/lgdpdb01.cbl](base/src/lgdpdb01.cbl)</SwmPath>:1"
%%   
%%   
%% click 2a3lk openCode "<SwmPath>[base/src/lgdpvs01.cbl](base/src/lgdpvs01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
