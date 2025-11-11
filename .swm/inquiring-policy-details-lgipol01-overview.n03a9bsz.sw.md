---
title: Inquiring Policy Details (LGIPOL01) - Overview
---
# Overview

This document describes the flow for inquiring about insurance policy details. When a request is made for a specific policy type (Endowment, House, or Motor), the flow validates the input, logs errors with timestamps, and retrieves the requested policy details if the request is valid.

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
  2mr0p("Managing Commercial Policy Operations (LGTESTP4)") --> 1dfcb("Inquiring Policy Details (LGIPOL01)"):::currentEntity
click 2mr0p openCode "base/src/lgtestp4.cbl:1"
tkrf0("House Policy Menu (LGTESTP3)") --> 1dfcb("Inquiring Policy Details (LGIPOL01)"):::currentEntity
click tkrf0 openCode "base/src/lgtestp3.cbl:1"
hldkz("Motor Policy Menu (LGTESTP1)") --> 1dfcb("Inquiring Policy Details (LGIPOL01)"):::currentEntity
click hldkz openCode "base/src/lgtestp1.cbl:1"
ld1jr("Endowment Policy Menu (LGTESTP2)") --> 1dfcb("Inquiring Policy Details (LGIPOL01)"):::currentEntity
click ld1jr openCode "base/src/lgtestp2.cbl:1"
  
  
click 1dfcb openCode "base/src/lgipol01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   2mr0p("Managing Commercial Policy Operations (LGTESTP4)") --> 1dfcb("Inquiring Policy Details (LGIPOL01)"):::currentEntity
%% click 2mr0p openCode "<SwmPath>[base/src/lgtestp4.cbl](base/src/lgtestp4.cbl)</SwmPath>:1"
%% tkrf0("House Policy Menu (LGTESTP3)") --> 1dfcb("Inquiring Policy Details (LGIPOL01)"):::currentEntity
%% click tkrf0 openCode "<SwmPath>[base/src/lgtestp3.cbl](base/src/lgtestp3.cbl)</SwmPath>:1"
%% hldkz("Motor Policy Menu (LGTESTP1)") --> 1dfcb("Inquiring Policy Details (LGIPOL01)"):::currentEntity
%% click hldkz openCode "<SwmPath>[base/src/lgtestp1.cbl](base/src/lgtestp1.cbl)</SwmPath>:1"
%% ld1jr("Endowment Policy Menu (LGTESTP2)") --> 1dfcb("Inquiring Policy Details (LGIPOL01)"):::currentEntity
%% click ld1jr openCode "<SwmPath>[base/src/lgtestp2.cbl](base/src/lgtestp2.cbl)</SwmPath>:1"
%%   
%%   
%% click 1dfcb openCode "<SwmPath>[base/src/lgipol01.cbl](base/src/lgipol01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
