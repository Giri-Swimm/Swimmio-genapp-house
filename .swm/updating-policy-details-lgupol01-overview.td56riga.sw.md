---
title: Updating Policy Details (LGUPOL01) - Overview
---
# Overview

This document explains the flow for updating insurance policy details for Endowment, House, and Motor policies. Incoming requests are validated and tagged with transaction context. Valid requests are processed and policy records are updated, while errors and insufficient data are logged with relevant context.

## Dependencies

### Programs

- LGUPOL01 (<SwmPath>[base/src/lgupol01.cbl](base/src/lgupol01.cbl)</SwmPath>)
- LGUPDB01 (<SwmPath>[base/src/lgupdb01.cbl](base/src/lgupdb01.cbl)</SwmPath>)
- LGUPVS01 (<SwmPath>[base/src/lgupvs01.cbl](base/src/lgupvs01.cbl)</SwmPath>)
- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)

### Copybooks

- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)
- LGPOLICY (<SwmPath>[base/src/lgpolicy.cpy](base/src/lgpolicy.cpy)</SwmPath>)
- SQLCA

# Where is this program used?

This program is used multiple times in the codebase as represented in the following diagram:

```mermaid
graph TD
  c40kt("House Policy Menu (LGTESTP3)") --> enaje("Updating Policy Details (LGUPOL01)"):::currentEntity
click c40kt openCode "base/src/lgtestp3.cbl:1"
2l995("Motor Policy Menu (LGTESTP1)") --> enaje("Updating Policy Details (LGUPOL01)"):::currentEntity
click 2l995 openCode "base/src/lgtestp1.cbl:1"
a63k6("Endowment Policy Menu (LGTESTP2)") --> enaje("Updating Policy Details (LGUPOL01)"):::currentEntity
click a63k6 openCode "base/src/lgtestp2.cbl:1"
  
  
click enaje openCode "base/src/lgupol01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   c40kt("House Policy Menu (LGTESTP3)") --> enaje("Updating Policy Details (LGUPOL01)"):::currentEntity
%% click c40kt openCode "<SwmPath>[base/src/lgtestp3.cbl](base/src/lgtestp3.cbl)</SwmPath>:1"
%% 2l995("Motor Policy Menu (LGTESTP1)") --> enaje("Updating Policy Details (LGUPOL01)"):::currentEntity
%% click 2l995 openCode "<SwmPath>[base/src/lgtestp1.cbl](base/src/lgtestp1.cbl)</SwmPath>:1"
%% a63k6("Endowment Policy Menu (LGTESTP2)") --> enaje("Updating Policy Details (LGUPOL01)"):::currentEntity
%% click a63k6 openCode "<SwmPath>[base/src/lgtestp2.cbl](base/src/lgtestp2.cbl)</SwmPath>:1"
%%   
%%   
%% click enaje openCode "<SwmPath>[base/src/lgupol01.cbl](base/src/lgupol01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
