---
title: Configuring Application Regions and Workload Management (DEFCPSM)
---
This document explains the flow of configuring application regions and workload management groups (DEFCPSM). It sets up the logical layout and control rules for organizing regions and workload groups. The flow processes configuration commands as input and outputs a logical model used for transaction routing and workload balancing. For example, defining regions and linking them to workload groups allows the system to manage transaction routing effectively.

## Configure Application Regions and Workload Management

Step in this section: `EYU9XDBT`.

The section establishes the logical layout and control rules for how main application regions and workload groups are grouped, managed, and linked for workload handling across the Swimmio-genapp-house system.

- The input contains configuration commands that define regions, groups, transaction segments, workload definitions, and cross-references among these entities.
- Each region and group definition in the input is parsed, then used to build an internal logical structure in the application management system.
- Workload transaction groups and routing rules are created and linked according to the input specifications, associating regions and logical groups with specific transaction and workload definitions.
- The output is a configured logical model that the system uses to handle transaction routing, workload grouping, and balancing based on the specified rules, making these structures available for operational workload management.

### Input

**EYU9XDBT.SYSIN**

Region, group, and workload configuration input definitions

Sample:

```
CONTEXT <CMASAPPL>
DEFINE CICSPLEX GNAPPLEX
*
CONTEXT GNAPPLEX
*
DEFINE REGION    <TORAPPL>    -
       APPLID    <TORAPPL>    -
       SYSID     <TORSYSID>   -
       CMASID    <CMASAPPL>
*
```

**EYU9XDBT.RESDEFS**

Definitions for resource groups, workload groups, transaction groups, and workload controls

Sample:

```
DEFINE RESGROUP GENAPP
RESGROUP_DESCCODEPAGE = "37";
RESGROUP_RESGROUP     = "GENAPP";
DEFINE TRANGRP GENAWLMT
TRANGRP_DESC = "DOR executable trans";
DEFINE WLMSPEC GENAWLM
WLMSPEC_DESC = "GENA Workload";
```

### Output

**CICSPLEX logical structure**

Configured regions, region groups, transaction group definitions, workload specification and groupings for application workload management

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
