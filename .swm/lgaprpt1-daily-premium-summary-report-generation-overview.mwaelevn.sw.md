---
title: LGAPRPT1 - Daily Premium Summary Report Generation - Overview
---
# Overview

This document explains the flow of generating daily premium summary reports. The system processes premium data, formats report headers, and calculates key statistics and breakdowns to provide management with insights into policy processing and risk analysis.

## Dependencies

### Program

- LGAPRPT1 (<SwmPath>[base/src/LGAPRPT1.cbl](base/src/LGAPRPT1.cbl)</SwmPath>)

### Copybook

- OUTPUTREC (<SwmPath>[base/src/OUTPUTREC.cpy](base/src/OUTPUTREC.cpy)</SwmPath>)

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  20ape("(LGAPJOB) Insurance policy premium calculation batch job") --> g9849("(LGAPRPT1) Daily premium summary report generator"):::currentEntity
click 20ape openCode "base/cntl/lgapjob.jcl:1"
  
  
click g9849 openCode "base/src/LGAPRPT1.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   20ape("(LGAPJOB) Insurance policy premium calculation batch job") --> g9849("(LGAPRPT1) Daily premium summary report generator"):::currentEntity
%% click 20ape openCode "<SwmPath>[base/cntl/lgapjob.jcl](base/cntl/lgapjob.jcl)</SwmPath>:1"
%%   
%%   
%% click g9849 openCode "<SwmPath>[base/src/LGAPRPT1.cbl](base/src/LGAPRPT1.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
