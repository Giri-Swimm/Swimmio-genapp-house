---
title: Processing Insurance Data Records (LGAPVS01) - Overview
---
# Overview

This document explains how insurance request data is mapped to standardized records and written to the insurance data file. If the write fails, the flow ensures that a detailed error log is generated and queued for analysis.

## Dependencies

### Programs

- LGAPVS01 (<SwmPath>[base/src/lgapvs01.cbl](base/src/lgapvs01.cbl)</SwmPath>)
- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)

### Copybook

- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  5rxbv("Database Policy Data Insertion (LGAPDB09)") --> e9n12("Processing Insurance Data Records (LGAPVS01)"):::currentEntity
click 5rxbv openCode "base/src/lgapdb09.cbl:1"
  
  
click e9n12 openCode "base/src/lgapvs01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   5rxbv("Database Policy Data Insertion (LGAPDB09)") --> e9n12("Processing Insurance Data Records (LGAPVS01)"):::currentEntity
%% click 5rxbv openCode "<SwmPath>[base/src/lgapdb09.cbl](base/src/lgapdb09.cbl)</SwmPath>:1"
%%   
%%   
%% click e9n12 openCode "<SwmPath>[base/src/lgapvs01.cbl](base/src/lgapvs01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
