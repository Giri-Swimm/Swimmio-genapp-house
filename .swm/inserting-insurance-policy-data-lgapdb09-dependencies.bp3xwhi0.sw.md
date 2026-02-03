---
title: Inserting Insurance Policy Data (LGAPDB09) - Dependencies
---
# Dependencies

```mermaid
graph TD
  
  31wd1("Inserting Insurance Policy Data (LGAPDB09)"):::currentEntity --> uz4y6("LGAPVS01")
click uz4y6 openCode "base/src/lgapvs01.cbl:1"
  uz4y6("LGAPVS01") --> udnpc("LGSTSQ")
click udnpc openCode "base/src/lgstsq.cbl:1"
  
  
  
31wd1("Inserting Insurance Policy Data (LGAPDB09)"):::currentEntity --> yn8kh("LGCOMCAL")
click yn8kh openCode "base/src/lgcomcal.cbl:1"
  
  
31wd1("Inserting Insurance Policy Data (LGAPDB09)"):::currentEntity --> 83htt("LGSTSQ")
click 83htt openCode "base/src/lgstsq.cbl:1"
  
  
  
click 31wd1 openCode "base/src/lgapdb09.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   
%%   31wd1("Inserting Insurance Policy Data (LGAPDB09)"):::currentEntity --> uz4y6("LGAPVS01")
%% click uz4y6 openCode "<SwmPath>[base/src/lgapvs01.cbl](base/src/lgapvs01.cbl)</SwmPath>:1"
%%   uz4y6("LGAPVS01") --> udnpc("LGSTSQ")
%% click udnpc openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% 31wd1("Inserting Insurance Policy Data (LGAPDB09)"):::currentEntity --> yn8kh("LGCOMCAL")
%% click yn8kh openCode "<SwmPath>[base/src/lgcomcal.cbl](base/src/lgcomcal.cbl)</SwmPath>:1"
%%   
%%   
%% 31wd1("Inserting Insurance Policy Data (LGAPDB09)"):::currentEntity --> 83htt("LGSTSQ")
%% click 83htt openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% click 31wd1 openCode "<SwmPath>[base/src/lgapdb09.cbl](base/src/lgapdb09.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

## Paths

<SwmPath>[base/src/lgapvs01.cbl](base/src/lgapvs01.cbl)</SwmPath>

<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>

<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>

<SwmPath>[base/src/lgcomcal.cbl](base/src/lgcomcal.cbl)</SwmPath>

<SwmPath>[base/src/lgcomdat.cpy](base/src/lgcomdat.cpy)</SwmPath>

<SwmPath>[base/src/lgpolicy.cpy](base/src/lgpolicy.cpy)</SwmPath>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
