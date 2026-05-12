---
title: (LGAPDB09) Inserting Insurance Records - Dependencies
---
# Dependencies

```mermaid
graph TD
  
  7zan8("(LGAPDB09) Inserting Insurance Records"):::currentEntity --> w3703("LGAPVS01")
click w3703 openCode "base/src/lgapvs01.cbl:1"
  w3703("LGAPVS01") --> gw2y2("LGSTSQ")
click gw2y2 openCode "base/src/lgstsq.cbl:1"
  
  
  
7zan8("(LGAPDB09) Inserting Insurance Records"):::currentEntity --> 0x3ag("LGCOMCAL")
click 0x3ag openCode "base/src/lgcomcal.cbl:1"
  
  
7zan8("(LGAPDB09) Inserting Insurance Records"):::currentEntity --> l8krm("LGSTSQ")
click l8krm openCode "base/src/lgstsq.cbl:1"
  
  
  
click 7zan8 openCode "base/src/lgapdb09.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   
%%   7zan8("(LGAPDB09) Inserting Insurance Records"):::currentEntity --> w3703("LGAPVS01")
%% click w3703 openCode "<SwmPath>[base/src/lgapvs01.cbl](base/src/lgapvs01.cbl)</SwmPath>:1"
%%   w3703("LGAPVS01") --> gw2y2("LGSTSQ")
%% click gw2y2 openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% 7zan8("(LGAPDB09) Inserting Insurance Records"):::currentEntity --> 0x3ag("LGCOMCAL")
%% click 0x3ag openCode "<SwmPath>[base/src/lgcomcal.cbl](base/src/lgcomcal.cbl)</SwmPath>:1"
%%   
%%   
%% 7zan8("(LGAPDB09) Inserting Insurance Records"):::currentEntity --> l8krm("LGSTSQ")
%% click l8krm openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% click 7zan8 openCode "<SwmPath>[base/src/lgapdb09.cbl](base/src/lgapdb09.cbl)</SwmPath>:1"
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
