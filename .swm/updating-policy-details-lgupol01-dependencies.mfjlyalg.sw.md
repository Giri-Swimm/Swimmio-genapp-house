---
title: Updating Policy Details (LGUPOL01) - Dependencies
---
# Dependencies

```mermaid
graph TD
  
  u24kb("Updating Policy Details (LGUPOL01)"):::currentEntity --> 4nkaq("LGUPDB01")
click 4nkaq openCode "base/src/lgupdb01.cbl:1"
  4nkaq("LGUPDB01") --> e1l2t("LGUPVS01")
  
  
4nkaq("LGUPDB01") --> j30dt("LGSTSQ")
  
  
  
u24kb("Updating Policy Details (LGUPOL01)"):::currentEntity --> 9r12y("LGSTSQ")
  
  
  
click u24kb openCode "base/src/lgupol01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   
%%   u24kb("Updating Policy Details (LGUPOL01)"):::currentEntity --> 4nkaq("LGUPDB01")
%% click 4nkaq openCode "<SwmPath>[base/src/lgupdb01.cbl](base/src/lgupdb01.cbl)</SwmPath>:1"
%%   4nkaq("LGUPDB01") --> e1l2t("LGUPVS01")
%%   
%%   
%% 4nkaq("LGUPDB01") --> j30dt("LGSTSQ")
%%   
%%   
%%   
%% u24kb("Updating Policy Details (LGUPOL01)"):::currentEntity --> 9r12y("LGSTSQ")
%%   
%%   
%%   
%% click u24kb openCode "<SwmPath>[base/src/lgupol01.cbl](base/src/lgupol01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

## Paths

<SwmPath>[base/src/lgupdb01.cbl](base/src/lgupdb01.cbl)</SwmPath>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
