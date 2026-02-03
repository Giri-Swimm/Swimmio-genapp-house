---
title: Managing Commercial Policy Operations (LGTESTP4) - Dependencies
---
# Dependencies

```mermaid
graph TD
  
  mn1pi("Managing Commercial Policy Operations (LGTESTP4)"):::currentEntity --> 1neai("LGIPOL01")
click 1neai openCode "base/src/lgipol01.cbl:1"
  1neai("LGIPOL01") --> ys853("LGIPDB01")
click ys853 openCode "base/src/lgipdb01.cbl:1"
  ys853("LGIPDB01") --> pu3rj("LGSTSQ")
click pu3rj openCode "base/src/lgstsq.cbl:1"
  
  
  
1neai("LGIPOL01") --> lx2rb("LGSTSQ")
click lx2rb openCode "base/src/lgstsq.cbl:1"
  
  
  
mn1pi("Managing Commercial Policy Operations (LGTESTP4)"):::currentEntity --> e1ec7("LGAPOL01")
click e1ec7 openCode "base/src/lgapol01.cbl:1"
  e1ec7("LGAPOL01") --> ecbeb("LGAPDB01")
click ecbeb openCode "base/src/LGAPDB01.cbl:1"
  ecbeb("LGAPDB01") --> hzxyh("LGAPDB02")
click hzxyh openCode "base/src/LGAPDB02.cbl:1"
  
  
ecbeb("LGAPDB01") --> wahcq("LGAPDB03")
click wahcq openCode "base/src/LGAPDB03.cbl:1"
  
  
ecbeb("LGAPDB01") --> 9j9b8("LGAPDB04")
click 9j9b8 openCode "base/src/LGAPDB04.cbl:1"
  
  
  
e1ec7("LGAPOL01") --> l3s0t("LGSTSQ")
click l3s0t openCode "base/src/lgstsq.cbl:1"
  
  
  
mn1pi("Managing Commercial Policy Operations (LGTESTP4)"):::currentEntity --> 6dqwk("LGDPOL01")
click 6dqwk openCode "base/src/lgdpol01.cbl:1"
  6dqwk("LGDPOL01") --> m71p7("LGDPDB01")
click m71p7 openCode "base/src/lgdpdb01.cbl:1"
  m71p7("LGDPDB01") --> 3ctdv("LGDPVS01")
click 3ctdv openCode "base/src/lgdpvs01.cbl:1"
  3ctdv("LGDPVS01") --> vnhjw("LGSTSQ")
click vnhjw openCode "base/src/lgstsq.cbl:1"
  
  
  
m71p7("LGDPDB01") --> 4615b("LGSTSQ")
click 4615b openCode "base/src/lgstsq.cbl:1"
  
  
  
6dqwk("LGDPOL01") --> wpgby("LGSTSQ")
click wpgby openCode "base/src/lgstsq.cbl:1"
  
  
  
  
click mn1pi openCode "base/src/lgtestp4.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   
%%   mn1pi("Managing Commercial Policy Operations (LGTESTP4)"):::currentEntity --> 1neai("LGIPOL01")
%% click 1neai openCode "<SwmPath>[base/src/lgipol01.cbl](base/src/lgipol01.cbl)</SwmPath>:1"
%%   1neai("LGIPOL01") --> ys853("LGIPDB01")
%% click ys853 openCode "<SwmPath>[base/src/lgipdb01.cbl](base/src/lgipdb01.cbl)</SwmPath>:1"
%%   ys853("LGIPDB01") --> pu3rj("LGSTSQ")
%% click pu3rj openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% 1neai("LGIPOL01") --> lx2rb("LGSTSQ")
%% click lx2rb openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% mn1pi("Managing Commercial Policy Operations (LGTESTP4)"):::currentEntity --> e1ec7("LGAPOL01")
%% click e1ec7 openCode "<SwmPath>[base/src/lgapol01.cbl](base/src/lgapol01.cbl)</SwmPath>:1"
%%   e1ec7("LGAPOL01") --> ecbeb("LGAPDB01")
%% click ecbeb openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%   ecbeb("LGAPDB01") --> hzxyh("LGAPDB02")
%% click hzxyh openCode "<SwmPath>[base/src/LGAPDB02.cbl](base/src/LGAPDB02.cbl)</SwmPath>:1"
%%   
%%   
%% ecbeb("LGAPDB01") --> wahcq("LGAPDB03")
%% click wahcq openCode "<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>:1"
%%   
%%   
%% ecbeb("LGAPDB01") --> 9j9b8("LGAPDB04")
%% click 9j9b8 openCode "<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% e1ec7("LGAPOL01") --> l3s0t("LGSTSQ")
%% click l3s0t openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% mn1pi("Managing Commercial Policy Operations (LGTESTP4)"):::currentEntity --> 6dqwk("LGDPOL01")
%% click 6dqwk openCode "<SwmPath>[base/src/lgdpol01.cbl](base/src/lgdpol01.cbl)</SwmPath>:1"
%%   6dqwk("LGDPOL01") --> m71p7("LGDPDB01")
%% click m71p7 openCode "<SwmPath>[base/src/lgdpdb01.cbl](base/src/lgdpdb01.cbl)</SwmPath>:1"
%%   m71p7("LGDPDB01") --> 3ctdv("LGDPVS01")
%% click 3ctdv openCode "<SwmPath>[base/src/lgdpvs01.cbl](base/src/lgdpvs01.cbl)</SwmPath>:1"
%%   3ctdv("LGDPVS01") --> vnhjw("LGSTSQ")
%% click vnhjw openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% m71p7("LGDPDB01") --> 4615b("LGSTSQ")
%% click 4615b openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% 6dqwk("LGDPOL01") --> wpgby("LGSTSQ")
%% click wpgby openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%%   
%% click mn1pi openCode "<SwmPath>[base/src/lgtestp4.cbl](base/src/lgtestp4.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

## Paths

<SwmPath>[base/src/lgipol01.cbl](base/src/lgipol01.cbl)</SwmPath>

<SwmPath>[base/src/lgipdb01.cbl](base/src/lgipdb01.cbl)</SwmPath>

<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>

<SwmPath>[base/src/lgpolicy.cpy](base/src/lgpolicy.cpy)</SwmPath>

<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>

<SwmPath>[base/src/lgapol01.cbl](base/src/lgapol01.cbl)</SwmPath>

<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>

<SwmPath>[base/src/LGAPDB02.cbl](base/src/LGAPDB02.cbl)</SwmPath>

<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>

<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>

<SwmPath>[base/src/INPUTREC2.cpy](base/src/INPUTREC2.cpy)</SwmPath>

<SwmPath>[base/src/OUTPUTREC.cpy](base/src/OUTPUTREC.cpy)</SwmPath>

<SwmPath>[base/src/WORKSTOR.cpy](base/src/WORKSTOR.cpy)</SwmPath>

<SwmPath>[base/src/LGAPACT.cpy](base/src/LGAPACT.cpy)</SwmPath>

<SwmPath>[base/src/lgdpol01.cbl](base/src/lgdpol01.cbl)</SwmPath>

<SwmPath>[base/src/lgdpdb01.cbl](base/src/lgdpdb01.cbl)</SwmPath>

<SwmPath>[base/src/lgdpvs01.cbl](base/src/lgdpvs01.cbl)</SwmPath>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
