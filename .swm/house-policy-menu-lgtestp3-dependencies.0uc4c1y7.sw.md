---
title: House Policy Menu (LGTESTP3) - Dependencies
---
# Dependencies

```mermaid
graph TD
  
  41q9y("House Policy Menu (LGTESTP3)"):::currentEntity --> 13wsa("LGIPOL01")
click 13wsa openCode "base/src/lgipol01.cbl:1"
  13wsa("LGIPOL01") --> pfmcd("LGIPDB01")
click pfmcd openCode "base/src/lgipdb01.cbl:1"
  pfmcd("LGIPDB01") --> xcdkm("LGSTSQ")
click xcdkm openCode "base/src/lgstsq.cbl:1"
  
  
  
13wsa("LGIPOL01") --> t552c("LGSTSQ")
click t552c openCode "base/src/lgstsq.cbl:1"
  
  
  
41q9y("House Policy Menu (LGTESTP3)"):::currentEntity --> hry1q("LGAPOL01")
click hry1q openCode "base/src/lgapol01.cbl:1"
  hry1q("LGAPOL01") --> l5m34("LGAPDB01")
click l5m34 openCode "base/src/LGAPDB01.cbl:1"
  l5m34("LGAPDB01") --> m5sy7("LGAPDB02")
click m5sy7 openCode "base/src/LGAPDB02.cbl:1"
  
  
l5m34("LGAPDB01") --> xptf8("LGAPDB03")
click xptf8 openCode "base/src/LGAPDB03.cbl:1"
  
  
l5m34("LGAPDB01") --> 4padq("LGAPDB04")
click 4padq openCode "base/src/LGAPDB04.cbl:1"
  
  
  
hry1q("LGAPOL01") --> og5a8("LGSTSQ")
click og5a8 openCode "base/src/lgstsq.cbl:1"
  
  
  
41q9y("House Policy Menu (LGTESTP3)"):::currentEntity --> qdd40("LGDPOL01")
click qdd40 openCode "base/src/lgdpol01.cbl:1"
  qdd40("LGDPOL01") --> xw9i4("LGDPDB01")
click xw9i4 openCode "base/src/lgdpdb01.cbl:1"
  xw9i4("LGDPDB01") --> xxwjb("LGDPVS01")
click xxwjb openCode "base/src/lgdpvs01.cbl:1"
  xxwjb("LGDPVS01") --> uz0gg("LGSTSQ")
click uz0gg openCode "base/src/lgstsq.cbl:1"
  
  
  
xw9i4("LGDPDB01") --> xzx65("LGSTSQ")
click xzx65 openCode "base/src/lgstsq.cbl:1"
  
  
  
qdd40("LGDPOL01") --> rzy9j("LGSTSQ")
click rzy9j openCode "base/src/lgstsq.cbl:1"
  
  
  
41q9y("House Policy Menu (LGTESTP3)"):::currentEntity --> e65bi("LGUPOL01")
click e65bi openCode "base/src/lgupol01.cbl:1"
  e65bi("LGUPOL01") --> yy2n4("LGUPDB01")
click yy2n4 openCode "base/src/lgupdb01.cbl:1"
  yy2n4("LGUPDB01") --> j8t76("LGUPVS01")
click j8t76 openCode "base/src/lgupvs01.cbl:1"
  j8t76("LGUPVS01") --> iyzvy("LGSTSQ")
click iyzvy openCode "base/src/lgstsq.cbl:1"
  
  
  
yy2n4("LGUPDB01") --> 03ro1("LGSTSQ")
click 03ro1 openCode "base/src/lgstsq.cbl:1"
  
  
  
e65bi("LGUPOL01") --> tj6vc("LGSTSQ")
click tj6vc openCode "base/src/lgstsq.cbl:1"
  
  
  
  
click 41q9y openCode "base/src/lgtestp3.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   
%%   41q9y("House Policy Menu (LGTESTP3)"):::currentEntity --> 13wsa("LGIPOL01")
%% click 13wsa openCode "<SwmPath>[base/src/lgipol01.cbl](base/src/lgipol01.cbl)</SwmPath>:1"
%%   13wsa("LGIPOL01") --> pfmcd("LGIPDB01")
%% click pfmcd openCode "<SwmPath>[base/src/lgipdb01.cbl](base/src/lgipdb01.cbl)</SwmPath>:1"
%%   pfmcd("LGIPDB01") --> xcdkm("LGSTSQ")
%% click xcdkm openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% 13wsa("LGIPOL01") --> t552c("LGSTSQ")
%% click t552c openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% 41q9y("House Policy Menu (LGTESTP3)"):::currentEntity --> hry1q("LGAPOL01")
%% click hry1q openCode "<SwmPath>[base/src/lgapol01.cbl](base/src/lgapol01.cbl)</SwmPath>:1"
%%   hry1q("LGAPOL01") --> l5m34("LGAPDB01")
%% click l5m34 openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%   l5m34("LGAPDB01") --> m5sy7("LGAPDB02")
%% click m5sy7 openCode "<SwmPath>[base/src/LGAPDB02.cbl](base/src/LGAPDB02.cbl)</SwmPath>:1"
%%   
%%   
%% l5m34("LGAPDB01") --> xptf8("LGAPDB03")
%% click xptf8 openCode "<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>:1"
%%   
%%   
%% l5m34("LGAPDB01") --> 4padq("LGAPDB04")
%% click 4padq openCode "<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% hry1q("LGAPOL01") --> og5a8("LGSTSQ")
%% click og5a8 openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% 41q9y("House Policy Menu (LGTESTP3)"):::currentEntity --> qdd40("LGDPOL01")
%% click qdd40 openCode "<SwmPath>[base/src/lgdpol01.cbl](base/src/lgdpol01.cbl)</SwmPath>:1"
%%   qdd40("LGDPOL01") --> xw9i4("LGDPDB01")
%% click xw9i4 openCode "<SwmPath>[base/src/lgdpdb01.cbl](base/src/lgdpdb01.cbl)</SwmPath>:1"
%%   xw9i4("LGDPDB01") --> xxwjb("LGDPVS01")
%% click xxwjb openCode "<SwmPath>[base/src/lgdpvs01.cbl](base/src/lgdpvs01.cbl)</SwmPath>:1"
%%   xxwjb("LGDPVS01") --> uz0gg("LGSTSQ")
%% click uz0gg openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% xw9i4("LGDPDB01") --> xzx65("LGSTSQ")
%% click xzx65 openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% qdd40("LGDPOL01") --> rzy9j("LGSTSQ")
%% click rzy9j openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% 41q9y("House Policy Menu (LGTESTP3)"):::currentEntity --> e65bi("LGUPOL01")
%% click e65bi openCode "<SwmPath>[base/src/lgupol01.cbl](base/src/lgupol01.cbl)</SwmPath>:1"
%%   e65bi("LGUPOL01") --> yy2n4("LGUPDB01")
%% click yy2n4 openCode "<SwmPath>[base/src/lgupdb01.cbl](base/src/lgupdb01.cbl)</SwmPath>:1"
%%   yy2n4("LGUPDB01") --> j8t76("LGUPVS01")
%% click j8t76 openCode "<SwmPath>[base/src/lgupvs01.cbl](base/src/lgupvs01.cbl)</SwmPath>:1"
%%   j8t76("LGUPVS01") --> iyzvy("LGSTSQ")
%% click iyzvy openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% yy2n4("LGUPDB01") --> 03ro1("LGSTSQ")
%% click 03ro1 openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% e65bi("LGUPOL01") --> tj6vc("LGSTSQ")
%% click tj6vc openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%%   
%% click 41q9y openCode "<SwmPath>[base/src/lgtestp3.cbl](base/src/lgtestp3.cbl)</SwmPath>:1"
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

<SwmPath>[base/src/lgupol01.cbl](base/src/lgupol01.cbl)</SwmPath>

<SwmPath>[base/src/lgupdb01.cbl](base/src/lgupdb01.cbl)</SwmPath>

<SwmPath>[base/src/lgupvs01.cbl](base/src/lgupvs01.cbl)</SwmPath>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
