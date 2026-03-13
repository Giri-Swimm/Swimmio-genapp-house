---
title: (LGTESTP2) Endowment policy menu management - Dependencies
---
# Dependencies

```mermaid
graph TD
  
  qpeb7("(LGTESTP2) Endowment policy menu management"):::currentEntity --> cdm9t("LGIPOL01")
click cdm9t openCode "base/src/lgipol01.cbl:1"
  cdm9t("LGIPOL01") --> 24kbd("LGIPDB01")
click 24kbd openCode "base/src/lgipdb01.cbl:1"
  24kbd("LGIPDB01") --> xcbo4("LGSTSQ")
click xcbo4 openCode "base/src/lgstsq.cbl:1"
  
  
  
cdm9t("LGIPOL01") --> xs3n4("LGSTSQ")
click xs3n4 openCode "base/src/lgstsq.cbl:1"
  
  
  
qpeb7("(LGTESTP2) Endowment policy menu management"):::currentEntity --> aznv8("LGAPOL01")
click aznv8 openCode "base/src/lgapol01.cbl:1"
  aznv8("LGAPOL01") --> b7pul("LGAPDB01")
click b7pul openCode "base/src/LGAPDB01.cbl:1"
  b7pul("LGAPDB01") --> npw6u("LGAPDB02")
click npw6u openCode "base/src/LGAPDB02.cbl:1"
  
  
b7pul("LGAPDB01") --> zanwl("LGAPDB03")
click zanwl openCode "base/src/LGAPDB03.cbl:1"
  
  
b7pul("LGAPDB01") --> vaoq6("LGAPDB04")
click vaoq6 openCode "base/src/LGAPDB04.cbl:1"
  
  
  
aznv8("LGAPOL01") --> 4ar5x("LGSTSQ")
click 4ar5x openCode "base/src/lgstsq.cbl:1"
  
  
  
qpeb7("(LGTESTP2) Endowment policy menu management"):::currentEntity --> a7r19("LGDPOL01")
click a7r19 openCode "base/src/lgdpol01.cbl:1"
  a7r19("LGDPOL01") --> wq7t2("LGDPDB01")
click wq7t2 openCode "base/src/lgdpdb01.cbl:1"
  wq7t2("LGDPDB01") --> yjygx("LGDPVS01")
click yjygx openCode "base/src/lgdpvs01.cbl:1"
  yjygx("LGDPVS01") --> e7gz4("LGSTSQ")
click e7gz4 openCode "base/src/lgstsq.cbl:1"
  
  
  
wq7t2("LGDPDB01") --> imgqs("LGSTSQ")
click imgqs openCode "base/src/lgstsq.cbl:1"
  
  
  
a7r19("LGDPOL01") --> 3h6j4("LGSTSQ")
click 3h6j4 openCode "base/src/lgstsq.cbl:1"
  
  
  
qpeb7("(LGTESTP2) Endowment policy menu management"):::currentEntity --> tp8g8("LGUPOL01")
click tp8g8 openCode "base/src/lgupol01.cbl:1"
  tp8g8("LGUPOL01") --> ju7ew("LGUPDB01")
click ju7ew openCode "base/src/lgupdb01.cbl:1"
  ju7ew("LGUPDB01") --> besf4("LGUPVS01")
click besf4 openCode "base/src/lgupvs01.cbl:1"
  besf4("LGUPVS01") --> e06w7("LGSTSQ")
click e06w7 openCode "base/src/lgstsq.cbl:1"
  
  
  
ju7ew("LGUPDB01") --> s42w7("LGSTSQ")
click s42w7 openCode "base/src/lgstsq.cbl:1"
  
  
  
tp8g8("LGUPOL01") --> cw796("LGSTSQ")
click cw796 openCode "base/src/lgstsq.cbl:1"
  
  
  
qpeb7("(LGTESTP2) Endowment policy menu management"):::currentEntity --> ckk03("SSP2")
  
  
  
click qpeb7 openCode "base/src/lgtestp2.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   
%%   qpeb7("(LGTESTP2) Endowment policy menu management"):::currentEntity --> cdm9t("LGIPOL01")
%% click cdm9t openCode "<SwmPath>[base/src/lgipol01.cbl](base/src/lgipol01.cbl)</SwmPath>:1"
%%   cdm9t("LGIPOL01") --> 24kbd("LGIPDB01")
%% click 24kbd openCode "<SwmPath>[base/src/lgipdb01.cbl](base/src/lgipdb01.cbl)</SwmPath>:1"
%%   24kbd("LGIPDB01") --> xcbo4("LGSTSQ")
%% click xcbo4 openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% cdm9t("LGIPOL01") --> xs3n4("LGSTSQ")
%% click xs3n4 openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% qpeb7("(LGTESTP2) Endowment policy menu management"):::currentEntity --> aznv8("LGAPOL01")
%% click aznv8 openCode "<SwmPath>[base/src/lgapol01.cbl](base/src/lgapol01.cbl)</SwmPath>:1"
%%   aznv8("LGAPOL01") --> b7pul("LGAPDB01")
%% click b7pul openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%   b7pul("LGAPDB01") --> npw6u("LGAPDB02")
%% click npw6u openCode "<SwmPath>[base/src/LGAPDB02.cbl](base/src/LGAPDB02.cbl)</SwmPath>:1"
%%   
%%   
%% b7pul("LGAPDB01") --> zanwl("LGAPDB03")
%% click zanwl openCode "<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>:1"
%%   
%%   
%% b7pul("LGAPDB01") --> vaoq6("LGAPDB04")
%% click vaoq6 openCode "<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% aznv8("LGAPOL01") --> 4ar5x("LGSTSQ")
%% click 4ar5x openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% qpeb7("(LGTESTP2) Endowment policy menu management"):::currentEntity --> a7r19("LGDPOL01")
%% click a7r19 openCode "<SwmPath>[base/src/lgdpol01.cbl](base/src/lgdpol01.cbl)</SwmPath>:1"
%%   a7r19("LGDPOL01") --> wq7t2("LGDPDB01")
%% click wq7t2 openCode "<SwmPath>[base/src/lgdpdb01.cbl](base/src/lgdpdb01.cbl)</SwmPath>:1"
%%   wq7t2("LGDPDB01") --> yjygx("LGDPVS01")
%% click yjygx openCode "<SwmPath>[base/src/lgdpvs01.cbl](base/src/lgdpvs01.cbl)</SwmPath>:1"
%%   yjygx("LGDPVS01") --> e7gz4("LGSTSQ")
%% click e7gz4 openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% wq7t2("LGDPDB01") --> imgqs("LGSTSQ")
%% click imgqs openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% a7r19("LGDPOL01") --> 3h6j4("LGSTSQ")
%% click 3h6j4 openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% qpeb7("(LGTESTP2) Endowment policy menu management"):::currentEntity --> tp8g8("LGUPOL01")
%% click tp8g8 openCode "<SwmPath>[base/src/lgupol01.cbl](base/src/lgupol01.cbl)</SwmPath>:1"
%%   tp8g8("LGUPOL01") --> ju7ew("LGUPDB01")
%% click ju7ew openCode "<SwmPath>[base/src/lgupdb01.cbl](base/src/lgupdb01.cbl)</SwmPath>:1"
%%   ju7ew("LGUPDB01") --> besf4("LGUPVS01")
%% click besf4 openCode "<SwmPath>[base/src/lgupvs01.cbl](base/src/lgupvs01.cbl)</SwmPath>:1"
%%   besf4("LGUPVS01") --> e06w7("LGSTSQ")
%% click e06w7 openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% ju7ew("LGUPDB01") --> s42w7("LGSTSQ")
%% click s42w7 openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% tp8g8("LGUPOL01") --> cw796("LGSTSQ")
%% click cw796 openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% qpeb7("(LGTESTP2) Endowment policy menu management"):::currentEntity --> ckk03("SSP2")
%%   
%%   
%%   
%% click qpeb7 openCode "<SwmPath>[base/src/lgtestp2.cbl](base/src/lgtestp2.cbl)</SwmPath>:1"
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
