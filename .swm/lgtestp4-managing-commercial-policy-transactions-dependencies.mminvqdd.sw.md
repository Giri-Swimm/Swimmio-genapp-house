---
title: (LGTESTP4) Managing commercial policy transactions - Dependencies
---
# Dependencies

```mermaid
graph TD
  
  0hr7k("(LGTESTP4) Managing commercial policy transactions"):::currentEntity --> 9ai3d("LGIPOL01")
click 9ai3d openCode "base/src/lgipol01.cbl:1"
  9ai3d("LGIPOL01") --> 244gc("LGIPDB01")
click 244gc openCode "base/src/lgipdb01.cbl:1"
  244gc("LGIPDB01") --> 7x80j("LGSTSQ")
click 7x80j openCode "base/src/lgstsq.cbl:1"
  
  
  
9ai3d("LGIPOL01") --> l7sy4("LGSTSQ")
click l7sy4 openCode "base/src/lgstsq.cbl:1"
  
  
  
0hr7k("(LGTESTP4) Managing commercial policy transactions"):::currentEntity --> 4ospi("LGAPOL01")
click 4ospi openCode "base/src/lgapol01.cbl:1"
  4ospi("LGAPOL01") --> i3gyi("LGAPDB01")
click i3gyi openCode "base/src/LGAPDB01.cbl:1"
  i3gyi("LGAPDB01") --> uio94("LGAPDB02")
click uio94 openCode "base/src/LGAPDB02.cbl:1"
  
  
i3gyi("LGAPDB01") --> eb9q4("LGAPDB03")
click eb9q4 openCode "base/src/LGAPDB03.cbl:1"
  
  
i3gyi("LGAPDB01") --> gxdg2("LGAPDB04")
click gxdg2 openCode "base/src/LGAPDB04.cbl:1"
  
  
  
4ospi("LGAPOL01") --> xrlaq("LGSTSQ")
click xrlaq openCode "base/src/lgstsq.cbl:1"
  
  
  
0hr7k("(LGTESTP4) Managing commercial policy transactions"):::currentEntity --> i4hoq("LGDPOL01")
click i4hoq openCode "base/src/lgdpol01.cbl:1"
  i4hoq("LGDPOL01") --> ql57r("LGDPDB01")
click ql57r openCode "base/src/lgdpdb01.cbl:1"
  ql57r("LGDPDB01") --> tcqr9("LGDPVS01")
click tcqr9 openCode "base/src/lgdpvs01.cbl:1"
  tcqr9("LGDPVS01") --> f39pp("LGSTSQ")
click f39pp openCode "base/src/lgstsq.cbl:1"
  
  
  
ql57r("LGDPDB01") --> g54cy("LGSTSQ")
click g54cy openCode "base/src/lgstsq.cbl:1"
  
  
  
i4hoq("LGDPOL01") --> a6px0("LGSTSQ")
click a6px0 openCode "base/src/lgstsq.cbl:1"
  
  
  
0hr7k("(LGTESTP4) Managing commercial policy transactions"):::currentEntity --> teiar("SSP4")
  
  
  
click 0hr7k openCode "base/src/lgtestp4.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   
%%   0hr7k("(LGTESTP4) Managing commercial policy transactions"):::currentEntity --> 9ai3d("LGIPOL01")
%% click 9ai3d openCode "<SwmPath>[base/src/lgipol01.cbl](base/src/lgipol01.cbl)</SwmPath>:1"
%%   9ai3d("LGIPOL01") --> 244gc("LGIPDB01")
%% click 244gc openCode "<SwmPath>[base/src/lgipdb01.cbl](base/src/lgipdb01.cbl)</SwmPath>:1"
%%   244gc("LGIPDB01") --> 7x80j("LGSTSQ")
%% click 7x80j openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% 9ai3d("LGIPOL01") --> l7sy4("LGSTSQ")
%% click l7sy4 openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% 0hr7k("(LGTESTP4) Managing commercial policy transactions"):::currentEntity --> 4ospi("LGAPOL01")
%% click 4ospi openCode "<SwmPath>[base/src/lgapol01.cbl](base/src/lgapol01.cbl)</SwmPath>:1"
%%   4ospi("LGAPOL01") --> i3gyi("LGAPDB01")
%% click i3gyi openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%   i3gyi("LGAPDB01") --> uio94("LGAPDB02")
%% click uio94 openCode "<SwmPath>[base/src/LGAPDB02.cbl](base/src/LGAPDB02.cbl)</SwmPath>:1"
%%   
%%   
%% i3gyi("LGAPDB01") --> eb9q4("LGAPDB03")
%% click eb9q4 openCode "<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>:1"
%%   
%%   
%% i3gyi("LGAPDB01") --> gxdg2("LGAPDB04")
%% click gxdg2 openCode "<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% 4ospi("LGAPOL01") --> xrlaq("LGSTSQ")
%% click xrlaq openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% 0hr7k("(LGTESTP4) Managing commercial policy transactions"):::currentEntity --> i4hoq("LGDPOL01")
%% click i4hoq openCode "<SwmPath>[base/src/lgdpol01.cbl](base/src/lgdpol01.cbl)</SwmPath>:1"
%%   i4hoq("LGDPOL01") --> ql57r("LGDPDB01")
%% click ql57r openCode "<SwmPath>[base/src/lgdpdb01.cbl](base/src/lgdpdb01.cbl)</SwmPath>:1"
%%   ql57r("LGDPDB01") --> tcqr9("LGDPVS01")
%% click tcqr9 openCode "<SwmPath>[base/src/lgdpvs01.cbl](base/src/lgdpvs01.cbl)</SwmPath>:1"
%%   tcqr9("LGDPVS01") --> f39pp("LGSTSQ")
%% click f39pp openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% ql57r("LGDPDB01") --> g54cy("LGSTSQ")
%% click g54cy openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% i4hoq("LGDPOL01") --> a6px0("LGSTSQ")
%% click a6px0 openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% 0hr7k("(LGTESTP4) Managing commercial policy transactions"):::currentEntity --> teiar("SSP4")
%%   
%%   
%%   
%% click 0hr7k openCode "<SwmPath>[base/src/lgtestp4.cbl](base/src/lgtestp4.cbl)</SwmPath>:1"
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
