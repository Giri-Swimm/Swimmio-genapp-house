---
title: Insurance Policy Premium Calculation Job (LGAPJOB) - Dependencies
---
# Dependencies

```mermaid
graph TD
  
  eea0h("Insurance Policy Premium Calculation Job (LGAPJOB)"):::currentEntity --> 94uda("Calculating Enhanced Insurance Premiums (LGAPDB01)")
click 94uda openCode "base/src/LGAPDB01.cbl:1"
  94uda("Calculating Enhanced Insurance Premiums (LGAPDB01)") --> htcef("LGAPDB02")
click htcef openCode "base/src/LGAPDB02.cbl:1"
  
  
94uda("Calculating Enhanced Insurance Premiums (LGAPDB01)") --> z48un("LGAPDB03")
click z48un openCode "base/src/LGAPDB03.cbl:1"
  
  
94uda("Calculating Enhanced Insurance Premiums (LGAPDB01)") --> rbg6d("LGAPDB04")
click rbg6d openCode "base/src/LGAPDB04.cbl:1"
  
  
  
eea0h("Insurance Policy Premium Calculation Job (LGAPJOB)"):::currentEntity --> 1d8g4("Daily Premium Summary Report Generator (LGAPRPT1)")
click 1d8g4 openCode "base/src/LGAPRPT1.cbl:1"
  
  
  
click eea0h openCode "base/cntl/lgapjob.jcl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   
%%   eea0h("Insurance Policy Premium Calculation Job (LGAPJOB)"):::currentEntity --> 94uda("Calculating Enhanced Insurance Premiums (LGAPDB01)")
%% click 94uda openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%   94uda("Calculating Enhanced Insurance Premiums (LGAPDB01)") --> htcef("LGAPDB02")
%% click htcef openCode "<SwmPath>[base/src/LGAPDB02.cbl](base/src/LGAPDB02.cbl)</SwmPath>:1"
%%   
%%   
%% 94uda("Calculating Enhanced Insurance Premiums (LGAPDB01)") --> z48un("LGAPDB03")
%% click z48un openCode "<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>:1"
%%   
%%   
%% 94uda("Calculating Enhanced Insurance Premiums (LGAPDB01)") --> rbg6d("LGAPDB04")
%% click rbg6d openCode "<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% eea0h("Insurance Policy Premium Calculation Job (LGAPJOB)"):::currentEntity --> 1d8g4("Daily Premium Summary Report Generator (LGAPRPT1)")
%% click 1d8g4 openCode "<SwmPath>[base/src/LGAPRPT1.cbl](base/src/LGAPRPT1.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% click eea0h openCode "<SwmPath>[base/cntl/lgapjob.jcl](base/cntl/lgapjob.jcl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

## Paths

<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>

<SwmPath>[base/src/LGAPDB02.cbl](base/src/LGAPDB02.cbl)</SwmPath>

<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>

<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>

<SwmPath>[base/src/INPUTREC2.cpy](base/src/INPUTREC2.cpy)</SwmPath>

<SwmPath>[base/src/OUTPUTREC.cpy](base/src/OUTPUTREC.cpy)</SwmPath>

<SwmPath>[base/src/WORKSTOR.cpy](base/src/WORKSTOR.cpy)</SwmPath>

<SwmPath>[base/src/LGAPACT.cpy](base/src/LGAPACT.cpy)</SwmPath>

<SwmPath>[base/src/LGAPRPT1.cbl](base/src/LGAPRPT1.cbl)</SwmPath>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
