---
title: Calculating Enhanced Insurance Premiums (LGAPDB01) - Overview
---
# Overview

## Dependencies

### Programs

- LGAPDB01 (<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>)
- LGAPDB02 (<SwmPath>[base/src/LGAPDB02.cbl](base/src/LGAPDB02.cbl)</SwmPath>)
- LGAPDB03 (<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>)
- LGAPDB04 (<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>)

### Copybooks

- SQLCA
- INPUTREC2 (<SwmPath>[base/src/INPUTREC2.cpy](base/src/INPUTREC2.cpy)</SwmPath>)
- OUTPUTREC (<SwmPath>[base/src/OUTPUTREC.cpy](base/src/OUTPUTREC.cpy)</SwmPath>)
- WORKSTOR (<SwmPath>[base/src/WORKSTOR.cpy](base/src/WORKSTOR.cpy)</SwmPath>)
- LGAPACT (<SwmPath>[base/src/LGAPACT.cpy](base/src/LGAPACT.cpy)</SwmPath>)

# Where is this program used?

This program is used multiple times in the codebase as represented in the following diagram:

```mermaid
graph TD
  qxcdb("Insurance Policy Premium Calculation Job (LGAPJOB)") --> rurva("Calculating Enhanced Insurance Premiums (LGAPDB01)"):::currentEntity
click qxcdb openCode "base/cntl/lgapjob.jcl:1"
0r3uy("Processing Policy Data and Error Logging (LGAPOL01)") --> rurva("Calculating Enhanced Insurance Premiums (LGAPDB01)"):::currentEntity
click 0r3uy openCode "base/src/lgapol01.cbl:1"
  
  
click rurva openCode "base/src/LGAPDB01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   qxcdb("Insurance Policy Premium Calculation Job (LGAPJOB)") --> rurva("Calculating Enhanced Insurance Premiums (LGAPDB01)"):::currentEntity
%% click qxcdb openCode "<SwmPath>[base/cntl/lgapjob.jcl](base/cntl/lgapjob.jcl)</SwmPath>:1"
%% 0r3uy("Processing Policy Data and Error Logging (LGAPOL01)") --> rurva("Calculating Enhanced Insurance Premiums (LGAPDB01)"):::currentEntity
%% click 0r3uy openCode "<SwmPath>[base/src/lgapol01.cbl](base/src/lgapol01.cbl)</SwmPath>:1"
%%   
%%   
%% click rurva openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
