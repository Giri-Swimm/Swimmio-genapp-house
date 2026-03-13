---
title: LGTESTP4 - Insurance Policy Management - Overview
---
# Overview

This document explains the flow for managing insurance policy inquiries, additions, and deletions. Users interact with a unified interface to submit requests, which are validated and routed to backend handlers. The system processes each request and responds with policy details, confirmation messages, or error notifications.

```mermaid
flowchart TD
  node1["Entry Point and Initial Input Check"]:::HeadingStyle
  click node1 goToHeading "Entry Point and Initial Input Check"
  node1 --> node2["User Request Handling and Dispatch"]:::HeadingStyle
  click node2 goToHeading "User Request Handling and Dispatch"
  node2 --> node3{"Action: Inquiry, Add, Delete?"}
  node3 -->|"Inquiry"| node4["Policy Inquiry Backend Dispatch"]:::HeadingStyle
  click node4 goToHeading "Policy Inquiry Backend Dispatch"
  node4 --> node5["Policy Data Fetch and Routing"]:::HeadingStyle
  click node5 goToHeading "Policy Data Fetch and Routing"
  node5 --> node6{"Was backend operation successful?"}
  node6 -->|"Yes"| node7["UI Update After Successful Inquiry"]:::HeadingStyle
  click node7 goToHeading "UI Update After Successful Inquiry"
  node6 -->|"No"| node8["Post-Inquiry Error Handling"]:::HeadingStyle
  click node8 goToHeading "Post-Inquiry Error Handling"
  node8 --> node7
  node3 -->|"Add"| node9["Policy Application Processing Workflow"]:::HeadingStyle
  click node9 goToHeading "Policy Application Processing Workflow"
  node9 --> node10{"Was add operation successful?"}
  node10 -->|"Yes"| node7
  node10 -->|"No"| node11["Post-Delete Error Handling and UI Update"]:::HeadingStyle
  click node11 goToHeading "Post-Delete Error Handling and UI Update"
  node11 --> node7
  node3 -->|"Delete"| node11

classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Programs

- LGTESTP4 (<SwmPath>[base/src/lgtestp4.cbl](base/src/lgtestp4.cbl)</SwmPath>)
- LGIPOL01 (<SwmPath>[base/src/lgipol01.cbl](base/src/lgipol01.cbl)</SwmPath>)
- LGIPDB01 (<SwmPath>[base/src/lgipdb01.cbl](base/src/lgipdb01.cbl)</SwmPath>)
- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)
- LGAPOL01 (<SwmPath>[base/src/lgapol01.cbl](base/src/lgapol01.cbl)</SwmPath>)
- LGAPDB01 (<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>)
- LGAPDB02 (<SwmPath>[base/src/LGAPDB02.cbl](base/src/LGAPDB02.cbl)</SwmPath>)
- LGAPDB03 (<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>)
- LGAPDB04 (<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>)
- LGDPOL01 (<SwmPath>[base/src/lgdpol01.cbl](base/src/lgdpol01.cbl)</SwmPath>)
- LGDPDB01 (<SwmPath>[base/src/lgdpdb01.cbl](base/src/lgdpdb01.cbl)</SwmPath>)
- LGDPVS01 (<SwmPath>[base/src/lgdpvs01.cbl](base/src/lgdpvs01.cbl)</SwmPath>)
- SSP4

### Copybooks

- SQLCA
- LGPOLICY (<SwmPath>[base/src/lgpolicy.cpy](base/src/lgpolicy.cpy)</SwmPath>)
- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)
- INPUTREC2 (<SwmPath>[base/src/INPUTREC2.cpy](base/src/INPUTREC2.cpy)</SwmPath>)
- OUTPUTREC (<SwmPath>[base/src/OUTPUTREC.cpy](base/src/OUTPUTREC.cpy)</SwmPath>)
- WORKSTOR (<SwmPath>[base/src/WORKSTOR.cpy](base/src/WORKSTOR.cpy)</SwmPath>)
- LGAPACT (<SwmPath>[base/src/LGAPACT.cpy](base/src/LGAPACT.cpy)</SwmPath>)
- XMAP

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
