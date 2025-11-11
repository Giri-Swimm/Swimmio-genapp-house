---
title: Managing Status Counters and Return Codes (LGASTAT1) - Overview
---
# Overview

This flow processes transaction requests by reading and validating input data, normalizing transaction identifiers, and updating transaction counters. If input data is missing, fallback values and queue initialization are used to ensure all transaction data is standardized and tracked.

## Dependencies

### Program

- LGASTAT1 (<SwmPath>[base/src/lgastat1.cbl](base/src/lgastat1.cbl)</SwmPath>)

### Copybook

- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
