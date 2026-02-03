---
title: House Policy Menu Screen (SSMAPP3)
---
The House Policy Menu screen (SSMAPP3) provides users with a structured interface to manage house insurance policies, including inquiry, addition, deletion, and update of policy details. It collects all relevant property and policy information and guides the user through available operations.

## Screen Preview

```
SSP3        General Insurance House Policy Menu

    1. Policy Inquiry 
    2. Policy Add     
    3. Policy Delete  
    4. Policy Update  

           Policy Number      ____________
           Cust Number        ____________
           Issue date         ____________ (yyyy-mm-dd)
           Expiry date        ____________ (yyyy-mm-dd)
           Property Type      ________________
           Bedrooms           ___
           House Value        __________
           House Name         ____________________
           House Number       ____
           Postcode           ________


    Select Option   _


[Message/Status Area]

ENTER=Continue  PF3=End  CLEAR=Clear
```

## Fields

### Policy Number (ENP3PNO)

- Length: 10 digits
- Numeric, right-justified, zero-filled
- Required for inquiry, add, delete, and update operations
- Input field, not fixed

### Cust Number (ENP3CNO)

- Length: 10 digits
- Numeric, right-justified, zero-filled
- Required for all operations
- Input field, not fixed

### Issue date (ENP3IDA)

- Length: 10 characters
- Format: yyyy-mm-dd
- Input field, not fixed
- No explicit validation in BMS, but program expects valid date

### Expiry date (ENP3EDA)

- Length: 10 characters
- Format: yyyy-mm-dd
- Input field, not fixed
- No explicit validation in BMS, but program expects valid date

### Property Type (ENP3TYP)

- Length: 15 characters
- Input field, not fixed
- No explicit validation in BMS or program

### Bedrooms (ENP3BED)

- Length: 3 digits
- Numeric, right-justified, zero-filled
- Input field, not fixed

### House Value (ENP3VAL)

- Length: 8 digits
- Numeric, right-justified, zero-filled
- Input field, not fixed

### House Name (ENP3HNM)

- Length: 20 characters
- Input field, not fixed

### House Number (ENP3HNO)

- Length: 4 characters
- Input field, not fixed

### Postcode (ENP3HPC)

- Length: 8 characters
- Input field, not fixed

### Select Option (ENP3OPT)

- Length: 1 digit
- Numeric only
- Must be entered (MUSTENTER validation)
- Options: 1=Inquiry, 2=Add, 3=Delete, 4=Update
- Input field, not fixed

### Message/Status Area (ERP3FLD)

- Length: 40 characters
- Output only, protected
- Used for error/status messages
- Populated by program logic

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
