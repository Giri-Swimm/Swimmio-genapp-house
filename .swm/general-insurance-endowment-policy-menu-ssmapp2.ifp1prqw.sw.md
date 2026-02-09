---
title: General Insurance Endowment Policy Menu (SSMAPP2)
---
The Endowment Policy Menu screen allows users to inquire, add, delete, or update endowment insurance policies. It collects and displays all relevant policy details and guides the user through the available operations.

## Screen Preview

```
SSP2        General Insurance Endowment Policy Menu

        1. Policy Inquiry 
        2. Policy Add     
        3. Policy Delete  
        4. Policy Update  

              Policy Number     ____________
              Cust Number       ____________
              Issue date        ____________ (yyyy-mm-dd)
              Expiry date       ____________ (yyyy-mm-dd)
              Fund Name         ____________
              Term              __
              Sum Assured       ______
              Life Assured      __________________________
              With Profits      _
              Equities          _
              Managed Funds     _

        Select Option _


        [                                        ]

ENTER=Continue  F3=Back  F4=Clear
```

## Fields

### Policy Number (ENP2PNO)

- Length: 10 digits
- Input field, right-justified, zero-filled
- Used for identifying the endowment policy
- Required for inquiry, update, and delete operations

### Customer Number (ENP2CNO)

- Length: 10 digits
- Input field, right-justified, zero-filled
- Used for identifying the customer
- Required for all operations

### Issue Date (ENP2IDA)

- Length: 10 characters
- Input field
- Format: yyyy-mm-dd
- Used for add and update operations
- No explicit validation in code, but must be a valid date

### Expiry Date (ENP2EDA)

- Length: 10 characters
- Input field
- Format: yyyy-mm-dd
- Used for add and update operations
- No explicit validation in code, but must be a valid date

### Fund Name (ENP2FNM)

- Length: 10 characters
- Input field
- Used for add and update operations

### Term (ENP2TER)

- Length: 2 digits
- Input field
- Used for add and update operations
- Numeric

### Sum Assured (ENP2SUM)

- Length: 6 digits
- Input field, right-justified, zero-filled
- Used for add and update operations
- Numeric

### Life Assured (ENP2LIF)

- Length: 25 characters
- Input field
- Used for add and update operations

### With Profits (ENP2WPR)

- Length: 1 character
- Input field
- Used for add and update operations
- No explicit validation in code

### Equities (ENP2EQU)

- Length: 1 character
- Input field
- Used for add and update operations
- No explicit validation in code

### Managed Funds (ENP2MAN)

- Length: 1 character
- Input field
- Used for add and update operations
- No explicit validation in code

### Select Option (ENP2OPT)

- Length: 1 digit
- Input field, numeric, must be entered (MUSTENTER)
- Determines which operation to perform (1=Inquiry, 2=Add, 3=Delete, 4=Update)
- If invalid, error message is shown

### Error/Status Message (ERP2FLD)

- Length: 40 characters
- Output only
- Used to display error or status messages to the user
- Populated by the COBOL program based on operation result

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
