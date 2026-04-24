---
title: General Insurance Endowment Policy Menu
---
The Endowment Policy Menu screen allows users to inquire, add, delete, or update endowment insurance policies by entering or editing policy and customer details. It serves as the main interface for managing endowment policy records within the insurance application.

## Screen Preview

```
SSP2        General Insurance Endowment Policy Menu

    1. Policy Inquiry 
    2. Policy Add     
    3. Policy Delete  
    4. Policy Update  

                             Policy Number __________
                             Cust Number  __________
                             Issue date   __________ (yyyy-mm-dd)
                             Expiry date  __________ (yyyy-mm-dd)
                             Fund Name    __________
                             Term         __
                             Sum Assured  ______
                             Life Assured __________________________
                             With Profits _
                             Equities     _
                             Managed Funds_

        Select Option _


        [                                        ]

ENTER=Continue  PF3=Exit  CLEAR=Clear
```

## Fields

### Policy Number (ENP2PNO)

- Length: 10 digits
- Right-justified, zero-filled
- Editable by user
- Used for identifying the endowment policy
- Required for inquiry, update, and delete operations

### Cust Number (ENP2CNO)

- Length: 10 digits
- Right-justified, zero-filled
- Editable by user
- Used for identifying the customer
- Required for all operations

### Issue date (ENP2IDA)

- Length: 10 characters
- Format: yyyy-mm-dd
- Editable by user
- Used for policy creation and updates
- No explicit validation in code, but must be a valid date

### Expiry date (ENP2EDA)

- Length: 10 characters
- Format: yyyy-mm-dd
- Editable by user
- Used for policy creation and updates
- No explicit validation in code, but must be a valid date

### Fund Name (ENP2FNM)

- Length: 10 characters
- Editable by user
- Used for endowment policy details

### Term (ENP2TER)

- Length: 2 digits
- Editable by user
- Used for endowment policy details
- No explicit validation in code

### Sum Assured (ENP2SUM)

- Length: 6 digits
- Right-justified, zero-filled
- Editable by user
- Used for endowment policy details

### Life Assured (ENP2LIF)

- Length: 25 characters
- Editable by user
- Used for endowment policy details

### With Profits (ENP2WPR)

- Length: 1 character
- Editable by user
- Used for endowment policy details
- No explicit validation in code

### Equities (ENP2EQU)

- Length: 1 character
- Editable by user
- Used for endowment policy details
- No explicit validation in code

### Managed Funds (ENP2MAN)

- Length: 1 character
- Editable by user
- Used for endowment policy details
- No explicit validation in code

### Select Option (ENP2OPT)

- Length: 1 digit
- Numeric only
- Required field (MUSTENTER)
- Determines which operation to perform: 1=Inquiry, 2=Add, 3=Delete, 4=Update
- If invalid, error message is shown and cursor is returned to this field

### Error/Status Message (ERP2FLD)

- Length: 40 characters
- Display only
- Used to show error or status messages (e.g., 'No data was returned.', 'Life Policy Updated')
- Not editable by user

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
