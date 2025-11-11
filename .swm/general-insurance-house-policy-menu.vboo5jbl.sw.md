---
title: General Insurance House Policy Menu
---
The House Policy Menu screen allows users to inquire about, add, update, or delete house insurance policies by entering or editing policy and property details. It serves as the main entry point for house policy transactions in the system.

## Screen Preview

```
SSP3        General Insurance House Policy Menu

    1. Policy Inquiry 
    2. Policy Add     
    3. Policy Delete  
    4. Policy Update  

                              Policy Number  ____________
                              Cust Number    ____________
                              Issue date     ____________ (yyyy-mm-dd)
                              Expiry date    ____________ (yyyy-mm-dd)
                              Property Type  ________________
                              Bedrooms       ___
                              House Value    __________
                              House Name     ____________________
                              House Number   ____
                              Postcode       ________

        Select Option _


        [                                        ]

ENTER=Continue  F3=Back  CLEAR=Clear
```

## Fields

### Policy Number (ENP3PNO)

- Length: 10 digits
- Right-justified, zero-filled
- Editable by user
- Used to identify the house policy uniquely
- No explicit validation in BMS, but program expects a valid policy number for inquiry, update, or delete.

### Cust Number (ENP3CNO)

- Length: 10 digits
- Right-justified, zero-filled
- Editable by user
- Used to identify the customer uniquely
- Required for all operations
- No explicit validation in BMS, but program expects a valid customer number.

### Issue date (ENP3IDA)

- Length: 10 characters
- Editable by user
- Expected format: yyyy-mm-dd
- No explicit validation in BMS, but program expects a valid date.

### Expiry date (ENP3EDA)

- Length: 10 characters
- Editable by user
- Expected format: yyyy-mm-dd
- No explicit validation in BMS, but program expects a valid date.

### Property Type (ENP3TYP)

- Length: 15 characters
- Editable by user
- Free text field for describing the property type
- No explicit validation in BMS or program.

### Bedrooms (ENP3BED)

- Length: 3 digits
- Right-justified, zero-filled
- Editable by user
- Represents the number of bedrooms
- No explicit validation in BMS or program.

### House Value (ENP3VAL)

- Length: 8 digits
- Right-justified, zero-filled
- Editable by user
- Represents the value of the house
- No explicit validation in BMS or program.

### House Name (ENP3HNM)

- Length: 20 characters
- Editable by user
- Free text field for the house name
- No explicit validation in BMS or program.

### House Number (ENP3HNO)

- Length: 4 characters
- Editable by user
- Free text field for the house number
- No explicit validation in BMS or program.

### Postcode (ENP3HPC)

- Length: 8 characters
- Editable by user
- Free text field for the postcode
- No explicit validation in BMS or program.

### Select Option (ENP3OPT)

- Length: 1 digit
- Numeric only
- Required field (MUSTENTER)
- Determines the menu action: 1=Inquiry, 2=Add, 3=Delete, 4=Update
- If invalid, error message is shown and cursor is placed on this field.

### Error/Status Message (ERP3FLD)

- Length: 40 characters
- Output only (protected)
- Used to display error or status messages to the user
- Populated by the program based on operation result.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
