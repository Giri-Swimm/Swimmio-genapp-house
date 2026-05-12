---
title: General Insurance House Policy Menu
---
The House Policy Menu screen allows users to inquire, add, delete, or update house insurance policies by entering or modifying policy and property details. It serves as the main interface for managing house policy records in the system.

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
                              House Value    ________
                              House Name     _____________________
                              House Number   ____
                              Postcode       ________

        Select Option _



        ________________________________________

ENTER=Continue  F3=Back  F4=Clear
```

## Fields

### Policy Number (ENP3PNO)

- Length: 10 characters, right-justified, zero-filled if numeric.
- Editable by the user.
- Used for identifying the house policy.
- Required for inquiry, update, and delete operations.

### Cust Number (ENP3CNO)

- Length: 10 characters, right-justified, zero-filled if numeric.
- Editable by the user.
- Used for identifying the customer.
- Required for all operations.

### Issue date (ENP3IDA)

- Length: 10 characters.
- Editable by the user.
- Format: yyyy-mm-dd.
- Used for policy start date.
- Validated for correct date format in COBOL logic.

### Expiry date (ENP3EDA)

- Length: 10 characters.
- Editable by the user.
- Format: yyyy-mm-dd.
- Used for policy end date.
- Validated for correct date format in COBOL logic.

### Property Type (ENP3TYP)

- Length: 15 characters.
- Editable by the user.
- Used to describe the type of property (e.g., Detached, Flat, etc.).

### Bedrooms (ENP3BED)

- Length: 3 digits, right-justified, zero-filled if numeric.
- Editable by the user.
- Represents the number of bedrooms in the property.

### House Value (ENP3VAL)

- Length: 8 digits, right-justified, zero-filled if numeric.
- Editable by the user.
- Represents the value of the house.

### House Name (ENP3HNM)

- Length: 20 characters.
- Editable by the user.
- Used for the name of the house (if applicable).

### House Number (ENP3HNO)

- Length: 4 characters.
- Editable by the user.
- Used for the house number.

### Postcode (ENP3HPC)

- Length: 8 characters.
- Editable by the user.
- Used for the postcode of the property.

### Select Option (ENP3OPT)

- Length: 1 digit, numeric only.
- Editable by the user.
- Must be entered (validated as MUSTENTER).
- Options: 1=Inquiry, 2=Add, 3=Delete, 4=Update.
- If invalid, error message is shown.

### Error/Status Message Area (ERP3FLD)

- Length: 40 characters.
- Display only (protected field).
- Used to show error or status messages (e.g., 'House Policy Updated', 'No data was returned.').

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
