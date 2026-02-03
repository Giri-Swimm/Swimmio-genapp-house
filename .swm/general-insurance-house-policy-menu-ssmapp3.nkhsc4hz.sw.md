---
title: General Insurance House Policy Menu (SSMAPP3)
---
The House Policy Menu screen (SSMAPP3) provides users with a structured interface to inquire, add, delete, or update house insurance policies. It collects and displays all relevant policy and property details, guiding users through the main operations for house insurance management.

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
         House Value        ________
         House Name         ____________________
         House Number       ____
         Postcode           ________

    Select Option   _


[Error/Status Message Area]

ENTER=Continue  PF3=End  CLEAR=Clear
```

## Fields

### Policy Number (ENP3PNO)

- Length: 10 digits
- Input field, right-justified, zero-filled
- Used for policy identification
- Required for most operations
- No explicit validation in BMS, but program expects valid numeric policy number

### Customer Number (ENP3CNO)

- Length: 10 digits
- Input field, right-justified, zero-filled
- Used for customer identification
- Required for most operations
- No explicit validation in BMS, but program expects valid numeric customer number

### Issue Date (ENP3IDA)

- Length: 10 characters
- Input field
- Format: yyyy-mm-dd
- No explicit validation in BMS, but program expects valid date format

### Expiry Date (ENP3EDA)

- Length: 10 characters
- Input field
- Format: yyyy-mm-dd
- No explicit validation in BMS, but program expects valid date format

### Property Type (ENP3TYP)

- Length: 15 characters
- Input field
- Used to describe the type of property insured
- No explicit validation in BMS or program

### Bedrooms (ENP3BED)

- Length: 3 digits
- Input field, right-justified, zero-filled
- Used to specify number of bedrooms
- No explicit validation in BMS or program

### House Value (ENP3VAL)

- Length: 8 digits
- Input field, right-justified, zero-filled
- Used to specify the value of the house
- No explicit validation in BMS or program

### House Name (ENP3HNM)

- Length: 20 characters
- Input field
- Used to specify the name of the house
- No explicit validation in BMS or program

### House Number (ENP3HNO)

- Length: 4 characters
- Input field
- Used to specify the house number
- No explicit validation in BMS or program

### Postcode (ENP3HPC)

- Length: 8 characters
- Input field
- Used to specify the postcode
- No explicit validation in BMS or program

### Select Option (ENP3OPT)

- Length: 1 digit
- Input field, numeric only
- Must be entered (MUSTENTER validation)
- Used to select menu option (1-4)
- Program expects valid option, otherwise displays error

### Error/Status Message Area (ERP3FLD)

- Length: 40 characters
- Output only
- Used to display error or status messages
- Populated by program logic
- Not editable by user

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
