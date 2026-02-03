---
title: General Insurance Customer Menu
---
The General Insurance Customer Menu screen provides a central interface for customer-related operations, including inquiry, addition, and update of customer records. It collects and displays customer details and guides the user through the available actions.

## Screen Preview

```
    SSC1        General Insurance Customer Menu

        1. Cust Inquiry 
        2. Cust Add     
        
        4. Cust Update  

              Cust Number      __________
              Cust Name :First __________
                        :Last  ____________________
              DOB              __________ (yyyy-mm-dd)
              House Name       ____________________
              House Number     ____
              Postcode         ________
              Phone: Home      ____________________
              Phone: Mob       ____________________
              Email  Addr      ____________________________

        Select Option _


        [                                        ]

ENTER=Continue  F3=Exit  CLEAR=Clear
```

## Fields

### Cust Number (ENT1CNO)

- Length: 10 digits (right-justified, zero-filled if needed)
- Used for customer identification in inquiry and update operations
- Editable only for inquiry/update; auto-filled after add
- No explicit validation in BMS, but must be numeric in COBOL logic

### First Name (ENT1FNA)

- Length: 10 characters
- Free text input
- Required for add operation
- No explicit validation in BMS or COBOL, but used in add/update logic

### Last Name (ENT1LNA)

- Length: 20 characters
- Free text input
- Required for add operation
- No explicit validation in BMS or COBOL, but used in add/update logic

### Date of Birth (ENT1DOB)

- Length: 10 characters
- Format: yyyy-mm-dd (as indicated on screen)
- Free text input
- No explicit validation in BMS or COBOL, but expected to be a date

### House Name (ENT1HNM)

- Length: 20 characters
- Free text input
- Used in add/update logic

### House Number (ENT1HNO)

- Length: 4 characters
- Free text input
- Used in add/update logic

### Postcode (ENT1HPC)

- Length: 8 characters
- Free text input
- Converted to uppercase in COBOL before processing
- Used in add/update logic

### Phone: Home (ENT1HP1)

- Length: 20 characters
- Free text input
- Used in add/update logic

### Phone: Mob (ENT1HP2)

- Length: 20 characters
- Free text input
- Used in add/update logic

### Email Addr (ENT1HMO)

- Length: 27 characters (BMS), but only 100 in commarea
- Free text input
- Used in add/update logic

### Select Option (ENT1OPT)

- Length: 1 character
- Numeric only (BMS: NUM, MUSTENTER)
- Required field
- Options: 1 (Inquiry), 2 (Add), 4 (Update)
- Error message shown if invalid

### Error/Status Message (ERRFLD)

- Length: 40 characters
- Output only (protected)
- Used to display error or status messages (e.g., 'No data was returned.', 'Customer details updated', etc.)

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
