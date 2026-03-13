---
title: General Insurance Customer Menu
---
The General Insurance Customer Menu screen allows users to inquire, add, or update customer information. It provides fields for customer details and guides the user through the available customer-related transactions.

## Screen Preview

```
SSC1        General Insurance Customer Menu

        1. Cust Inquiry 
        2. Cust Add     
        
        4. Cust Update  

              Cust Number      __________
              Cust Name :First __________
                        :Last  ____________________
              DOB         __________ (yyyy-mm-dd)
              House Name  _____________________
              House Number ____
              Postcode    ________
              Phone: Home _____________________
              Phone: Mob  _____________________
              Email  Addr _____________________________

        Select Option _


        ________________________________________

ENTER=Continue  PF3=Exit  CLEAR=Clear
```

## Fields

### Cust Number (ENT1CNO)

- Length: 10 digits
- Right-justified, zero-filled
- Used for customer identification in inquiry and update
- Editable only for inquiry and update, not for add
- No explicit validation in the provided code, but must be numeric

### First Name (ENT1FNA)

- Length: 10 characters
- Free text input
- Used for add and update operations
- No explicit validation in the provided code

### Last Name (ENT1LNA)

- Length: 20 characters
- Free text input
- Used for add and update operations
- No explicit validation in the provided code

### Date of Birth (ENT1DOB)

- Length: 10 characters
- Format: yyyy-mm-dd (as indicated on screen)
- Used for add and update operations
- No explicit validation in the provided code

### House Name (ENT1HNM)

- Length: 20 characters
- Free text input
- Used for add and update operations
- No explicit validation in the provided code

### House Number (ENT1HNO)

- Length: 4 characters
- Free text input
- Used for add and update operations
- No explicit validation in the provided code

### Postcode (ENT1HPC)

- Length: 8 characters
- Free text input
- Used for add and update operations
- Converted to uppercase before processing
- No explicit validation in the provided code

### Phone: Home (ENT1HP1)

- Length: 20 characters
- Free text input
- Used for add and update operations
- No explicit validation in the provided code

### Phone: Mob (ENT1HP2)

- Length: 20 characters
- Free text input
- Used for add and update operations
- No explicit validation in the provided code

### Email Addr (ENT1HMO)

- Length: 27 characters
- Free text input
- Used for add and update operations
- No explicit validation in the provided code

### Select Option (ENT1OPT)

- Length: 1 digit
- Must be entered (MUSTENTER)
- Numeric only
- Options: 1 (Inquiry), 2 (Add), 4 (Update)
- If invalid, error message is shown and cursor is placed here

### Error/Status Message Area (ERRFLD)

- Length: 40 characters
- Display only
- Used to show error or status messages (e.g., 'No data was returned.', 'Error Adding Customer', etc.)
- Not editable

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
