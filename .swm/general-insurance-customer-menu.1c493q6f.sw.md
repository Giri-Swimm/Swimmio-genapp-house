---
title: General Insurance Customer Menu
---
The General Insurance Customer Menu screen allows users to inquire about, add, or update customer information in the insurance system. It provides fields for customer details and guides the user through the available customer-related transactions.

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
              House Name  ____________________
              House Number ____
              Postcode    ________
              Phone: Home ____________________
              Phone: Mob  ____________________
              Email  Addr ____________________________

        Select Option _

        [                                        ]

ENTER=Continue  F3=Exit  CLEAR=Clear
```

## Fields

### Cust Number (ENT1CNO)

- Length: 10 digits
- Right-justified, zero-filled
- Used for customer lookup (inquiry/update)
- Required for inquiry/update, not for add
- Numeric only

### First Name (ENT1FNA)

- Length: 10 characters
- Free text input
- Used for add/update
- No explicit validation in code

### Last Name (ENT1LNA)

- Length: 20 characters
- Free text input
- Used for add/update
- No explicit validation in code

### Date of Birth (ENT1DOB)

- Length: 10 characters
- Format: yyyy-mm-dd (as shown on screen)
- Used for add/update
- No explicit validation in code, but format is suggested

### House Name (ENT1HNM)

- Length: 20 characters
- Free text input
- Used for add/update
- No explicit validation in code

### House Number (ENT1HNO)

- Length: 4 characters
- Free text input
- Used for add/update
- No explicit validation in code

### Postcode (ENT1HPC)

- Length: 8 characters
- Free text input
- Used for add/update
- Converted to uppercase before processing

### Phone: Home (ENT1HP1)

- Length: 20 characters
- Free text input
- Used for add/update
- No explicit validation in code

### Phone: Mob (ENT1HP2)

- Length: 20 characters
- Free text input
- Used for add/update
- No explicit validation in code

### Email Addr (ENT1HMO)

- Length: 27 characters (screen), 100 in commarea
- Free text input
- Used for add/update
- No explicit validation in code

### Select Option (ENT1OPT)

- Length: 1 character
- Numeric only
- Required (MUSTENTER)
- Options: '1' (Inquiry), '2' (Add), '4' (Update)
- Any other value triggers error message

### Error/Status Message (ERRFLD)

- Length: 40 characters
- Output only (protected)
- Used to display error or status messages (e.g., 'No data was returned.', 'Error Adding Customer', etc.)

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
