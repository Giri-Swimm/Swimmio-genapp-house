---
title: General Insurance Customer Menu
---
The General Insurance Customer Menu screen provides a central interface for customer-related operations, allowing users to inquire, add, or update customer details. It collects and displays all relevant customer information and guides the user through the available actions.

## Screen Preview

```
SSC1        General Insurance Customer Menu

        1. Cust Inquiry 
        2. Cust Add     
        
        4. Cust Update  

              Cust Number      ____________
              Cust Name :First ____________
                        :Last  ____________________
              DOB         ____________ (yyyy-mm-dd)
              House Name  ____________________
              House Number ____
              Postcode    ________
              Phone: Home ____________________
              Phone: Mob  ____________________
              Email  Addr ____________________________

        Select Option _


        ________________________________________

ENTER=Continue  PF3=Exit  CLEAR=Clear
```

## Fields

### Cust Number (ENT1CNO)

- Length: 10 digits
- Right-justified, zero-filled
- Used for inquiry and update operations
- Required for options 1 (Inquiry) and 4 (Update)
- Numeric only
- When adding a customer (option 2), this field is not entered by the user.

### First Name (ENT1FNA)

- Length: 10 characters
- Free-form text
- Used for add and update operations
- No explicit validation in code, but likely expected to be alphabetic
- Required for add (option 2) and update (option 4)

### Last Name (ENT1LNA)

- Length: 20 characters
- Free-form text
- Used for add and update operations
- No explicit validation in code, but likely expected to be alphabetic
- Required for add (option 2) and update (option 4)

### Date of Birth (ENT1DOB)

- Length: 10 characters
- Format: yyyy-mm-dd (as indicated on screen)
- Used for add and update operations
- No explicit validation in code, but format is expected
- Required for add (option 2) and update (option 4)

### House Name (ENT1HNM)

- Length: 20 characters
- Free-form text
- Used for add and update operations
- No explicit validation in code
- Required for add (option 2) and update (option 4)

### House Number (ENT1HNO)

- Length: 4 characters
- Free-form text (could be numeric or alphanumeric)
- Used for add and update operations
- No explicit validation in code
- Required for add (option 2) and update (option 4)

### Postcode (ENT1HPC)

- Length: 8 characters
- Free-form text
- Converted to uppercase before being sent to backend
- Used for add and update operations
- Required for add (option 2) and update (option 4)

### Phone: Home (ENT1HP1)

- Length: 20 characters
- Free-form text
- Used for add and update operations
- No explicit validation in code
- Required for add (option 2) and update (option 4)

### Phone: Mob (ENT1HP2)

- Length: 20 characters
- Free-form text
- Used for add and update operations
- No explicit validation in code
- Required for add (option 2) and update (option 4)

### Email Addr (ENT1HMO)

- Length: 27 characters (screen), 100 in backend
- Free-form text
- Used for add and update operations
- No explicit validation in code
- Required for add (option 2) and update (option 4)

### Select Option (ENT1OPT)

- Length: 1 character
- Numeric only (1, 2, or 4)
- Must be entered (MUSTENTER)
- Determines which operation is performed:
  - 1: Customer Inquiry
  - 2: Customer Add
  - 4: Customer Update
- Any other value triggers an error message

### Error/Status Message Area (ERRFLD)

- Length: 40 characters
- Used to display error or status messages (e.g., 'No data was returned.', 'Customer details updated', etc.)
- Read-only for user
- Blank if no message

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
