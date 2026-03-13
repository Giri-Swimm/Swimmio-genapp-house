---
title: General Insurance Customer Menu (SSC1)
---
The General Insurance Customer Menu screen (SSC1) provides a central interface for customer-related operations, including inquiry, addition, and update of customer records. Users can enter or view customer details and select transaction options, with feedback provided in the status area.

## Screen Preview

```
SSC1        General Insurance Customer Menu

    1. Cust Inquiry 
    2. Cust Add     
                
    4. Cust Update  

          Cust Number      ____________
          Cust Name :First ____________
                    :Last  ______________________
          DOB              ____________ (yyyy-mm-dd)
          House Name       ______________________
          House Number     ____
          Postcode         ________
          Phone: Home      ______________________
          Phone: Mob       ______________________
          Email  Addr      _____________________________

        Select Option      _


        [Error/Status Message Area]

ENTER=Continue  PF3=Exit  CLEAR=Clear
```

## Fields

### Cust Number (ENT1CNO)

- Length: 10 characters
- Numeric input
- Used for customer identification in inquiry and update operations
- Required for options 1 (Inquiry) and 4 (Update)
- Initial value is '0000000000' for new customers (option 2)
- Validations: Numeric only, right-justified, zero-filled

### Cust Name :First (ENT1FNA)

- Length: 10 characters
- Alphanumeric input
- Used for customer first name
- Required for add and update operations
- No explicit validation in code, but must be filled for add/update

### Cust Name :Last (ENT1LNA)

- Length: 20 characters
- Alphanumeric input
- Used for customer last name
- Required for add and update operations
- No explicit validation in code, but must be filled for add/update

### DOB (ENT1DOB)

- Length: 10 characters
- Format: yyyy-mm-dd
- Used for customer date of birth
- Required for add and update operations
- No explicit validation in code, but must follow date format

### House Name (ENT1HNM)

- Length: 20 characters
- Alphanumeric input
- Used for customer house name
- Required for add and update operations
- No explicit validation in code

### House Number (ENT1HNO)

- Length: 4 characters
- Alphanumeric input
- Used for customer house number
- Required for add and update operations
- No explicit validation in code

### Postcode (ENT1HPC)

- Length: 8 characters
- Alphanumeric input
- Used for customer postcode
- Required for add and update operations
- Converted to uppercase before processing

### Phone: Home (ENT1HP1)

- Length: 20 characters
- Alphanumeric input
- Used for customer home phone number
- Required for add and update operations
- No explicit validation in code

### Phone: Mob (ENT1HP2)

- Length: 20 characters
- Alphanumeric input
- Used for customer mobile phone number
- Required for add and update operations
- No explicit validation in code

### Email Addr (ENT1HMO)

- Length: 27 characters
- Alphanumeric input
- Used for customer email address
- Required for add and update operations
- No explicit validation in code

### Select Option (ENT1OPT)

- Length: 1 character
- Numeric input
- Must be entered (VALIDN=MUSTENTER)
- Options: '1' (Inquiry), '2' (Add), '4' (Update)
- Validation: Only accepts valid menu options; error message shown for invalid input

### Error/Status Message Area (ERRFLD)

- Length: 40 characters
- Display-only field
- Used to show error or status messages (e.g., 'New Customer Inserted', 'Customer details updated', 'Error Adding Customer', etc.)
- Set by program logic based on operation outcome

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
