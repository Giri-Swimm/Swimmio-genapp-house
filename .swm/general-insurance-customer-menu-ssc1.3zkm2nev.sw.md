---
title: General Insurance Customer Menu (SSC1)
---
The General Insurance Customer Menu screen (SSC1) provides users with a central interface to inquire about, add, or update customer details for general insurance. It collects and displays key customer information and guides users through the main customer-related transactions.

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
        Email  Addr _____________________________

        Select Option _


        [Error/Status Message Area]

ENTER=Continue  PF3=Exit  CLEAR=Clear
```

## Fields

### Cust Number (ENT1CNO)

- Length: 10 digits
- Right-justified, zero-filled
- Used for customer lookup and update
- Required for Inquiry (option 1) and Update (option 4)
- For Add (option 2), system assigns value
- Numeric only

### Cust Name :First (ENT1FNA)

- Length: 10 characters
- Free text
- Required for Add (option 2) and Update (option 4)
- No explicit validation in code

### Cust Name :Last (ENT1LNA)

- Length: 20 characters
- Free text
- Required for Add (option 2) and Update (option 4)
- No explicit validation in code

### DOB (ENT1DOB)

- Length: 10 characters
- Format: yyyy-mm-dd (not enforced in code, but shown on screen)
- Required for Add (option 2) and Update (option 4)
- No explicit validation in code

### House Name (ENT1HNM)

- Length: 20 characters
- Free text
- Required for Add (option 2) and Update (option 4)
- No explicit validation in code

### House Number (ENT1HNO)

- Length: 4 characters
- Free text
- Required for Add (option 2) and Update (option 4)
- No explicit validation in code

### Postcode (ENT1HPC)

- Length: 8 characters
- Free text
- Required for Add (option 2) and Update (option 4)
- Converted to uppercase before processing
- No other explicit validation in code

### Phone: Home (ENT1HP1)

- Length: 20 characters
- Free text
- Required for Add (option 2) and Update (option 4)
- No explicit validation in code

### Phone: Mob (ENT1HP2)

- Length: 20 characters
- Free text
- Required for Add (option 2) and Update (option 4)
- No explicit validation in code

### Email Addr (ENT1HMO)

- Length: 27 characters (screen), up to 100 in commarea
- Free text
- Required for Add (option 2) and Update (option 4)
- No explicit validation in code

### Select Option (ENT1OPT)

- Length: 1 character
- Numeric only
- Must be entered (MUSTENTER)
- Valid options: '1' (Inquiry), '2' (Add), '4' (Update)
- Error message shown for invalid option

### Error/Status Message Area (ERRFLD)

- Length: 40 characters
- Display only
- Used to show status or error messages (e.g., 'New Customer Inserted', 'Customer details updated', 'Error Adding Customer', etc.)
- Not editable by user

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
