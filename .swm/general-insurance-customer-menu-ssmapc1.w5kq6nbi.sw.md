---
title: General Insurance Customer Menu (SSMAPC1)
---
The General Insurance Customer Menu screen (SSMAPC1) provides users with options to inquire, add, or update customer details. It serves as the main entry point for customer-related transactions in the insurance application.

## Screen Preview

```
SSC1        General Insurance Customer Menu

    1. Cust Inquiry 
    2. Cust Add     
                    
    4. Cust Update  

         Cust Number         ____________
         Cust Name :First    ____________
                   :Last     ____________________
         DOB                 ____________ (yyyy-mm-dd)
         House Name          ____________________
         House Number        ____
         Postcode            ________
         Phone: Home         ____________________
         Phone: Mob          ____________________
         Email  Addr         ___________________________

        Select Option        _



        [Error/Status Message Area]

ENTER=Continue  F3=Back  F4=Clear
```

## Fields

### Cust Number (ENT1CNO)

- Length: 10 digits
- Right-justified, zero-filled
- Used for customer lookup and update
- Required for Inquiry and Update options
- No explicit validation in BMS, but program expects numeric input

### Cust Name :First (ENT1FNA)

- Length: 10 characters
- Free text input
- Used for Add and Update operations
- No explicit validation in BMS or program

### Cust Name :Last (ENT1LNA)

- Length: 20 characters
- Free text input
- Used for Add and Update operations
- No explicit validation in BMS or program

### DOB (ENT1DOB)

- Length: 10 characters
- Format expected: yyyy-mm-dd
- Used for Add and Update operations
- No explicit validation in BMS or program, but format is indicated on screen

### House Name (ENT1HNM)

- Length: 20 characters
- Free text input
- Used for Add and Update operations
- No explicit validation in BMS or program

### House Number (ENT1HNO)

- Length: 4 characters
- Free text input
- Used for Add and Update operations
- No explicit validation in BMS or program

### Postcode (ENT1HPC)

- Length: 8 characters
- Free text input
- Used for Add and Update operations
- Program converts input to uppercase before storing
- No explicit validation in BMS

### Phone: Home (ENT1HP1)

- Length: 20 characters
- Free text input
- Used for Add and Update operations
- No explicit validation in BMS or program

### Phone: Mob (ENT1HP2)

- Length: 20 characters
- Free text input
- Used for Add and Update operations
- No explicit validation in BMS or program

### Email Addr (ENT1HMO)

- Length: 27 characters
- Free text input
- Used for Add and Update operations
- No explicit validation in BMS or program

### Select Option (ENT1OPT)

- Length: 1 digit
- Numeric input only
- Must be entered (MUSTENTER validation)
- Options: '1' (Inquiry), '2' (Add), '4' (Update)
- Program displays error if invalid option entered

### Error/Status Message Area (ERRFLD)

- Length: 40 characters
- Display only
- Used to show status or error messages
- Populated by program logic
- Not editable by user

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
