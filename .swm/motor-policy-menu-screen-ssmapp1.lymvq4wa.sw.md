---
title: Motor Policy Menu Screen (SSMAPP1)
---
The Motor Policy Menu screen (SSMAPP1) provides users with a central interface to manage motor insurance policies, including inquiry, addition, deletion, and update of policy records. It collects and displays all relevant motor policy information and guides users through transaction options.

## Screen Preview

````
SSP1        General Insurance Motor Policy Menu

    1. Policy Inquiry 
    2. Policy Add     
    3. Policy Delete  
    4. Policy Update  

                              Policy Number ____________
                              Cust Number  ____________
                              Issue date   ____________ (yyyy-mm-dd)
                              Expiry date  ____________ (yyyy-mm-dd)
                              Car Make     ______________________
                              Car Model    ______________________
                              Car Value    ______
                              Registration _________
                              Car Colour   ________
                              CC           ________
                              Manufacture Date ____________ (yyyy-mm-dd)
                              No. of Accidents ______
                              Policy Premium  ______

        Select Option _


        [Error/Status Message Area]

ENTER=Continue  PF3=Exit  CLEAR=Clear
```,
````

## Fields

### Policy Number (ENP1PNO)

- Input field, length 10 characters.
- Right-justified, zero-filled.
- Required for most transactions.
- Validated in COBOL for inquiry, add, delete, update operations.

### Customer Number (ENP1CNO)

- Input field, length 10 characters.
- Right-justified, zero-filled.
- Required for most transactions.
- Validated in COBOL for inquiry, add, delete, update operations.

### Issue Date (ENP1IDA)

- Input field, length 10 characters.
- Format: yyyy-mm-dd (displayed as hint).
- Used for add/update operations.
- No explicit validation in COBOL, but expected to be a valid date.

### Expiry Date (ENP1EDA)

- Input field, length 10 characters.
- Format: yyyy-mm-dd (displayed as hint).
- Used for add/update operations.
- No explicit validation in COBOL, but expected to be a valid date.

### Car Make (ENP1CMK)

- Input field, length 20 characters.
- Used for add/update operations.
- No explicit validation in COBOL.

### Car Model (ENP1CMO)

- Input field, length 20 characters.
- Used for add/update operations.
- No explicit validation in COBOL.

### Car Value (ENP1VAL)

- Input field, length 6 characters.
- Right-justified, zero-filled.
- Used for add/update operations.
- No explicit validation in COBOL.

### Registration (ENP1REG)

- Input field, length 7 characters.
- Used for add/update operations.
- No explicit validation in COBOL.

### Car Colour (ENP1COL)

- Input field, length 8 characters.
- Used for add/update operations.
- No explicit validation in COBOL.

### CC (ENP1CC)

- Input field, length 6 characters.
- Right-justified, zero-filled.
- Used for add/update operations.
- No explicit validation in COBOL.

### Manufacture Date (ENP1MAN)

- Input field, length 10 characters.
- Format: yyyy-mm-dd (displayed as hint).
- Used for add/update operations.
- No explicit validation in COBOL.

### No. of Accidents (ENP1ACC)

- Input field, length 6 characters.
- Right-justified, zero-filled.
- Used for add/update operations.
- No explicit validation in COBOL.

### Policy Premium (ENP1PRE)

- Input field, length 6 characters.
- Right-justified, zero-filled.
- Used for add/update operations.
- No explicit validation in COBOL.

### Select Option (ENP1OPT)

- Input field, length 1 character.
- Numeric only.
- Must be entered (MUSTENTER validation).
- Used to select menu option (1-4).
- Validated in COBOL; error message shown for invalid input.

### Error/Status Message Area (ERP1FLD)

- Output field, length 40 characters.
- Used to display error/status messages from COBOL logic.
- Not user-editable.
- Messages include transaction results, errors, prompts.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
