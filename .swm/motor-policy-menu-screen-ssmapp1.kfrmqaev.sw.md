---
title: Motor Policy Menu Screen (SSMAPP1)
---
The Motor Policy Menu screen (SSMAPP1) provides users with a central interface to manage motor insurance policies. Users can inquire, add, delete, or update policy records by entering relevant details and selecting the desired operation. The screen guides users through the required fields and displays status messages for each transaction.

## Screen Preview

```
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

ENTER=Continue  PF3=End  CLEAR=Clear
```

## Fields

### Policy Number (ENP1PNO)

- Input field, 10 characters, numeric.
- Right-justified, zero-filled.
- Initial value: '0000000000' on menu display.
- Required for inquiry, add, delete, update operations.
- Validations: Numeric only, must be entered for most operations.

### Customer Number (ENP1CNO)

- Input field, 10 characters, numeric.
- Right-justified, zero-filled.
- Initial value: '0000000000' on menu display.
- Required for inquiry, add, delete, update operations.
- Validations: Numeric only, must be entered for most operations.

### Issue Date (ENP1IDA)

- Input field, 10 characters.
- Format: yyyy-mm-dd.
- Used for add/update operations.
- Validations: Must be a valid date, format enforced by label.

### Expiry Date (ENP1EDA)

- Input field, 10 characters.
- Format: yyyy-mm-dd.
- Used for add/update operations.
- Validations: Must be a valid date, format enforced by label.

### Car Make (ENP1CMK)

- Input field, 20 characters.
- Used for add/update operations.
- No explicit validation in code, but must be filled for add/update.

### Car Model (ENP1CMO)

- Input field, 20 characters.
- Used for add/update operations.
- No explicit validation in code, but must be filled for add/update.

### Car Value (ENP1VAL)

- Input field, 6 characters, numeric.
- Right-justified, zero-filled.
- Initial value: '000000' on menu display.
- Used for add/update operations.
- Validations: Numeric only.

### Registration (ENP1REG)

- Input field, 7 characters.
- Used for add/update operations.
- No explicit validation in code, but must be filled for add/update.

### Car Colour (ENP1COL)

- Input field, 8 characters.
- Used for add/update operations.
- No explicit validation in code, but must be filled for add/update.

### CC (ENP1CC)

- Input field, 6 characters, numeric.
- Right-justified, zero-filled.
- Initial value: '00000' on menu display.
- Used for add/update operations.
- Validations: Numeric only.

### Manufacture Date (ENP1MAN)

- Input field, 10 characters.
- Format: yyyy-mm-dd.
- Used for add/update operations.
- Validations: Must be a valid date, format enforced by label.

### No. of Accidents (ENP1ACC)

- Input field, 6 characters, numeric.
- Right-justified, zero-filled.
- Initial value: '000000' on menu display.
- Used for add/update operations.
- Validations: Numeric only.

### Policy Premium (ENP1PRE)

- Input field, 6 characters, numeric.
- Right-justified, zero-filled.
- Initial value: '000000' on menu display.
- Used for add/update operations.
- Validations: Numeric only.

### Select Option (ENP1OPT)

- Input field, 1 character, numeric.
- Must be entered (VALIDN=MUSTENTER).
- Used to select menu option (1-4).
- Validations: Numeric, must be 1-4.

### Error/Status Message Area (ERP1FLD)

- Output field, 40 characters.
- Used to display error or status messages.
- Protected, cannot be edited by user.
- Populated by program logic (e.g., 'Motor Policy Updated', 'Error Adding Motor Policy').

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
