# Requirements Document & Test Cases
## Feature: Save Email Recipients for Future Transfers

**Product:** iPYMT
**Feature Area:** Zelle / Peer-to-Peer Payments
**User Story ID:** iPYMT-SAVE-EMAIL-001
**Date:** 2026-03-12
**Status:** Draft

---

## 1. Requirements Document

### 1.1 Overview

Users who complete a Zelle transfer using a recipient's email address should be given the option to save that contact. Saved contacts must be persisted in the user's contact list, clearly distinguished from mobile-number contacts, and be fully manageable (view, edit, delete) with deduplication enforced.

---

### 1.2 Functional Requirements

#### FR-01 — Save Prompt After Successful Transfer
- **ID:** FR-01
- **Priority:** Must Have
- **Description:** Immediately after a successful Zelle transfer to an email address, the system shall display a prompt asking the user if they wish to save the recipient as a contact.
- **Details:**
  - The prompt shall appear on the transfer confirmation/success screen.
  - The prompt shall include the recipient's email address pre-populated.
  - The user may optionally enter or edit a display name (nickname) for the contact.
  - The prompt shall provide a clear "Save" and "Skip" (or "Not Now") action.
  - If the recipient is already saved, the save prompt shall not appear; instead, the system may display a "Already in your contacts" indicator.

#### FR-02 — Contact List Storage
- **ID:** FR-02
- **Priority:** Must Have
- **Description:** Upon confirming the save action, the system shall persist the email contact to the user's contact list associated with their iPYMT profile.
- **Details:**
  - Stored attributes: display name (optional), email address, contact type = "Email", date added, last used date.
  - The contact shall be available immediately for selection in future transfer flows.
  - The contact list shall be retrievable across sessions and devices tied to the same user account.

#### FR-03 — Email Contact Labeling
- **ID:** FR-03
- **Priority:** Must Have
- **Description:** Saved email contacts shall be visually and functionally differentiated from mobile (phone number) contacts throughout the application.
- **Details:**
  - Email contacts shall display an "Email" label or icon badge in the contact list.
  - Mobile contacts shall display a "Mobile" label or icon badge.
  - In search/filter views, users shall be able to filter by contact type (Email / Mobile / All).
  - Screen reader / accessibility labels shall include contact type in the announced text.

#### FR-04 — Edit Saved Email Recipients
- **ID:** FR-04
- **Priority:** Must Have
- **Description:** Users shall be able to edit the display name and email address of a previously saved email contact.
- **Details:**
  - Edit is accessible from the contact detail screen (e.g., long-press or overflow menu).
  - On save of an edited email address, deduplication check (FR-06) shall be re-evaluated.
  - Editing the email address shall not create a duplicate entry; it shall update the existing record.
  - The system shall confirm the edit with a success message.

#### FR-05 — Delete Saved Email Recipients
- **ID:** FR-05
- **Priority:** Must Have
- **Description:** Users shall be able to delete a saved email contact from their contact list.
- **Details:**
  - Delete is accessible from the contact detail screen or via swipe-to-delete in the list view.
  - The system shall display a confirmation dialog before permanently deleting the contact.
  - Upon confirmation, the contact is removed immediately from the list.
  - Deletion does not affect past transaction history records.

#### FR-06 — Duplicate Email Prevention
- **ID:** FR-06
- **Priority:** Must Have
- **Description:** The system shall prevent saving or editing a contact that results in a duplicate email address within the same user's contact list.
- **Details:**
  - Duplicate check is case-insensitive (e.g., `User@Example.com` == `user@example.com`).
  - When a duplicate is detected during save, the system shall display an informational message: "This email address is already saved as [Contact Name]."
  - When a duplicate is detected during edit, the system shall display an inline validation error and prevent save.
  - The user shall be offered an option to navigate to the existing contact.

---

### 1.3 Non-Functional Requirements

| ID     | Category        | Requirement |
|--------|-----------------|-------------|
| NFR-01 | Performance     | Contact list with up to 500 entries shall load in under 2 seconds on a standard mobile connection. |
| NFR-02 | Security        | Email addresses stored in contacts shall be encrypted at rest and transmitted over TLS 1.2+. |
| NFR-03 | Data Retention  | Deleted contacts shall be purged from all data stores within 30 days per data retention policy. |
| NFR-04 | Accessibility   | All contact management UI elements shall meet WCAG 2.1 AA standards. |
| NFR-05 | Usability       | Save prompt shall not interrupt the success confirmation for more than 3 seconds before being dismissible. |
| NFR-06 | Compatibility   | Feature shall function on iOS 15+ and Android 10+ native app builds and the responsive web version. |

---

### 1.4 Out of Scope

- Importing contacts from the device's native address book.
- Sharing saved contacts with other users.
- Grouping or tagging contacts beyond the Email / Mobile type distinction.
- Syncing contacts with third-party applications.

---

## 2. Test Cases

### 2.1 Test Case Naming Convention

`TC-[FEATURE]-[ID]` — e.g., `TC-SAVEEMAIL-001`

---

### TC-SAVEEMAIL-001 — Save Prompt Appears After Successful Email Transfer

| Field           | Detail |
|-----------------|--------|
| **Test Case ID**  | TC-SAVEEMAIL-001 |
| **Requirement**   | FR-01 |
| **Priority**      | High |
| **Type**          | Functional / UI |
| **Preconditions** | User is logged in; user has completed a successful Zelle transfer to an email address not in their contacts. |

**Steps:**

| Step | Action | Expected Result |
|------|--------|-----------------|
| 1 | Complete a Zelle transfer to `newrecipient@example.com` | Transfer succeeds and confirmation screen appears. |
| 2 | Observe the confirmation screen | A "Save Contact?" prompt is displayed containing `newrecipient@example.com`. |
| 3 | Verify prompt elements | Prompt shows: pre-filled email, optional "Display Name" field, "Save" button, "Skip" button. |

---

### TC-SAVEEMAIL-002 — Skip Save Prompt Does Not Save Contact

| Field           | Detail |
|-----------------|--------|
| **Test Case ID**  | TC-SAVEEMAIL-002 |
| **Requirement**   | FR-01 |
| **Priority**      | High |
| **Type**          | Functional |
| **Preconditions** | TC-SAVEEMAIL-001 preconditions met; save prompt is displayed. |

**Steps:**

| Step | Action | Expected Result |
|------|--------|-----------------|
| 1 | Tap "Skip" on the save prompt | Prompt dismisses; user remains on confirmation screen. |
| 2 | Navigate to the Contacts list | `newrecipient@example.com` does not appear in the contact list. |

---

### TC-SAVEEMAIL-003 — Saving a New Email Contact With Display Name

| Field           | Detail |
|-----------------|--------|
| **Test Case ID**  | TC-SAVEEMAIL-003 |
| **Requirement**   | FR-01, FR-02 |
| **Priority**      | High |
| **Type**          | Functional |
| **Preconditions** | Save prompt is displayed for `newrecipient@example.com`. |

**Steps:**

| Step | Action | Expected Result |
|------|--------|-----------------|
| 1 | Enter "Jane Doe" in the Display Name field | Field accepts text. |
| 2 | Tap "Save" | Success toast/message: "Contact saved." |
| 3 | Navigate to Contacts list | "Jane Doe" with email `newrecipient@example.com` appears in the list. |
| 4 | Open the contact detail | Detail shows: Name = "Jane Doe", Email = `newrecipient@example.com`, Type = "Email", Date Added = today. |

---

### TC-SAVEEMAIL-004 — Saving a New Email Contact Without Display Name

| Field           | Detail |
|-----------------|--------|
| **Test Case ID**  | TC-SAVEEMAIL-004 |
| **Requirement**   | FR-01, FR-02 |
| **Priority**      | Medium |
| **Type**          | Functional |
| **Preconditions** | Save prompt is displayed for `newrecipient@example.com`. |

**Steps:**

| Step | Action | Expected Result |
|------|--------|-----------------|
| 1 | Leave Display Name field blank | Field is empty. |
| 2 | Tap "Save" | Contact is saved using email address as the display identifier. |
| 3 | Navigate to Contacts list | `newrecipient@example.com` appears as the contact identifier. |

---

### TC-SAVEEMAIL-005 — Email Contact Displays Correct Label

| Field           | Detail |
|-----------------|--------|
| **Test Case ID**  | TC-SAVEEMAIL-005 |
| **Requirement**   | FR-03 |
| **Priority**      | High |
| **Type**          | Functional / UI |
| **Preconditions** | At least one email contact and one mobile contact are saved in the user's contact list. |

**Steps:**

| Step | Action | Expected Result |
|------|--------|-----------------|
| 1 | Open the Contacts list | Both contacts are visible. |
| 2 | Observe the email contact row | An "Email" label or icon badge is displayed on the email contact. |
| 3 | Observe the mobile contact row | A "Mobile" label or icon badge is displayed on the mobile contact. |
| 4 | Apply filter "Email" | Only email contacts are displayed. |
| 5 | Apply filter "Mobile" | Only mobile contacts are displayed. |
| 6 | Apply filter "All" | All contacts are displayed. |

---

### TC-SAVEEMAIL-006 — Edit Display Name of Email Contact

| Field           | Detail |
|-----------------|--------|
| **Test Case ID**  | TC-SAVEEMAIL-006 |
| **Requirement**   | FR-04 |
| **Priority**      | High |
| **Type**          | Functional |
| **Preconditions** | An email contact "Jane Doe" / `newrecipient@example.com` exists. |

**Steps:**

| Step | Action | Expected Result |
|------|--------|-----------------|
| 1 | Open the contact detail for "Jane Doe" | Detail screen is displayed. |
| 2 | Tap "Edit" | Edit form opens pre-populated with existing name and email. |
| 3 | Change Display Name to "Jane Smith" | Field updates to "Jane Smith". |
| 4 | Tap "Save" | Success message displayed; contact detail shows "Jane Smith". |
| 5 | Navigate to Contacts list | Contact appears as "Jane Smith" in the list. |

---

### TC-SAVEEMAIL-007 — Edit Email Address of Email Contact

| Field           | Detail |
|-----------------|--------|
| **Test Case ID**  | TC-SAVEEMAIL-007 |
| **Requirement**   | FR-04 |
| **Priority**      | High |
| **Type**          | Functional |
| **Preconditions** | An email contact exists with `oldaddress@example.com`; `newaddress@example.com` is not saved. |

**Steps:**

| Step | Action | Expected Result |
|------|--------|-----------------|
| 1 | Open the contact detail and tap "Edit" | Edit form opens. |
| 2 | Change email to `newaddress@example.com` | Field updates. |
| 3 | Tap "Save" | Success message displayed; contact now shows `newaddress@example.com`. |
| 4 | Verify `oldaddress@example.com` is no longer in the list | Old email address is not present in contacts. |

---

### TC-SAVEEMAIL-008 — Delete Email Contact With Confirmation

| Field           | Detail |
|-----------------|--------|
| **Test Case ID**  | TC-SAVEEMAIL-008 |
| **Requirement**   | FR-05 |
| **Priority**      | High |
| **Type**          | Functional |
| **Preconditions** | An email contact "Jane Doe" / `newrecipient@example.com` exists. |

**Steps:**

| Step | Action | Expected Result |
|------|--------|-----------------|
| 1 | Open the contact detail for "Jane Doe" | Detail screen displayed. |
| 2 | Tap "Delete" (or swipe to delete in list view) | Confirmation dialog appears: "Are you sure you want to delete Jane Doe?" with "Delete" and "Cancel" options. |
| 3 | Tap "Cancel" | Dialog dismisses; contact is not deleted. |
| 4 | Repeat step 2 and tap "Delete" | Contact is removed; success toast: "Contact deleted." |
| 5 | Navigate to Contacts list | "Jane Doe" / `newrecipient@example.com` is not present. |

---

### TC-SAVEEMAIL-009 — Duplicate Email Prevented During Save

| Field           | Detail |
|-----------------|--------|
| **Test Case ID**  | TC-SAVEEMAIL-009 |
| **Requirement**   | FR-06 |
| **Priority**      | High |
| **Type**          | Functional / Negative |
| **Preconditions** | `existing@example.com` is already saved as contact "Existing User". User completes a new transfer to `existing@example.com`. |

**Steps:**

| Step | Action | Expected Result |
|------|--------|-----------------|
| 1 | Complete a successful Zelle transfer to `existing@example.com` | Transfer succeeds. |
| 2 | Observe confirmation screen | Save prompt does NOT appear; instead, an indicator "Already in your contacts as Existing User" is shown. |
| 3 | (Alternative) If save prompt appears, tap "Save" | System displays message: "This email address is already saved as Existing User." Contact is not duplicated. |

---

### TC-SAVEEMAIL-010 — Duplicate Email Check Is Case-Insensitive

| Field           | Detail |
|-----------------|--------|
| **Test Case ID**  | TC-SAVEEMAIL-010 |
| **Requirement**   | FR-06 |
| **Priority**      | Medium |
| **Type**          | Functional / Negative |
| **Preconditions** | `existing@example.com` is saved. User initiates transfer to `EXISTING@EXAMPLE.COM`. |

**Steps:**

| Step | Action | Expected Result |
|------|--------|-----------------|
| 1 | Complete a transfer to `EXISTING@EXAMPLE.COM` | Transfer succeeds. |
| 2 | Observe confirmation screen | Duplicate detected; save prompt does not appear or shows "Already in your contacts." |
| 3 | Attempt to save via any path | System blocks save and references the existing contact. |

---

### TC-SAVEEMAIL-011 — Duplicate Email Prevented During Edit

| Field           | Detail |
|-----------------|--------|
| **Test Case ID**  | TC-SAVEEMAIL-011 |
| **Requirement**   | FR-06 |
| **Priority**      | High |
| **Type**          | Functional / Negative |
| **Preconditions** | Two contacts exist: `contact1@example.com` and `contact2@example.com`. |

**Steps:**

| Step | Action | Expected Result |
|------|--------|-----------------|
| 1 | Open edit form for `contact1@example.com` | Edit form opens. |
| 2 | Change email to `contact2@example.com` | Field updates. |
| 3 | Tap "Save" | Inline error displayed: "This email address is already saved as [Contact 2 Name]." |
| 4 | Verify no duplicate is created | `contact2@example.com` still maps only to its original contact; `contact1@example.com` unchanged. |

---

### TC-SAVEEMAIL-012 — Contact Available for Selection in Future Transfer

| Field           | Detail |
|-----------------|--------|
| **Test Case ID**  | TC-SAVEEMAIL-012 |
| **Requirement**   | FR-02 |
| **Priority**      | High |
| **Type**          | Functional / Integration |
| **Preconditions** | `newrecipient@example.com` has been saved as "Jane Doe". |

**Steps:**

| Step | Action | Expected Result |
|------|--------|-----------------|
| 1 | Initiate a new Zelle transfer | Transfer flow opens. |
| 2 | Navigate to the recipient selection/search screen | Contact list or search is displayed. |
| 3 | Search for "Jane" or `newrecipient@example.com` | "Jane Doe" appears in search results with Email label. |
| 4 | Select "Jane Doe" | Email `newrecipient@example.com` is pre-populated as transfer recipient. |
| 5 | Complete transfer | Transfer succeeds using saved contact. |

---

### TC-SAVEEMAIL-013 — Contacts Persist Across Sessions

| Field           | Detail |
|-----------------|--------|
| **Test Case ID**  | TC-SAVEEMAIL-013 |
| **Requirement**   | FR-02 |
| **Priority**      | High |
| **Type**          | Functional / Session |
| **Preconditions** | `newrecipient@example.com` saved as "Jane Doe" in current session. |

**Steps:**

| Step | Action | Expected Result |
|------|--------|-----------------|
| 1 | Log out of the iPYMT application | Session ends. |
| 2 | Log back in with the same credentials | Session starts. |
| 3 | Navigate to Contacts list | "Jane Doe" / `newrecipient@example.com` is still present. |

---

### TC-SAVEEMAIL-014 — Accessibility: Screen Reader Announces Contact Type

| Field           | Detail |
|-----------------|--------|
| **Test Case ID**  | TC-SAVEEMAIL-014 |
| **Requirement**   | FR-03, NFR-04 |
| **Priority**      | Medium |
| **Type**          | Accessibility |
| **Preconditions** | Screen reader enabled (TalkBack / VoiceOver); email and mobile contacts exist. |

**Steps:**

| Step | Action | Expected Result |
|------|--------|-----------------|
| 1 | Focus on an email contact row using screen reader | Screen reader announces name, email address, and "Email contact" type. |
| 2 | Focus on a mobile contact row using screen reader | Screen reader announces name, phone number, and "Mobile contact" type. |

---

### TC-SAVEEMAIL-015 — Contact List Performance With 500 Entries

| Field           | Detail |
|-----------------|--------|
| **Test Case ID**  | TC-SAVEEMAIL-015 |
| **Requirement**   | NFR-01 |
| **Priority**      | Medium |
| **Type**          | Performance |
| **Preconditions** | User account has 500 saved contacts (mixed email and mobile). Standard mobile network conditions. |

**Steps:**

| Step | Action | Expected Result |
|------|--------|-----------------|
| 1 | Navigate to the Contacts list | Contact list loads fully within 2 seconds. |
| 2 | Scroll through the full list | No perceptible lag or frame drops during scrolling. |
| 3 | Search for a specific contact | Search results appear within 1 second of input. |

---

## 3. Traceability Matrix

| Acceptance Criteria | Requirement(s) | Test Case(s) |
|---------------------|----------------|--------------|
| Option to save recipient after successful transfer | FR-01 | TC-SAVEEMAIL-001, TC-SAVEEMAIL-002, TC-SAVEEMAIL-003, TC-SAVEEMAIL-004 |
| Saved contacts appear in contact list | FR-02 | TC-SAVEEMAIL-003, TC-SAVEEMAIL-012, TC-SAVEEMAIL-013 |
| Email contacts clearly labeled vs. mobile contacts | FR-03 | TC-SAVEEMAIL-005, TC-SAVEEMAIL-014 |
| Users can edit or delete saved email recipients | FR-04, FR-05 | TC-SAVEEMAIL-006, TC-SAVEEMAIL-007, TC-SAVEEMAIL-008 |
| No duplicate email addresses | FR-06 | TC-SAVEEMAIL-009, TC-SAVEEMAIL-010, TC-SAVEEMAIL-011 |

---

*Document Owner: Product / QA Team*
*Review Cycle: Before sprint start*
