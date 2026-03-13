# Software Requirements Document (SRD)
## Feature: Email Address Validation for Zelle Transfers

**Project:** iPYMT — Zelle Integration
**Document ID:** iPYMT-SRD-EMAILVAL-001
**Version:** 1.0
**Date:** 2026-03-12
**Status:** Approved for Development

---

## 1. Introduction

### 1.1 Purpose
This document defines the complete software requirements for the Email Address Validation feature within the iPYMT Zelle transfer flow. It serves as the authoritative reference for design, development, testing, and acceptance activities.

### 1.2 Scope
The feature validates email addresses entered by customers during a Zelle money transfer. Validation includes:
- Real-time format checking as the user types.
- Visual feedback (valid/invalid indicators).
- Submit-button gating until valid format is confirmed.
- Back-end verification that the email is registered with the Zelle network before the transfer is committed.
- User-friendly error messaging throughout.

### 1.3 Definitions and Acronyms

| Term | Definition |
|------|------------|
| iPYMT | Payments mobile/web application |
| Zelle | Real-time payment network operated by Early Warning Services, LLC |
| Email Format Validation | Syntactic check per RFC 5322 |
| Zelle Registry Check | API call to Zelle Early Warning Services to confirm enrollment |
| UI | User Interface |
| API | Application Programming Interface |
| REST | Representational State Transfer |
| SRD | Software Requirements Document |
| RTM | Requirements Traceability Matrix |
| HLD | High-Level Design |
| LLD | Low-Level Design |

### 1.4 References
- RFC 5322 — Internet Message Format (email syntax)
- Early Warning Services Zelle API Integration Guide (internal)
- iPYMT UX Design System v3.2
- OWASP Input Validation Cheat Sheet

---

## 2. Overall Description

### 2.1 Product Perspective
The Email Validation feature is a component within the existing Zelle transfer flow in iPYMT. It is triggered when a user selects "Email Address" as the recipient identification method on the transfer entry screen.

### 2.2 User Classes and Characteristics

| User Class | Description |
|------------|-------------|
| Authenticated Customer | Logged-in iPYMT user initiating a Zelle transfer |
| Guest / Unauthenticated | Not in scope; Zelle requires authentication |
| System Administrator | Monitors validation error rates; no direct feature interaction |

### 2.3 Operating Environment
- Mobile: iOS 15+, Android 10+ (native apps)
- Web: Chrome 110+, Safari 16+, Firefox 115+, Edge 110+
- Backend: Java 17, Spring Boot 3.x, deployed on AWS EKS
- Integration: Zelle Early Warning Services REST API

### 2.4 Assumptions and Dependencies
- The Zelle EWS API is available with SLA >= 99.9%.
- Network latency for Zelle registry check does not exceed 3 seconds (P99).
- Users are authenticated before entering the transfer flow.
- Email addresses are entered manually; paste is permitted.

---

## 3. Functional Requirements

### 3.1 Real-Time Format Validation (FR-EV-01)

**ID:** FR-EV-01
**Priority:** Must Have
**Description:** The system shall validate the email address format in real-time as the user types each character.

**Detailed Behaviour:**
- Validation logic shall conform to RFC 5322 simplified pattern:
  `[local-part]@[domain].[tld]`
- Validation shall trigger after each keystroke with no more than 300 ms debounce delay.
- Format rules enforced:
  - Must contain exactly one `@` symbol.
  - Local part: 1–64 characters; alphanumeric, dots, hyphens, underscores, plus signs allowed; cannot start or end with a dot.
  - Domain: 1–253 characters; alphanumeric and hyphens; labels separated by dots; no consecutive dots.
  - TLD: minimum 2 characters, alphabetic only.
  - Total length: maximum 254 characters (per RFC 5321).
- Leading and trailing whitespace shall be automatically trimmed before validation.
- Emoji, null bytes, and control characters shall be rejected immediately on input.

### 3.2 Visual Validation Indicators (FR-EV-02)

**ID:** FR-EV-02
**Priority:** Must Have
**Description:** Clear visual indicators shall communicate the current validation state to the user.

**States and Visual Treatment:**

| State | Input Border | Icon | Helper Text |
|-------|-------------|------|-------------|
| Empty / Untouched | Default (grey) | None | Placeholder text only |
| Typing / In-progress | Default (grey) | None | None |
| Valid Format | Green border | Green checkmark | "Valid email format" |
| Invalid Format | Red border | Red exclamation | Error message (see FR-EV-05) |
| Zelle Check In Progress | Blue border | Spinner | "Checking Zelle registration..." |
| Zelle Registered | Green border | Green double-checkmark | "Registered with Zelle" |
| Zelle Not Registered | Amber border | Amber warning icon | "This email is not registered with Zelle" |
| API Error / Timeout | Grey border | Grey info icon | "Unable to verify — you may still proceed" |

- All colour indicators must be accompanied by a non-colour indicator (icon/text) for colour-blind accessibility (WCAG 1.4.1).

### 3.3 Submit Button Gating (FR-EV-03)

**ID:** FR-EV-03
**Priority:** Must Have
**Description:** The "Continue" / "Send Money" submit button shall be disabled until a syntactically valid email address has been entered.

**Rules:**
- Button is disabled (greyed out, non-interactive) on initial page load.
- Button enables only when FR-EV-01 format validation passes.
- Button re-disables if the user edits the field back to an invalid state.
- Zelle registration check (FR-EV-04) must complete (pass or fail) before the button is enabled for final submission.
- If Zelle API is unavailable, button is enabled with a warning indicator (user can proceed with disclaimer).
- The disabled state shall be communicated via `aria-disabled="true"` and a visible tooltip on hover/focus: "Please enter a valid email address."

### 3.4 Zelle Registration Check (FR-EV-04)

**ID:** FR-EV-04
**Priority:** Must Have
**Description:** After format validation passes and the user pauses typing (1 500 ms debounce), the system shall call the Zelle Early Warning Services API to verify that the email address is enrolled in the Zelle network.

**Behaviour:**
- API call is made asynchronously; the UI shows a loading indicator (FR-EV-02).
- On registered response: green indicator; button fully enabled.
- On not-registered response: amber warning shown; user is informed and offered to proceed at their own risk or cancel (configurable by bank policy flag).
- On API error / timeout (>3 s): grey info indicator; user may proceed with acknowledgement.
- Rate limiting: no more than 1 check per 1 500 ms per session to prevent API abuse.
- The email address shall be hashed (SHA-256) before transmission to the Zelle API to comply with PII handling policies.
- Result shall be cached in session memory for 60 seconds to avoid redundant calls.

### 3.5 Error Messages (FR-EV-05)

**ID:** FR-EV-05
**Priority:** Must Have
**Description:** The system shall display specific, actionable error messages for each invalid email scenario.

**Error Message Catalogue:**

| Scenario | Error Message |
|----------|---------------|
| Missing `@` symbol | "Email address must include an '@' symbol." |
| Multiple `@` symbols | "Email address can only contain one '@' symbol." |
| Missing domain | "Please include a domain after '@' (e.g., example.com)." |
| Missing TLD | "Email domain must include a valid extension (e.g., .com, .org)." |
| Local part too long (>64 chars) | "The part before '@' cannot exceed 64 characters." |
| Total length exceeded (>254 chars) | "Email address is too long (max 254 characters)." |
| Invalid characters | "Email address contains invalid characters." |
| Starts/ends with dot | "Email address cannot start or end with a dot." |
| Consecutive dots | "Email address cannot contain consecutive dots." |
| Not registered with Zelle | "This email address is not registered with Zelle. Please check the address or ask the recipient to enroll." |
| Empty on submit attempt | "Please enter an email address." |

- Error messages shall appear below the input field.
- Only the most specific error for the current input state shall be shown (no stacking).
- Messages shall be announced via `aria-live="polite"` for screen readers.

### 3.6 Input Sanitisation (FR-EV-06)

**ID:** FR-EV-06
**Priority:** Must Have
**Description:** All email input shall be sanitised to prevent injection attacks before processing.

- HTML/script tags stripped from input.
- Null bytes and control characters (ASCII 0–31) rejected.
- Input length hard-capped at 320 characters server-side (defence in depth).
- Server-side validation mirrors all client-side format rules.

---

## 4. Non-Functional Requirements

| ID | Category | Requirement |
|----|----------|-------------|
| NFR-EV-01 | Performance | Client-side format validation response within 50 ms of keystroke. |
| NFR-EV-02 | Performance | Zelle API call completes within 3 s (P95). Timeout threshold: 5 s. |
| NFR-EV-03 | Security | Email PII hashed (SHA-256) before transmission to Zelle API. |
| NFR-EV-04 | Security | All API calls over TLS 1.3; certificate pinning on mobile. |
| NFR-EV-05 | Security | Server-side validation mirrors all client-side rules (no trust of client). |
| NFR-EV-06 | Accessibility | WCAG 2.1 AA compliance; colour-blind safe indicators; screen reader support. |
| NFR-EV-07 | Availability | Zelle registry check failure must not block transfer flow; graceful degradation. |
| NFR-EV-08 | Usability | Error messages display within 300 ms of detecting invalid state. |
| NFR-EV-09 | Internationalisation | Validation logic handles internationalised domain names (IDN / punycode). |
| NFR-EV-10 | Logging | All validation failures and Zelle check outcomes logged (no PII in logs). |

---

## 5. Constraints

- The Zelle EWS API contract must not be changed by this feature.
- Session-only caching; no persistent storage of Zelle registration results.
- Feature flag controlled rollout: `FEATURE_EMAIL_VALIDATION_V2` must be true to activate new validation.
- Backwards compatible with existing transfer payloads.

---

## 6. Acceptance Criteria Summary

| AC# | Criterion |
|-----|-----------|
| AC-01 | Real-time validation fires within 300 ms debounce of each keystroke. |
| AC-02 | Green/red visual indicator accurately reflects format validity. |
| AC-03 | Submit button disabled until valid format; re-disables on regression. |
| AC-04 | Zelle registration API called and result shown before final submit. |
| AC-05 | All error messages from catalogue shown for correct scenarios. |
| AC-06 | API failure does not block transfer; user can proceed with warning. |
| AC-07 | Server-side re-validates format and sanitises input. |
