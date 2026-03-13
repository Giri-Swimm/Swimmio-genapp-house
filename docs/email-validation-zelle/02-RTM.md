# Requirements Traceability Matrix (RTM)
## Feature: Email Address Validation for Zelle Transfers

**Document ID:** iPYMT-RTM-EMAILVAL-001
**Version:** 1.0
**Date:** 2026-03-12

---

## 1. Traceability Matrix

The RTM traces each User Story Acceptance Criterion through Functional Requirements, Design artefacts, Java implementation classes, and Test Cases.

| AC# | Acceptance Criterion | Requirement(s) | HLD Component | LLD Class / Method | Test Case(s) |
|-----|----------------------|---------------|---------------|--------------------|--------------|
| AC-01 | Real-time validation fires within 300 ms debounce of each keystroke | FR-EV-01 | Frontend Input Handler | `EmailValidator.isValidFormat()` | TC-EV-001, TC-EV-002, TC-EV-003 |
| AC-02 | Green/red visual indicators accurately reflect format validity | FR-EV-02 | UI State Manager | `ValidationState` enum, `EmailValidationResponse` | TC-EV-004, TC-EV-005 |
| AC-03 | Submit button disabled until valid format; re-disables on regression | FR-EV-03 | Submit Gate Component | `EmailValidationController.validateFormat()` | TC-EV-006, TC-EV-007 |
| AC-04 | Zelle registration API called and result shown before final submit | FR-EV-04 | Zelle Registry Client | `ZelleRegistryService.checkRegistration()` | TC-EV-008, TC-EV-009, TC-EV-010 |
| AC-05 | Correct error messages shown for each invalid scenario | FR-EV-05 | Error Message Resolver | `ErrorMessageResolver.resolve()` | TC-EV-011 through TC-EV-021 |
| AC-06 | API failure does not block transfer; graceful degradation | FR-EV-04, NFR-EV-07 | Zelle Registry Client | `ZelleRegistryService` circuit-breaker / fallback | TC-EV-022, TC-EV-023 |
| AC-07 | Server-side re-validates format and sanitises input | FR-EV-06, NFR-EV-05 | Validation Controller | `EmailSanitiser.sanitise()`, `EmailValidationController` | TC-EV-024, TC-EV-025 |

---

## 2. Requirement-to-Test Coverage

| Requirement ID | Requirement Summary | Test Cases | Coverage |
|----------------|---------------------|------------|----------|
| FR-EV-01 | Real-time format validation | TC-EV-001 – TC-EV-003, TC-EV-011 – TC-EV-021 | Full |
| FR-EV-02 | Visual indicators | TC-EV-004, TC-EV-005 | Full |
| FR-EV-03 | Submit button gating | TC-EV-006, TC-EV-007 | Full |
| FR-EV-04 | Zelle registration check | TC-EV-008 – TC-EV-010, TC-EV-022, TC-EV-023 | Full |
| FR-EV-05 | Error messages | TC-EV-011 – TC-EV-021 | Full |
| FR-EV-06 | Input sanitisation | TC-EV-024, TC-EV-025 | Full |
| NFR-EV-01 | Client validation < 50 ms | TC-EV-PERF-001 | Full |
| NFR-EV-02 | Zelle API < 3 s P95 | TC-EV-PERF-002 | Full |
| NFR-EV-03 | PII hashing | TC-EV-SEC-001 | Full |
| NFR-EV-04 | TLS enforcement | TC-EV-SEC-002 | Full |
| NFR-EV-05 | Server-side mirrors client validation | TC-EV-024, TC-EV-025 | Full |
| NFR-EV-06 | WCAG 2.1 AA | TC-EV-ACC-001 – TC-EV-ACC-003 | Full |
| NFR-EV-07 | Graceful degradation on API failure | TC-EV-022, TC-EV-023 | Full |
| NFR-EV-08 | Error shown within 300 ms | TC-EV-PERF-003 | Full |
| NFR-EV-09 | IDN / punycode support | TC-EV-026 | Full |
| NFR-EV-10 | Logging without PII | TC-EV-SEC-003 | Full |

---

## 3. Design-to-Requirement Coverage

| Design Artefact | Implements Requirement(s) |
|-----------------|--------------------------|
| HLD — Frontend Validation Module | FR-EV-01, FR-EV-02, FR-EV-03 |
| HLD — Email Validation API Service | FR-EV-01, FR-EV-04, FR-EV-06 |
| HLD — Zelle Registry Integration | FR-EV-04, NFR-EV-07 |
| LLD — `EmailValidator` | FR-EV-01, FR-EV-05 |
| LLD — `ZelleRegistryService` | FR-EV-04, NFR-EV-02, NFR-EV-03, NFR-EV-07 |
| LLD — `EmailValidationController` | FR-EV-01, FR-EV-06, NFR-EV-05 |
| LLD — `ErrorMessageResolver` | FR-EV-05 |
| LLD — `EmailSanitiser` | FR-EV-06 |
| ER Diagram — `email_validation_audit` table | NFR-EV-10 |

---

## 4. Acceptance Criteria Signoff Tracker

| AC# | Dev Complete | Unit Tests Pass | Integration Tests Pass | UAT Pass | Sign-Off Owner |
|-----|-------------|-----------------|----------------------|----------|----------------|
| AC-01 | [ ] | [ ] | [ ] | [ ] | Engineering Lead |
| AC-02 | [ ] | [ ] | [ ] | [ ] | UX Lead |
| AC-03 | [ ] | [ ] | [ ] | [ ] | Engineering Lead |
| AC-04 | [ ] | [ ] | [ ] | [ ] | Integration Lead |
| AC-05 | [ ] | [ ] | [ ] | [ ] | QA Lead |
| AC-06 | [ ] | [ ] | [ ] | [ ] | Engineering Lead |
| AC-07 | [ ] | [ ] | [ ] | [ ] | Security Lead |
