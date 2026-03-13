# High-Level Design (HLD)
## Feature: Email Address Validation for Zelle Transfers

**Document ID:** iPYMT-HLD-EMAILVAL-001
**Version:** 1.0
**Date:** 2026-03-12

---

## 1. Architecture Overview

The Email Validation feature follows a **layered, event-driven architecture** with client-side validation as the first gate and server-side validation as the authoritative gate. The Zelle registry check is an asynchronous enrichment step.

```
+----------------------------------------------------------+
|                    iPYMT Mobile / Web App                |
|  +----------------------------------------------------+  |
|  |           Zelle Transfer Screen (UI Layer)          |  |
|  |  [Email Input Field] --> [Validation State Engine] |  |
|  |  [Visual Indicator]  <-- [State Engine]            |  |
|  |  [Error Message]     <-- [Error Resolver]          |  |
|  |  [Continue Button]   <-- [Submit Gate]             |  |
|  +--------------------+-------------------------------+  |
+------------------------|---------------------------------+
                         | HTTPS / REST
+------------------------v---------------------------------+
|              iPYMT Backend — API Gateway                 |
|  +----------------------------------------------------+  |
|  |        Email Validation Service (Spring Boot)       |  |
|  |   +----------------+   +------------------------+  |  |
|  |   | Format         |   | Sanitiser              |  |  |
|  |   | Validator      |   | (XSS / Injection)      |  |  |
|  |   +----------------+   +------------------------+  |  |
|  |   +------------------------------------------+     |  |
|  |   |     Zelle Registry Client (Async)        |     |  |
|  |   |  [Circuit Breaker] [SHA-256 Hash] [Cache]|     |  |
|  |   +------------------------------------------+     |  |
|  |   +------------------------------------------+     |  |
|  |   |     Audit Logger (no PII)                |     |  |
|  |   +------------------------------------------+     |  |
|  +----------------------------------------------------+  |
+------------------------|---------------------------------+
                         | HTTPS / mTLS
+------------------------v---------------------------------+
|        Zelle Early Warning Services API                  |
|        POST /v1/recipient/lookup                         |
+----------------------------------------------------------+
```

---

## 2. Component Description

### 2.1 Frontend — Validation State Engine
- **Technology:** React (web) / React Native (mobile)
- **Responsibility:** Debounced keystroke listener triggers `validateEmailFormat()`. Manages UI state transitions (EMPTY → TYPING → VALID → INVALID → CHECKING → REGISTERED / NOT_REGISTERED).
- **Key interactions:** Calls backend `/api/v1/email/validate` for format check (debounced 300 ms) and `/api/v1/email/zelle-check` for registry check (triggered on format-valid + 1 500 ms idle).

### 2.2 Frontend — Submit Gate
- **Responsibility:** Subscribes to validation state. Enables the submit button only when state is `REGISTERED` or `API_ERROR` (with disclaimer). Disables otherwise.

### 2.3 Frontend — Error Message Resolver
- **Responsibility:** Maps `ValidationError` codes returned by the backend to human-readable strings from a locale-specific message bundle.

### 2.4 Backend — Email Validation Service
- **Technology:** Java 17, Spring Boot 3.x, deployed as a microservice
- **Endpoints:**
  - `POST /api/v1/email/validate` — format validation + sanitisation
  - `POST /api/v1/email/zelle-check` — Zelle registry lookup
- **Responsibility:** Authoritative validation; sanitises input; delegates to `EmailValidator` and `ZelleRegistryService`.

### 2.5 Backend — Zelle Registry Client
- **Technology:** Spring WebClient (reactive, non-blocking)
- **Responsibility:** Hashes email (SHA-256), calls Zelle EWS API, parses response, returns `ZelleRegistrationStatus`.
- **Resilience:** Resilience4j circuit breaker; 5 s timeout; retry once on 5xx; fallback returns `UNKNOWN` status.
- **Caching:** Caffeine in-memory cache; TTL 60 s per hashed email; max 10 000 entries.

### 2.6 Backend — Audit Logger
- **Technology:** SLF4J → ELK Stack
- **Responsibility:** Logs validation outcome, timestamp, session ID (no email PII), error codes, latency.

---

## 3. Data Flow

### 3.1 Format Validation Flow
```
User types character
        |
   [Debounce 300ms]
        |
   Client: validateEmailFormat(input)
        |
   Valid? ──Yes──> POST /api/v1/email/validate
        |                    |
        No                   | 200 OK {valid:true}  or  422 {valid:false, errorCode}
        |                    |
   Show error           Update UI State
   Red indicator        Green indicator (format valid)
                             |
                        [Start 1500ms idle timer]
                             |
                    POST /api/v1/email/zelle-check
                             |
               +-------------+-------------+
               |             |             |
          REGISTERED    NOT_REGISTERED   API_ERROR
               |             |             |
          Green ✓✓       Amber ⚠         Grey ℹ
          Enable btn    Show warning    Enable w/ disclaimer
```

### 3.2 Zelle Registry Check Flow
```
POST /api/v1/email/zelle-check  {email: "user@example.com"}
        |
   [EmailSanitiser.sanitise()]
        |
   [EmailValidator.isValidFormat()]  — server-side re-check
        |
   [SHA-256 hash of email]
        |
   [Cache lookup: hash → status]
        |
   Cache HIT? ──Yes──> Return cached status
        |
        No
        |
   [ZelleRegistryClient.lookup(hashedEmail)]
        |
   [Resilience4j: circuit open?]
        |
   Call Zelle EWS API
        |
   Parse response → ZelleRegistrationStatus
        |
   Store in cache (TTL 60s)
        |
   Return status to caller
```

---

## 4. API Contract

### 4.1 Format Validate Endpoint

**Request:**
```
POST /api/v1/email/validate
Content-Type: application/json

{
  "email": "user@example.com",
  "sessionId": "sess-abc123"
}
```

**Response (valid):**
```json
{
  "valid": true,
  "errorCode": null,
  "errorMessage": null
}
```

**Response (invalid):**
```json
{
  "valid": false,
  "errorCode": "MISSING_AT_SYMBOL",
  "errorMessage": "Email address must include an '@' symbol."
}
```

**HTTP Status Codes:**
- `200 OK` — validation completed (result in body)
- `400 Bad Request` — malformed request payload
- `429 Too Many Requests` — rate limit exceeded

### 4.2 Zelle Registry Check Endpoint

**Request:**
```
POST /api/v1/email/zelle-check
Content-Type: application/json

{
  "email": "user@example.com",
  "sessionId": "sess-abc123"
}
```

**Response:**
```json
{
  "registrationStatus": "REGISTERED",
  "canProceed": true,
  "message": "Registered with Zelle",
  "requiresAcknowledgement": false
}
```

**`registrationStatus` values:** `REGISTERED` | `NOT_REGISTERED` | `UNKNOWN`

**HTTP Status Codes:**
- `200 OK`
- `400 Bad Request` — invalid email format
- `503 Service Unavailable` — Zelle API unavailable (body includes fallback guidance)

---

## 5. Security Design

| Concern | Mitigation |
|---------|-----------|
| PII transmission | Email hashed (SHA-256) before leaving iPYMT backend |
| Injection attacks | Input sanitised server-side; parameterised all DB queries |
| Man-in-the-middle | TLS 1.3; certificate pinning on iOS and Android |
| API enumeration | Rate limiting: 10 checks/min/session; exponential backoff |
| Logging PII | Session ID logged, not email; hashed token used in audit |
| CSRF | All POST endpoints protected by CSRF token (Spring Security) |

---

## 6. Deployment and Feature Flag

- Feature controlled by flag `FEATURE_EMAIL_VALIDATION_V2` in LaunchDarkly.
- Canary rollout: 5% → 25% → 100% over 2 weeks.
- Rollback: toggle feature flag; no schema migration required.
