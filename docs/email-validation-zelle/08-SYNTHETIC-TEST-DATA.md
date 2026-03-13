# Synthetic Test Data
## Feature: Email Address Validation for Zelle Transfers

**Document ID:** iPYMT-STD-EMAILVAL-001
**Version:** 1.0
**Date:** 2026-03-12

> All data below is **entirely synthetic**. No real customers, accounts, or email addresses are used.
> Test email domains use `@testbank.invalid` or `@zelletest.invalid` (`.invalid` is reserved per RFC 2606).

---

## 1. Valid Email Addresses — Format Passes

| TD# | Email | Description | Expected Format Result |
|-----|-------|-------------|----------------------|
| TD-V-001 | `alice.johnson@testbank.invalid` | Standard name.surname format | VALID |
| TD-V-002 | `bob+payments@zelletest.invalid` | Plus-tag addressing | VALID |
| TD-V-003 | `carol_smith@zelletest.invalid` | Underscore in local part | VALID |
| TD-V-004 | `david-jones@testbank.invalid` | Hyphen in local part | VALID |
| TD-V-005 | `eve123@testbank.invalid` | Alphanumeric local part | VALID |
| TD-V-006 | `frank@sub.testbank.invalid` | Subdomain | VALID |
| TD-V-007 | `grace@testbank.co.invalid` | Multi-part TLD | VALID |
| TD-V-008 | `henry.o'brien@testbank.invalid` | Apostrophe (RFC-allowed special char) | VALID |
| TD-V-009 | `isabelle!#$%@testbank.invalid` | RFC-allowed special chars in local | VALID |
| TD-V-010 | `j@testbank.invalid` | Single-char local part | VALID |
| TD-V-011 | `user.name.middle@testbank.invalid` | Multiple dots in local | VALID |
| TD-V-012 | `UPPERCASE@TESTBANK.INVALID` | Uppercase (case-insensitive) | VALID |
| TD-V-013 | `a`.repeat(64)_`@testbank.invalid` | Local part exactly 64 chars | VALID |
| TD-V-014 | `user@xn--n3h.ws` | IDN punycode domain | VALID |

---

## 2. Invalid Email Addresses — Format Fails

| TD# | Email | Error Code Expected | Description |
|-----|-------|--------------------|----|
| TD-I-001 | `` (empty string) | `EMPTY_EMAIL` | No input |
| TD-I-002 | `   ` | `EMPTY_EMAIL` | Whitespace only |
| TD-I-003 | `alicetestbank.invalid` | `MISSING_AT_SYMBOL` | No @ symbol |
| TD-I-004 | `alice@@testbank.invalid` | `MULTIPLE_AT_SYMBOLS` | Double @ |
| TD-I-005 | `alice@name@testbank.invalid` | `MULTIPLE_AT_SYMBOLS` | Two @ symbols |
| TD-I-006 | `@testbank.invalid` | `MISSING_AT_SYMBOL` | Empty local part |
| TD-I-007 | `alice@` | `MISSING_DOMAIN` | No domain after @ |
| TD-I-008 | `alice@testbank` | `MISSING_TLD` | No TLD dot |
| TD-I-009 | `.alice@testbank.invalid` | `STARTS_ENDS_WITH_DOT` | Local starts with dot |
| TD-I-010 | `alice.@testbank.invalid` | `STARTS_ENDS_WITH_DOT` | Local ends with dot |
| TD-I-011 | `alice..bob@testbank.invalid` | `CONSECUTIVE_DOTS` | Consecutive dots in local |
| TD-I-012 | `alice@testbank..invalid` | `CONSECUTIVE_DOTS` | Consecutive dots in domain |
| TD-I-013 | `a`.repeat(65)_`@testbank.invalid` | `LOCAL_PART_TOO_LONG` | 65-char local part |
| TD-I-014 | (254+ char total email) | `TOTAL_LENGTH_EXCEEDED` | Over RFC limit |
| TD-I-015 | `alice johnson@testbank.invalid` | `INVALID_CHARACTERS` | Space in local part |
| TD-I-016 | `alice\t@testbank.invalid` | `INVALID_CHARACTERS` | Tab character |
| TD-I-017 | `alice\0@testbank.invalid` | `INVALID_CHARACTERS` | Null byte |
| TD-I-018 | `alice😀@testbank.invalid` | `INVALID_CHARACTERS` | Emoji |
| TD-I-019 | `alice\r\n@testbank.invalid` | `INVALID_CHARACTERS` | CRLF injection attempt |
| TD-I-020 | `alice<script>@testbank.invalid` | `INVALID_CHARACTERS` | XSS attempt in local |

---

## 3. Security / Injection Inputs — Sanitiser Must Clean

| TD# | Raw Input | After Sanitise | Notes |
|-----|-----------|---------------|-------|
| TD-S-001 | `<script>alert('xss')</script>alice@testbank.invalid` | `alert('xss')alice@testbank.invalid` | Script tag stripped |
| TD-S-002 | `  alice@testbank.invalid  ` | `alice@testbank.invalid` | Whitespace trimmed |
| TD-S-003 | `alice\u0000@testbank.invalid` | `alice@testbank.invalid` | Null byte removed |
| TD-S-004 | `alice\u001F@testbank.invalid` | `alice@testbank.invalid` | Control char removed |
| TD-S-005 | `alice'; DROP TABLE customers;--@testbank.invalid` | flagged as dangerous | SQL injection detected |
| TD-S-006 | `alice@testbank.invalid` + 300 extra chars | Truncated to 320 chars | Hard length cap |
| TD-S-007 | `javascript:alert(1)@testbank.invalid` | flagged as dangerous | JS injection detected |

---

## 4. Zelle Registration Check Data

| TD# | Email | Zelle Stub Response | Expected `ZelleCheckResponse` |
|-----|-------|--------------------|-----------------------------|
| TD-Z-001 | `alice.johnson@testbank.invalid` | `FOUND` / `ACTIVE` | `REGISTERED`, `canProceed=true`, `requiresAcknowledgement=false` |
| TD-Z-002 | `bob.unknown@testbank.invalid` | `NOT_FOUND` | `NOT_REGISTERED`, `canProceed=false` |
| TD-Z-003 | `carol@testbank.invalid` | API timeout (5s) | `UNKNOWN`, `canProceed=true`, `requiresAcknowledgement=true` |
| TD-Z-004 | `david@testbank.invalid` | HTTP 503 from Zelle | `UNKNOWN`, `canProceed=true`, `requiresAcknowledgement=true` |
| TD-Z-005 | `eve@testbank.invalid` | HTTP 400 from Zelle | `UNKNOWN`, graceful degradation |
| TD-Z-006 | `frank@testbank.invalid` | Circuit breaker open | `UNKNOWN`, fallback invoked |
| TD-Z-007 | `alice.johnson@testbank.invalid` (2nd call) | Cache hit | `REGISTERED` (no new API call) |

---

## 5. API Request/Response Examples

### 5.1 Valid Format — POST /api/v1/email/validate

**Request:**
```json
{
  "email": "alice.johnson@testbank.invalid",
  "sessionId": "sess-synth-0001"
}
```

**Response:**
```json
{
  "valid": true,
  "errorCode": null,
  "errorMessage": null
}
```

---

### 5.2 Invalid Format — Missing @ Symbol

**Request:**
```json
{
  "email": "alicetestbank.invalid",
  "sessionId": "sess-synth-0002"
}
```

**Response:**
```json
{
  "valid": false,
  "errorCode": "MISSING_AT_SYMBOL",
  "errorMessage": "Email address must include an '@' symbol."
}
```

---

### 5.3 Zelle Check — Registered

**Request:**
```json
{
  "email": "alice.johnson@testbank.invalid",
  "sessionId": "sess-synth-0003"
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

---

### 5.4 Zelle Check — Not Registered

**Request:**
```json
{
  "email": "bob.unknown@testbank.invalid",
  "sessionId": "sess-synth-0004"
}
```

**Response:**
```json
{
  "registrationStatus": "NOT_REGISTERED",
  "canProceed": false,
  "message": "This email address is not registered with Zelle. Please check the address or ask the recipient to enroll.",
  "requiresAcknowledgement": false
}
```

---

### 5.5 Zelle Check — API Unavailable (Graceful Degradation)

**Request:**
```json
{
  "email": "carol@testbank.invalid",
  "sessionId": "sess-synth-0005"
}
```

**Response:**
```json
{
  "registrationStatus": "UNKNOWN",
  "canProceed": true,
  "message": "Unable to verify Zelle registration. You may still proceed.",
  "requiresAcknowledgement": true
}
```

---

## 6. Database Seed Data (email_validation_audit)

```sql
-- Synthetic audit records for QA environment
INSERT INTO email_validation_audit
  (audit_id, session_id, customer_id, email_hash, validation_type, validation_result, error_code, latency_ms, validated_at, platform)
VALUES
  (1,  'sess-synth-0001', 10001, '2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824', 'FORMAT',       'VALID',           null,                  12,   '2026-03-12 10:00:00+00', 'IOS'),
  (2,  'sess-synth-0001', 10001, '2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824', 'ZELLE_CHECK',  'REGISTERED',      null,                  245,  '2026-03-12 10:00:01+00', 'IOS'),
  (3,  'sess-synth-0002', 10002, 'f9af0678afb02e8f75bdd1d88d10c96f7c6a2f72b4a3a5a6e8e6e7c8b5a3d1e2', 'FORMAT',       'INVALID',         'MISSING_AT_SYMBOL',   8,    '2026-03-12 10:01:00+00', 'ANDROID'),
  (4,  'sess-synth-0003', 10003, 'a4e1b2c3d5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2', 'FORMAT',       'VALID',           null,                  10,   '2026-03-12 10:02:00+00', 'WEB'),
  (5,  'sess-synth-0003', 10003, 'a4e1b2c3d5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2', 'ZELLE_CHECK',  'NOT_REGISTERED',  null,                  1893, '2026-03-12 10:02:02+00', 'WEB'),
  (6,  'sess-synth-0004', 10004, 'b5c2d3e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2', 'FORMAT',       'VALID',           null,                  11,   '2026-03-12 10:03:00+00', 'IOS'),
  (7,  'sess-synth-0004', 10004, 'b5c2d3e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2', 'ZELLE_CHECK',  'UNKNOWN',         null,                  5001, '2026-03-12 10:03:06+00', 'IOS'),
  (8,  'sess-synth-0005', 10005, 'c6d3e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c3d4', 'FORMAT',       'INVALID',         'CONSECUTIVE_DOTS',    7,    '2026-03-12 10:04:00+00', 'ANDROID'),
  (9,  'sess-synth-0006', 10006, 'd7e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2d3e4', 'FORMAT',       'INVALID',         'TOTAL_LENGTH_EXCEEDED', 5,  '2026-03-12 10:05:00+00', 'WEB'),
  (10, 'sess-synth-0007', 10001, '2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824', 'ZELLE_CHECK',  'REGISTERED',      null,                  18,   '2026-03-12 10:06:00+00', 'IOS');
-- Note: audit_id=10 is a cache HIT — same hash as audit_id=2, low latency (18ms vs 245ms)
```

---

## 7. Database Seed Data (zelle_registry_cache)

```sql
INSERT INTO zelle_registry_cache (email_hash, registration_status, cached_at, expires_at, lookup_count)
VALUES
  ('2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824', 'REGISTERED',     '2026-03-12 10:00:01+00', '2026-03-12 10:01:01+00', 2),
  ('a4e1b2c3d5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2', 'NOT_REGISTERED', '2026-03-12 10:02:02+00', '2026-03-12 10:03:02+00', 1),
  ('b5c2d3e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2', 'UNKNOWN',        '2026-03-12 10:03:06+00', '2026-03-12 10:04:06+00', 1);
```

---

## 8. Database Seed Data (validation_error_catalogue)

```sql
INSERT INTO validation_error_catalogue (error_code, message_en, category, display_order, is_active)
VALUES
  ('EMPTY_EMAIL',            'Please enter an email address.',                               'FORMAT',       1,  true),
  ('MISSING_AT_SYMBOL',      'Email address must include an ''@'' symbol.',                  'FORMAT',       2,  true),
  ('MULTIPLE_AT_SYMBOLS',    'Email address can only contain one ''@'' symbol.',             'FORMAT',       3,  true),
  ('MISSING_DOMAIN',         'Please include a domain after ''@'' (e.g., example.com).',    'FORMAT',       4,  true),
  ('MISSING_TLD',            'Email domain must include a valid extension (e.g., .com).',   'FORMAT',       5,  true),
  ('LOCAL_PART_TOO_LONG',    'The part before ''@'' cannot exceed 64 characters.',          'FORMAT',       6,  true),
  ('TOTAL_LENGTH_EXCEEDED',  'Email address is too long (max 254 characters).',             'FORMAT',       7,  true),
  ('INVALID_CHARACTERS',     'Email address contains invalid characters.',                  'FORMAT',       8,  true),
  ('STARTS_ENDS_WITH_DOT',   'Email address cannot start or end with a dot.',               'FORMAT',       9,  true),
  ('CONSECUTIVE_DOTS',       'Email address cannot contain consecutive dots.',              'FORMAT',       10, true),
  ('NOT_REGISTERED_WITH_ZELLE',
   'This email address is not registered with Zelle. Please check the address or ask the recipient to enroll.',
   'REGISTRATION', 11, true);
```

---

## 9. Synthetic Customers (QA Environment)

| Customer ID | Name | Session IDs Used | Notes |
|------------|------|-----------------|-------|
| 10001 | Alice Johnson (synthetic) | sess-synth-0001, sess-synth-0007 | Registered Zelle user |
| 10002 | Bob Martinez (synthetic) | sess-synth-0002 | Enters invalid email |
| 10003 | Carol Nguyen (synthetic) | sess-synth-0003 | Recipient not in Zelle |
| 10004 | David Park (synthetic) | sess-synth-0004 | Zelle API timeout scenario |
| 10005 | Eve Thompson (synthetic) | sess-synth-0005 | Consecutive dots error |
| 10006 | Frank Williams (synthetic) | sess-synth-0006 | Email too long error |
