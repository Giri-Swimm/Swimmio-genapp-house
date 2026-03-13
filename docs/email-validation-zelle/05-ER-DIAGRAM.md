# ER Diagram
## Feature: Email Address Validation for Zelle Transfers

**Document ID:** iPYMT-ER-EMAILVAL-001
**Version:** 1.0
**Date:** 2026-03-12

---

## 1. Entity-Relationship Diagram (Mermaid)

```mermaid
erDiagram

    CUSTOMER {
        BIGINT      customer_id         PK
        VARCHAR(100) first_name
        VARCHAR(100) last_name
        VARCHAR(254) email_address
        VARCHAR(20)  phone_number
        TIMESTAMP    created_at
        TIMESTAMP    updated_at
        BOOLEAN      is_active
    }

    SESSION {
        VARCHAR(64)  session_id          PK
        BIGINT       customer_id         FK
        VARCHAR(45)  ip_address
        VARCHAR(200) user_agent
        TIMESTAMP    created_at
        TIMESTAMP    expires_at
        BOOLEAN      is_active
    }

    EMAIL_VALIDATION_AUDIT {
        BIGINT       audit_id            PK
        VARCHAR(64)  session_id          FK
        BIGINT       customer_id         FK
        VARCHAR(64)  email_hash          "SHA-256 of email — no PII"
        VARCHAR(50)  validation_type     "FORMAT | ZELLE_CHECK"
        VARCHAR(50)  validation_result   "VALID | INVALID | REGISTERED | NOT_REGISTERED | UNKNOWN"
        VARCHAR(50)  error_code          "nullable"
        BIGINT       latency_ms
        TIMESTAMP    validated_at
        VARCHAR(20)  platform            "WEB | IOS | ANDROID"
    }

    ZELLE_REGISTRY_CACHE {
        VARCHAR(64)  email_hash          PK   "SHA-256 of normalised email"
        VARCHAR(30)  registration_status      "REGISTERED | NOT_REGISTERED | UNKNOWN"
        TIMESTAMP    cached_at
        TIMESTAMP    expires_at
        INT          lookup_count
    }

    TRANSFER_ATTEMPT {
        BIGINT       attempt_id          PK
        BIGINT       customer_id         FK
        VARCHAR(64)  session_id          FK
        BIGINT       audit_id            FK   "last validation audit"
        VARCHAR(64)  recipient_email_hash     "SHA-256 — no PII"
        VARCHAR(20)  transfer_status          "PENDING | COMPLETED | FAILED | CANCELLED"
        DECIMAL(15,2) amount
        VARCHAR(3)   currency
        TIMESTAMP    initiated_at
        TIMESTAMP    completed_at
        BOOLEAN      zelle_check_bypassed     "true if API was unavailable"
    }

    VALIDATION_ERROR_CATALOGUE {
        VARCHAR(50)  error_code          PK
        VARCHAR(300) message_en
        VARCHAR(300) message_es
        VARCHAR(50)  category            "FORMAT | REGISTRATION | SYSTEM"
        INT          display_order
        BOOLEAN      is_active
        TIMESTAMP    created_at
        TIMESTAMP    updated_at
    }

    CUSTOMER         ||--o{ SESSION              : "has"
    CUSTOMER         ||--o{ EMAIL_VALIDATION_AUDIT : "triggers"
    CUSTOMER         ||--o{ TRANSFER_ATTEMPT     : "initiates"
    SESSION          ||--o{ EMAIL_VALIDATION_AUDIT : "contains"
    SESSION          ||--o{ TRANSFER_ATTEMPT     : "scopes"
    EMAIL_VALIDATION_AUDIT ||--o| TRANSFER_ATTEMPT : "informs"
    ZELLE_REGISTRY_CACHE   }|--|| EMAIL_VALIDATION_AUDIT : "serves"
    VALIDATION_ERROR_CATALOGUE ||--o{ EMAIL_VALIDATION_AUDIT : "referenced by"
```

---

## 2. Entity Descriptions

### 2.1 CUSTOMER
Represents the authenticated iPYMT user. Stores identity and contact details.
- `customer_id` — surrogate primary key.
- `email_address` — the customer's own email; not the transfer recipient's email.
- No recipient email is stored in plain text anywhere in the schema.

### 2.2 SESSION
Represents a user authentication session in iPYMT.
- `session_id` — UUID string; used as the non-PII correlation key across audit logs.
- Foreign key to `CUSTOMER`.
- `expires_at` — used to enforce session validity on all API calls.

### 2.3 EMAIL_VALIDATION_AUDIT
Immutable audit log of every validation event.
- `email_hash` — SHA-256 of the normalised (lowercase, trimmed) email entered by the user. Never the raw email.
- `validation_type` — distinguishes client-side format checks from Zelle registry checks.
- `validation_result` — outcome of the check.
- `error_code` — FK reference to `VALIDATION_ERROR_CATALOGUE.error_code`; null for passing validations.
- `latency_ms` — for SLA monitoring of Zelle API calls.
- Append-only; no UPDATE or DELETE permitted.

### 2.4 ZELLE_REGISTRY_CACHE
Persistent cache (supplements in-memory Caffeine cache for cross-instance sharing).
- `email_hash` — PK; SHA-256 of normalised email.
- `expires_at` — TTL enforced by scheduled cleanup job (runs every 5 minutes).
- `lookup_count` — telemetry: how many times this hash has been looked up.

### 2.5 TRANSFER_ATTEMPT
Records each attempt to send a Zelle transfer from the iPYMT application.
- `recipient_email_hash` — SHA-256 of recipient's email; no plain text.
- `audit_id` — links to the most recent validation audit that preceded this attempt.
- `zelle_check_bypassed` — flag set to `true` when Zelle API was unavailable and user acknowledged.

### 2.6 VALIDATION_ERROR_CATALOGUE
Reference table of all validation error codes and their localised messages.
- Managed by the admin portal; updated without code deployment.
- `display_order` — controls order of display when multiple errors could apply.

---

## 3. DDL (PostgreSQL)

```sql
-- CUSTOMER (excerpt: relevant columns)
CREATE TABLE customer (
    customer_id  BIGSERIAL PRIMARY KEY,
    first_name   VARCHAR(100) NOT NULL,
    last_name    VARCHAR(100) NOT NULL,
    email_address VARCHAR(254) NOT NULL,
    phone_number  VARCHAR(20),
    created_at    TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    updated_at    TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    is_active     BOOLEAN NOT NULL DEFAULT TRUE
);

-- SESSION
CREATE TABLE session (
    session_id   VARCHAR(64)  PRIMARY KEY,
    customer_id  BIGINT       NOT NULL REFERENCES customer(customer_id),
    ip_address   VARCHAR(45),
    user_agent   VARCHAR(200),
    created_at   TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    expires_at   TIMESTAMP WITH TIME ZONE NOT NULL,
    is_active    BOOLEAN NOT NULL DEFAULT TRUE
);

-- EMAIL_VALIDATION_AUDIT
CREATE TABLE email_validation_audit (
    audit_id          BIGSERIAL PRIMARY KEY,
    session_id        VARCHAR(64)  NOT NULL REFERENCES session(session_id),
    customer_id       BIGINT       NOT NULL REFERENCES customer(customer_id),
    email_hash        VARCHAR(64)  NOT NULL,
    validation_type   VARCHAR(50)  NOT NULL CHECK (validation_type IN ('FORMAT','ZELLE_CHECK')),
    validation_result VARCHAR(50)  NOT NULL,
    error_code        VARCHAR(50)  REFERENCES validation_error_catalogue(error_code),
    latency_ms        BIGINT,
    validated_at      TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    platform          VARCHAR(20)  CHECK (platform IN ('WEB','IOS','ANDROID'))
);
-- Append-only enforcement
CREATE RULE no_update_audit AS ON UPDATE TO email_validation_audit DO INSTEAD NOTHING;
CREATE RULE no_delete_audit AS ON DELETE TO email_validation_audit DO INSTEAD NOTHING;
CREATE INDEX idx_audit_session    ON email_validation_audit(session_id);
CREATE INDEX idx_audit_customer   ON email_validation_audit(customer_id);
CREATE INDEX idx_audit_email_hash ON email_validation_audit(email_hash);

-- ZELLE_REGISTRY_CACHE
CREATE TABLE zelle_registry_cache (
    email_hash          VARCHAR(64)  PRIMARY KEY,
    registration_status VARCHAR(30)  NOT NULL CHECK (registration_status IN ('REGISTERED','NOT_REGISTERED','UNKNOWN')),
    cached_at           TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    expires_at          TIMESTAMP WITH TIME ZONE NOT NULL,
    lookup_count        INT NOT NULL DEFAULT 1
);
CREATE INDEX idx_zelle_cache_expires ON zelle_registry_cache(expires_at);

-- TRANSFER_ATTEMPT
CREATE TABLE transfer_attempt (
    attempt_id            BIGSERIAL PRIMARY KEY,
    customer_id           BIGINT       NOT NULL REFERENCES customer(customer_id),
    session_id            VARCHAR(64)  NOT NULL REFERENCES session(session_id),
    audit_id              BIGINT       REFERENCES email_validation_audit(audit_id),
    recipient_email_hash  VARCHAR(64)  NOT NULL,
    transfer_status       VARCHAR(20)  NOT NULL DEFAULT 'PENDING'
                              CHECK (transfer_status IN ('PENDING','COMPLETED','FAILED','CANCELLED')),
    amount                DECIMAL(15,2) NOT NULL CHECK (amount > 0),
    currency              VARCHAR(3)   NOT NULL DEFAULT 'USD',
    initiated_at          TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    completed_at          TIMESTAMP WITH TIME ZONE,
    zelle_check_bypassed  BOOLEAN NOT NULL DEFAULT FALSE
);
CREATE INDEX idx_transfer_customer ON transfer_attempt(customer_id);
CREATE INDEX idx_transfer_session  ON transfer_attempt(session_id);

-- VALIDATION_ERROR_CATALOGUE
CREATE TABLE validation_error_catalogue (
    error_code     VARCHAR(50)  PRIMARY KEY,
    message_en     VARCHAR(300) NOT NULL,
    message_es     VARCHAR(300),
    category       VARCHAR(50)  NOT NULL CHECK (category IN ('FORMAT','REGISTRATION','SYSTEM')),
    display_order  INT NOT NULL DEFAULT 0,
    is_active      BOOLEAN NOT NULL DEFAULT TRUE,
    created_at     TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    updated_at     TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);
```

---

## 4. Relationships Summary

| From Entity | Cardinality | To Entity | Description |
|------------|-------------|-----------|-------------|
| CUSTOMER | 1 : M | SESSION | A customer may have many sessions |
| CUSTOMER | 1 : M | EMAIL_VALIDATION_AUDIT | A customer triggers many audits |
| CUSTOMER | 1 : M | TRANSFER_ATTEMPT | A customer initiates many transfers |
| SESSION | 1 : M | EMAIL_VALIDATION_AUDIT | A session has many validation events |
| SESSION | 1 : M | TRANSFER_ATTEMPT | A session may initiate many transfer attempts |
| EMAIL_VALIDATION_AUDIT | 1 : 0..1 | TRANSFER_ATTEMPT | One audit record informs zero or one transfer |
| ZELLE_REGISTRY_CACHE | M : M (logical) | EMAIL_VALIDATION_AUDIT | Cache serves many audit lookups for same hash |
| VALIDATION_ERROR_CATALOGUE | 1 : M | EMAIL_VALIDATION_AUDIT | One error code referenced by many audit rows |
