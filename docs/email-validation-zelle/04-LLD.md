# Low-Level Design (LLD)
## Feature: Email Address Validation for Zelle Transfers

**Document ID:** iPYMT-LLD-EMAILVAL-001
**Version:** 1.0
**Date:** 2026-03-12

---

## 1. Package Structure

```
com.ipymt.zelle.emailvalidation
├── controller
│   └── EmailValidationController.java
├── service
│   ├── EmailValidationService.java
│   ├── ZelleRegistryService.java
│   └── EmailSanitiser.java
├── validator
│   └── EmailValidator.java
├── client
│   └── ZelleRegistryClient.java
├── model
│   ├── dto
│   │   ├── EmailValidationRequest.java
│   │   ├── EmailValidationResponse.java
│   │   ├── ZelleCheckRequest.java
│   │   └── ZelleCheckResponse.java
│   └── enums
│       ├── ValidationErrorCode.java
│       ├── ZelleRegistrationStatus.java
│       └── ValidationState.java
├── resolver
│   └── ErrorMessageResolver.java
├── config
│   ├── ZelleClientConfig.java
│   └── CacheConfig.java
├── audit
│   └── ValidationAuditLogger.java
└── exception
    ├── EmailValidationException.java
    └── ZelleApiException.java
```

---

## 2. Class Designs

### 2.1 EmailValidator

**Responsibility:** Stateless utility that checks email format against RFC 5322 simplified rules.

```
Class: EmailValidator
Package: com.ipymt.zelle.emailvalidation.validator

Fields:
  - EMAIL_PATTERN : Pattern  (compiled regex, static final)
  - LOCAL_MAX     : int = 64
  - TOTAL_MAX     : int = 254

Methods:
  + isValidFormat(String email) : ValidationResult
      - Trim whitespace
      - Check null / blank → EMPTY_EMAIL
      - Check total length ≤ 254 → TOTAL_LENGTH_EXCEEDED
      - Reject control chars / emoji via char scan → INVALID_CHARACTERS
      - Count '@': zero → MISSING_AT_SYMBOL, >1 → MULTIPLE_AT_SYMBOLS
      - Split on '@': local part and domain
      - Check local length ≤ 64 → LOCAL_PART_TOO_LONG
      - Check local starts/ends with dot → STARTS_ENDS_WITH_DOT
      - Check consecutive dots in local or domain → CONSECUTIVE_DOTS
      - Apply full EMAIL_PATTERN match
      - Return ValidationResult.valid() or ValidationResult.invalid(errorCode)

  - buildPattern() : Pattern  (private, called once at class-load)

Data Types:
  ValidationResult {
    boolean valid;
    ValidationErrorCode errorCode;   // null when valid
  }
```

**Regex (EMAIL_PATTERN):**
```
^[a-zA-Z0-9!#$%&'*+/=?^_`{|}~-]+
  (?:\.[a-zA-Z0-9!#$%&'*+/=?^_`{|}~-]+)*
@
(?:[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?\.)+
[a-zA-Z]{2,}$
```

---

### 2.2 EmailSanitiser

**Responsibility:** Strips dangerous input before processing.

```
Class: EmailSanitiser
Package: com.ipymt.zelle.emailvalidation.service

Methods:
  + sanitise(String rawInput) : String
      - Return empty string if null
      - Trim leading/trailing whitespace
      - Remove control characters (0x00–0x1F, 0x7F)
      - Remove HTML tags via regex <[^>]*>
      - Hard truncate to 320 characters
      - Return sanitised string

  + containsDangerousPatterns(String input) : boolean
      - Detect SQL injection keywords pattern
      - Detect script injection patterns
      - Used in server-side guard; throws EmailValidationException if true
```

---

### 2.3 ZelleRegistryService

**Responsibility:** Orchestrates Zelle registration check with caching and resilience.

```
Class: ZelleRegistryService
Package: com.ipymt.zelle.emailvalidation.service

Dependencies (injected):
  - ZelleRegistryClient zelleClient
  - Cache<String, ZelleRegistrationStatus> registrationCache
  - ValidationAuditLogger auditLogger

Methods:
  + checkRegistration(String email) : ZelleRegistrationStatus
      1. Compute hash = SHA-256(email.toLowerCase().trim())
      2. Check cache.getIfPresent(hash)
         - HIT  → log cache hit, return cached status
         - MISS → call zelleClient.lookup(hash)
      3. On success: cache.put(hash, status, TTL=60s); return status
      4. On ZelleApiException / timeout: log warning; return UNKNOWN
      5. On circuit breaker open: log warn; return UNKNOWN

  - computeSha256(String input) : String  (private)
      - MessageDigest SHA-256
      - Hex encode result
```

---

### 2.4 ZelleRegistryClient

**Responsibility:** HTTP client for Zelle EWS API.

```
Class: ZelleRegistryClient
Package: com.ipymt.zelle.emailvalidation.client

Dependencies:
  - WebClient zelleWebClient  (configured in ZelleClientConfig)

Annotations:
  - @CircuitBreaker(name="zelleRegistry", fallbackMethod="fallback")
  - @TimeLimiter(name="zelleRegistry")

Methods:
  + lookup(String hashedEmail) : ZelleRegistrationStatus
      POST {zelleBaseUrl}/v1/recipient/lookup
      Body: { "tokenizedEmail": hashedEmail }
      Headers: Authorization: Bearer {apiKey}
      Timeout: 5s
      Retry: 1 time on 5xx
      Parse response → map to ZelleRegistrationStatus

  - fallback(String hashedEmail, Throwable t) : ZelleRegistrationStatus
      log.warn("Zelle API unavailable: {}", t.getMessage())
      return ZelleRegistrationStatus.UNKNOWN
```

---

### 2.5 EmailValidationController

**Responsibility:** REST endpoints; input guard; delegates to services.

```
Class: EmailValidationController
Package: com.ipymt.zelle.emailvalidation.controller

Annotations: @RestController, @RequestMapping("/api/v1/email"), @Validated

Dependencies:
  - EmailValidationService validationService
  - ErrorMessageResolver messageResolver

Endpoints:

  POST /validate
  + validateFormat(@RequestBody @Valid EmailValidationRequest req) : ResponseEntity<EmailValidationResponse>
      1. sanitise req.email via EmailSanitiser
      2. call EmailValidator.isValidFormat(sanitised)
      3. Build EmailValidationResponse
      4. Return 200 OK with response

  POST /zelle-check
  + checkZelleRegistration(@RequestBody @Valid ZelleCheckRequest req) : ResponseEntity<ZelleCheckResponse>
      1. sanitise req.email
      2. Re-validate format; if invalid return 400
      3. Call ZelleRegistryService.checkRegistration(email)
      4. Build ZelleCheckResponse
      5. Return 200 OK

  Exception handlers:
  @ExceptionHandler(MethodArgumentNotValidException)  → 400
  @ExceptionHandler(RateLimitExceededException)       → 429
  @ExceptionHandler(ZelleApiException)                → 503
```

---

### 2.6 EmailValidationService

**Responsibility:** Orchestrates format validation and audit logging.

```
Class: EmailValidationService
Package: com.ipymt.zelle.emailvalidation.service

Dependencies:
  - EmailValidator emailValidator
  - EmailSanitiser sanitiser
  - ValidationAuditLogger auditLogger

Methods:
  + validate(String rawEmail, String sessionId) : EmailValidationResponse
      1. sanitised = sanitiser.sanitise(rawEmail)
      2. result = emailValidator.isValidFormat(sanitised)
      3. auditLogger.log(sessionId, result.getErrorCode(), isValid)
      4. Build and return EmailValidationResponse
```

---

### 2.7 ErrorMessageResolver

**Responsibility:** Maps `ValidationErrorCode` to locale-aware user-facing strings.

```
Class: ErrorMessageResolver
Package: com.ipymt.zelle.emailvalidation.resolver

Dependencies:
  - MessageSource messageSource  (Spring i18n)

Methods:
  + resolve(ValidationErrorCode code, Locale locale) : String
      return messageSource.getMessage("email.error." + code.name(), null, locale)
```

**Message bundle (messages.properties):**
```
email.error.MISSING_AT_SYMBOL=Email address must include an '@' symbol.
email.error.MULTIPLE_AT_SYMBOLS=Email address can only contain one '@' symbol.
email.error.MISSING_DOMAIN=Please include a domain after '@' (e.g., example.com).
email.error.MISSING_TLD=Email domain must include a valid extension (e.g., .com, .org).
email.error.LOCAL_PART_TOO_LONG=The part before '@' cannot exceed 64 characters.
email.error.TOTAL_LENGTH_EXCEEDED=Email address is too long (max 254 characters).
email.error.INVALID_CHARACTERS=Email address contains invalid characters.
email.error.STARTS_ENDS_WITH_DOT=Email address cannot start or end with a dot.
email.error.CONSECUTIVE_DOTS=Email address cannot contain consecutive dots.
email.error.EMPTY_EMAIL=Please enter an email address.
email.error.NOT_REGISTERED_WITH_ZELLE=This email address is not registered with Zelle. Please check the address or ask the recipient to enroll.
```

---

### 2.8 DTOs

```
EmailValidationRequest {
  @NotNull @Size(max=320)
  String email;
  String sessionId;
}

EmailValidationResponse {
  boolean valid;
  String  errorCode;     // null if valid
  String  errorMessage;  // null if valid
}

ZelleCheckRequest {
  @NotNull @Size(max=320)
  String email;
  String sessionId;
}

ZelleCheckResponse {
  ZelleRegistrationStatus registrationStatus;  // REGISTERED | NOT_REGISTERED | UNKNOWN
  boolean canProceed;
  String  message;
  boolean requiresAcknowledgement;
}
```

---

### 2.9 Enums

```
ValidationErrorCode {
  EMPTY_EMAIL,
  MISSING_AT_SYMBOL,
  MULTIPLE_AT_SYMBOLS,
  MISSING_DOMAIN,
  MISSING_TLD,
  LOCAL_PART_TOO_LONG,
  TOTAL_LENGTH_EXCEEDED,
  INVALID_CHARACTERS,
  STARTS_ENDS_WITH_DOT,
  CONSECUTIVE_DOTS,
  NOT_REGISTERED_WITH_ZELLE
}

ZelleRegistrationStatus {
  REGISTERED,
  NOT_REGISTERED,
  UNKNOWN
}

ValidationState {      // Frontend state machine
  EMPTY,
  TYPING,
  FORMAT_VALID,
  FORMAT_INVALID,
  ZELLE_CHECKING,
  REGISTERED,
  NOT_REGISTERED,
  API_ERROR
}
```

---

### 2.10 CacheConfig

```
@Configuration
CacheConfig {
  @Bean
  Cache<String, ZelleRegistrationStatus> zelleRegistrationCache()
      return Caffeine.newBuilder()
          .maximumSize(10_000)
          .expireAfterWrite(60, TimeUnit.SECONDS)
          .recordStats()
          .build();
}
```

---

### 2.11 ValidationAuditLogger

```
Class: ValidationAuditLogger
Package: com.ipymt.zelle.emailvalidation.audit

Methods:
  + log(String sessionId, ValidationErrorCode errorCode, boolean isValid)
      MDC.put("sessionId", sessionId)
      log.info("EMAIL_VALIDATION sessionId={} valid={} errorCode={}", sessionId, isValid, errorCode)
      MDC.clear()

  + logZelleCheck(String sessionId, ZelleRegistrationStatus status, long latencyMs)
      log.info("ZELLE_CHECK sessionId={} status={} latencyMs={}", sessionId, status, latencyMs)
```

---

## 3. Sequence Diagram — Format Validation

```
Client          Controller          Service          Validator        AuditLogger
  |                  |                  |                  |               |
  |--POST /validate->|                  |                  |               |
  |                  |--sanitise()----->|                  |               |
  |                  |                  |--isValidFormat()->|               |
  |                  |                  |<--ValidationResult|               |
  |                  |                  |--log()--------------------------------->|
  |                  |<--EmailValidationResponse            |               |
  |<--200 OK---------|                  |                  |               |
```

---

## 4. Sequence Diagram — Zelle Registry Check

```
Client     Controller    Service         Client(HTTP)       ZelleEWS API     Cache
  |              |           |                |                    |            |
  |--POST /zelle-check->|   |                |                    |            |
  |              |--validate+sanitise->|      |                    |            |
  |              |           |--checkCache---------------------------->|        |
  |              |           |    MISS        |                    |    |       |
  |              |           |--SHA256+lookup->|                   |    |       |
  |              |           |                |--POST /lookup----->|    |       |
  |              |           |                |<--{status}---------|    |       |
  |              |           |<--status-------|                    |    |       |
  |              |           |--putCache----------------------------->| |       |
  |              |<--ZelleCheckResponse       |                    |            |
  |<--200 OK-----|           |                |                    |            |
```

---

## 5. Error Handling Strategy

| Layer | Error | Handling |
|-------|-------|---------|
| Controller | `MethodArgumentNotValidException` | 400 with field errors |
| Controller | `EmailValidationException` | 422 with error code |
| Controller | `ZelleApiException` | 503 with fallback message |
| Service | Zelle timeout | Return `UNKNOWN`, log, allow proceed |
| Service | Circuit open | Return `UNKNOWN`, log |
| Client | HTTP 4xx from Zelle API | Throw `ZelleApiException` |
| Client | HTTP 5xx from Zelle API | Retry once, then `ZelleApiException` |
