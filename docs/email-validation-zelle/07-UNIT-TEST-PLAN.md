# Unit Test Plan
## Feature: Email Address Validation for Zelle Transfers

**Document ID:** iPYMT-UTP-EMAILVAL-001
**Version:** 1.0
**Date:** 2026-03-12

---

## 1. Scope and Objectives

This plan covers all JUnit 5 unit tests for the backend Java service layer of the Email Validation feature. Each test is isolated using Mockito mocks. No real HTTP calls are made; Zelle EWS API interactions are stubbed.

**Test Framework Stack:**
- JUnit 5 (Jupiter)
- Mockito 5.x
- AssertJ 3.x
- Spring Boot Test (for integration slice tests)

---

## 2. Test Class Inventory

| Test Class | Class Under Test | Test Count |
|------------|-----------------|------------|
| `EmailValidatorTest` | `EmailValidator` | 22 |
| `EmailSanitiserTest` | `EmailSanitiser` | 9 |
| `ZelleRegistryServiceTest` | `ZelleRegistryService` | 9 |
| `EmailValidationServiceTest` | `EmailValidationService` | 9 |
| `ErrorMessageResolverTest` | `ErrorMessageResolver` | 3 |
| `EmailValidationControllerTest` | `EmailValidationController` | 8 |
| **Total** | | **60** |

---

## 3. Test Cases — EmailValidator

| TC# | Test Method | Input | Expected Outcome |
|-----|-------------|-------|-----------------|
| UV-001 | `shouldReturnValidForWellFormedEmails` | `user@example.com` | `valid=true, errorCode=null` |
| UV-002 | `shouldReturnValidForWellFormedEmails` | `user+tag@example.com` | `valid=true` |
| UV-003 | `shouldReturnValidForWellFormedEmails` | `user-name@sub.example.co.uk` | `valid=true` |
| UV-004 | `nullReturnsEmptyEmail` | `null` | `EMPTY_EMAIL` |
| UV-005 | `emptyStringReturnsEmptyEmail` | `""` | `EMPTY_EMAIL` |
| UV-006 | `whitespaceOnlyReturnsEmptyEmail` | `"   "` | `EMPTY_EMAIL` |
| UV-007 | `missingAtSymbol` | `userexample.com` | `MISSING_AT_SYMBOL` |
| UV-008 | `multipleAtSymbols` | `user@@example.com` | `MULTIPLE_AT_SYMBOLS` |
| UV-009 | `twoAtSymbolsInEmail` | `user@name@example.com` | `MULTIPLE_AT_SYMBOLS` |
| UV-010 | `localPartTooLong` | 65-char local + `@example.com` | `LOCAL_PART_TOO_LONG` |
| UV-011 | `localPartExactly64` | 64-char local + `@example.com` | `valid=true` |
| UV-012 | `totalLengthExceeded` | > 254-char email | `TOTAL_LENGTH_EXCEEDED` |
| UV-013 | `localStartsWithDot` | `.user@example.com` | `STARTS_ENDS_WITH_DOT` |
| UV-014 | `localEndsWithDot` | `user.@example.com` | `STARTS_ENDS_WITH_DOT` |
| UV-015 | `consecutiveDotsInLocal` | `user..name@example.com` | `CONSECUTIVE_DOTS` |
| UV-016 | `consecutiveDotsInDomain` | `user@example..com` | `CONSECUTIVE_DOTS` |
| UV-017 | `missingDomain` | `user@` | `MISSING_DOMAIN` |
| UV-018 | `missingTld` | `user@example` | `MISSING_TLD` |
| UV-019 | `controlCharTab` | `user\t@example.com` | `INVALID_CHARACTERS` |
| UV-020 | `nullByte` | `user\0@example.com` | `INVALID_CHARACTERS` |
| UV-021 | `spaceInEmail` | `user name@example.com` | `INVALID_CHARACTERS` |
| UV-022 | `emojiInEmail` | `user😀@example.com` | `INVALID_CHARACTERS` |

---

## 4. Test Cases — EmailSanitiser

| TC# | Test Method | Input | Expected Output |
|-----|-------------|-------|----------------|
| US-001 | `nullInputReturnsEmpty` | `null` | `""` |
| US-002 | `trimsWhitespace` | `"  user@example.com  "` | `"user@example.com"` |
| US-003 | `removesControlCharacters` | `"user\0\x1F@example.com"` | `"user@example.com"` |
| US-004 | `stripsHtmlTags` | `"<script>alert(1)</script>user@example.com"` | `"alert(1)user@example.com"` |
| US-005 | `truncatesOverLength` | 400-char string | length == 320 |
| US-006 | `normalEmailUnchanged` | `"user@example.com"` | `"user@example.com"` |
| US-007 | `detectsScriptInjection` | `"<script>alert()</script>"` | `containsDangerousPatterns = true` |
| US-008 | `detectsSqlInjection` | `"user'; drop table users--"` | `containsDangerousPatterns = true` |
| US-009 | `cleanInputReturnsFalse` | `"user@example.com"` | `containsDangerousPatterns = false` |

---

## 5. Test Cases — ZelleRegistryService

| TC# | Test Method | Stub Setup | Expected Outcome |
|-----|-------------|-----------|-----------------|
| UZ-001 | `returnsRegistered` | client returns REGISTERED | `REGISTERED` |
| UZ-002 | `returnsNotRegistered` | client returns NOT_REGISTERED | `NOT_REGISTERED` |
| UZ-003 | `returnsUnknownOnApiException` | client throws ZelleApiException | `UNKNOWN` |
| UZ-004 | `returnsUnknownOnRuntimeException` | client throws RuntimeException | `UNKNOWN` |
| UZ-005 | `cacheHitSkipsClientCall` | two calls same email | client invoked only once |
| UZ-006 | `differentEmailsHaveIndependentCacheEntries` | two different emails | client invoked twice, correct statuses |
| UZ-007 | `sha256IsCaseInsensitive` | `user@example.com` vs `USER@EXAMPLE.COM` | hashes are equal |
| UZ-008 | `sha256HasCorrectLength` | `user@example.com` | 64 hex chars |
| UZ-009 | `auditLoggerCalled` | client returns REGISTERED | `auditLogger.logZelleCheck(...)` called |

---

## 6. Test Cases — EmailValidationService

| TC# | Test Method | Stub Setup | Expected Outcome |
|-----|-------------|-----------|-----------------|
| SV-001 | `validateReturnsValidForGoodEmail` | validator returns valid | `{valid:true}` |
| SV-002 | `validateReturnsInvalidForBadEmail` | validator returns MISSING_AT_SYMBOL | `{valid:false, errorCode:"MISSING_AT_SYMBOL"}` |
| SV-003 | `validateCallsAuditLogger` | valid email | `auditLogger.log(...)` called |
| SV-004 | `validateSanitisesFirst` | whitespace input | sanitiser called before validator |
| SV-005 | `zelleCheckReturnsRegistered` | Zelle returns REGISTERED | `canProceed=true, requiresAcknowledgement=false` |
| SV-006 | `zelleCheckReturnsNotRegistered` | Zelle returns NOT_REGISTERED | `canProceed=false` |
| SV-007 | `zelleCheckReturnsUnknownOnApiFailure` | Zelle returns UNKNOWN | `canProceed=true, requiresAcknowledgement=true` |
| SV-008 | `zelleCheckReturnsFalseIfFormatInvalid` | validator returns invalid | Zelle service never called |

---

## 7. Test Cases — ErrorMessageResolver

| TC# | Test Method | Input | Expected Output |
|-----|-------------|-------|----------------|
| UR-001 | `resolvesKnownCode` | `MISSING_AT_SYMBOL`, `Locale.ENGLISH` | `"Email address must include an '@' symbol."` |
| UR-002 | `returnsEmptyForNullCode` | `null`, `Locale.ENGLISH` | `""` |
| UR-003 | `fallsBackToCodeNameForUnknownKey` | unknown code, ENGLISH | code name string returned |

---

## 8. Test Cases — EmailValidationController (Slice Tests)

| TC# | Test Method | HTTP Request | Expected Response |
|-----|-------------|-------------|------------------|
| UC-001 | `validateReturns200ForValidEmail` | POST /validate `{email:"user@example.com"}` | 200 `{valid:true}` |
| UC-002 | `validateReturns200WithErrorForInvalidEmail` | POST /validate `{email:"bad"}` | 200 `{valid:false, errorCode:"..."}` |
| UC-003 | `validateReturns400ForNullEmail` | POST /validate `{email:null}` | 400 |
| UC-004 | `zelleCheckReturns200Registered` | POST /zelle-check `{email:"user@example.com"}` | 200 REGISTERED |
| UC-005 | `zelleCheckReturns200NotRegistered` | POST /zelle-check `{email:"unknown@example.com"}` | 200 NOT_REGISTERED |
| UC-006 | `zelleCheckReturns200UnknownOnApiError` | POST /zelle-check (API fails) | 200 UNKNOWN |
| UC-007 | `validateReturns400ForMissingBody` | POST /validate (empty body) | 400 |
| UC-008 | `zelleCheckReturns400ForNullEmail` | POST /zelle-check `{email:null}` | 400 |

---

## 9. Coverage Targets

| Package | Line Coverage Target | Branch Coverage Target |
|---------|---------------------|----------------------|
| `validator` | 100% | 100% |
| `service` | 95% | 90% |
| `client` | 85% | 80% |
| `controller` | 90% | 85% |
| `resolver` | 100% | 100% |
| **Overall** | **95%** | **90%** |

---

## 10. Test Execution Commands

```bash
# Run all unit tests
mvn test

# Run a specific test class
mvn test -Dtest=EmailValidatorTest

# Run with coverage report (JaCoCo)
mvn test jacoco:report

# View coverage report
open target/site/jacoco/index.html
```

---

## 11. CI Gate

All tests must pass and coverage thresholds must be met before a pull request can be merged. The JaCoCo Maven plugin is configured to fail the build if overall line coverage drops below 95%.

```xml
<!-- pom.xml excerpt -->
<plugin>
    <groupId>org.jacoco</groupId>
    <artifactId>jacoco-maven-plugin</artifactId>
    <configuration>
        <rules>
            <rule>
                <limits>
                    <limit>
                        <counter>LINE</counter>
                        <value>COVEREDRATIO</value>
                        <minimum>0.95</minimum>
                    </limit>
                </limits>
            </rule>
        </rules>
    </configuration>
</plugin>
```
