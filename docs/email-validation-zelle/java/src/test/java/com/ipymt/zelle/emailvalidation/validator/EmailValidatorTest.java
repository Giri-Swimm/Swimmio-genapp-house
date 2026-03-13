package com.ipymt.zelle.emailvalidation.validator;

import com.ipymt.zelle.emailvalidation.model.enums.ValidationErrorCode;
import com.ipymt.zelle.emailvalidation.validator.EmailValidator.ValidationResult;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit tests for {@link EmailValidator}.
 * Covers all format rules defined in FR-EV-01 and FR-EV-05.
 */
@DisplayName("EmailValidator")
class EmailValidatorTest {

    private EmailValidator validator;

    @BeforeEach
    void setUp() {
        validator = new EmailValidator();
    }

    // -----------------------------------------------------------------------
    // Valid Email Addresses
    // -----------------------------------------------------------------------

    @Nested
    @DisplayName("Valid email addresses")
    class ValidEmails {

        @ParameterizedTest(name = "should accept [{0}]")
        @ValueSource(strings = {
            "user@example.com",
            "User@Example.COM",
            "user.name@example.com",
            "user+tag@example.com",
            "user-name@sub.example.co.uk",
            "user_name@example.org",
            "user123@example.net",
            "a@b.io",
            "user!#$%&'*+/=?^_`{|}~@example.com",
            "123@456.com",
            "user@xn--n3h.ws"  // IDN punycode
        })
        void shouldReturnValidForWellFormedEmails(String email) {
            ValidationResult result = validator.isValidFormat(email);
            assertThat(result.isValid()).isTrue();
            assertThat(result.getErrorCode()).isNull();
        }
    }

    // -----------------------------------------------------------------------
    // Empty / Null
    // -----------------------------------------------------------------------

    @Nested
    @DisplayName("Empty and null inputs")
    class EmptyInputs {

        @Test
        @DisplayName("null input returns EMPTY_EMAIL")
        void nullReturnsEmptyEmail() {
            ValidationResult result = validator.isValidFormat(null);
            assertThat(result.isValid()).isFalse();
            assertThat(result.getErrorCode()).isEqualTo(ValidationErrorCode.EMPTY_EMAIL);
        }

        @Test
        @DisplayName("empty string returns EMPTY_EMAIL")
        void emptyStringReturnsEmptyEmail() {
            ValidationResult result = validator.isValidFormat("");
            assertThat(result.isValid()).isFalse();
            assertThat(result.getErrorCode()).isEqualTo(ValidationErrorCode.EMPTY_EMAIL);
        }

        @Test
        @DisplayName("whitespace-only string returns EMPTY_EMAIL")
        void whitespaceOnlyReturnsEmptyEmail() {
            ValidationResult result = validator.isValidFormat("   ");
            assertThat(result.isValid()).isFalse();
            assertThat(result.getErrorCode()).isEqualTo(ValidationErrorCode.EMPTY_EMAIL);
        }
    }

    // -----------------------------------------------------------------------
    // @ Symbol Rules
    // -----------------------------------------------------------------------

    @Nested
    @DisplayName("@ symbol rules")
    class AtSymbolRules {

        @Test
        @DisplayName("missing @ returns MISSING_AT_SYMBOL")
        void missingAtSymbol() {
            ValidationResult result = validator.isValidFormat("userexample.com");
            assertThat(result.isValid()).isFalse();
            assertThat(result.getErrorCode()).isEqualTo(ValidationErrorCode.MISSING_AT_SYMBOL);
        }

        @Test
        @DisplayName("multiple @ returns MULTIPLE_AT_SYMBOLS")
        void multipleAtSymbols() {
            ValidationResult result = validator.isValidFormat("user@@example.com");
            assertThat(result.isValid()).isFalse();
            assertThat(result.getErrorCode()).isEqualTo(ValidationErrorCode.MULTIPLE_AT_SYMBOLS);
        }

        @Test
        @DisplayName("two @ in valid-looking email returns MULTIPLE_AT_SYMBOLS")
        void twoAtSymbolsInEmail() {
            ValidationResult result = validator.isValidFormat("user@name@example.com");
            assertThat(result.isValid()).isFalse();
            assertThat(result.getErrorCode()).isEqualTo(ValidationErrorCode.MULTIPLE_AT_SYMBOLS);
        }
    }

    // -----------------------------------------------------------------------
    // Length Rules
    // -----------------------------------------------------------------------

    @Nested
    @DisplayName("Length constraints")
    class LengthConstraints {

        @Test
        @DisplayName("local part > 64 chars returns LOCAL_PART_TOO_LONG")
        void localPartTooLong() {
            String local = "a".repeat(65);
            ValidationResult result = validator.isValidFormat(local + "@example.com");
            assertThat(result.isValid()).isFalse();
            assertThat(result.getErrorCode()).isEqualTo(ValidationErrorCode.LOCAL_PART_TOO_LONG);
        }

        @Test
        @DisplayName("local part exactly 64 chars is valid")
        void localPartExactly64() {
            String local = "a".repeat(64);
            ValidationResult result = validator.isValidFormat(local + "@example.com");
            assertThat(result.isValid()).isTrue();
        }

        @Test
        @DisplayName("total length > 254 returns TOTAL_LENGTH_EXCEEDED")
        void totalLengthExceeded() {
            String local = "a".repeat(64);
            String domain = "b".repeat(63) + "." + "c".repeat(63) + "." + "d".repeat(63) + ".com";
            String email = local + "@" + domain;
            // Ensure it's over 254
            assertThat(email.length()).isGreaterThan(254);
            ValidationResult result = validator.isValidFormat(email);
            assertThat(result.isValid()).isFalse();
            assertThat(result.getErrorCode()).isEqualTo(ValidationErrorCode.TOTAL_LENGTH_EXCEEDED);
        }
    }

    // -----------------------------------------------------------------------
    // Dot Rules
    // -----------------------------------------------------------------------

    @Nested
    @DisplayName("Dot placement rules")
    class DotRules {

        @Test
        @DisplayName("local part starting with dot returns STARTS_ENDS_WITH_DOT")
        void localStartsWithDot() {
            ValidationResult result = validator.isValidFormat(".user@example.com");
            assertThat(result.isValid()).isFalse();
            assertThat(result.getErrorCode()).isEqualTo(ValidationErrorCode.STARTS_ENDS_WITH_DOT);
        }

        @Test
        @DisplayName("local part ending with dot returns STARTS_ENDS_WITH_DOT")
        void localEndsWithDot() {
            ValidationResult result = validator.isValidFormat("user.@example.com");
            assertThat(result.isValid()).isFalse();
            assertThat(result.getErrorCode()).isEqualTo(ValidationErrorCode.STARTS_ENDS_WITH_DOT);
        }

        @Test
        @DisplayName("consecutive dots in local part returns CONSECUTIVE_DOTS")
        void consecutiveDotsInLocal() {
            ValidationResult result = validator.isValidFormat("user..name@example.com");
            assertThat(result.isValid()).isFalse();
            assertThat(result.getErrorCode()).isEqualTo(ValidationErrorCode.CONSECUTIVE_DOTS);
        }

        @Test
        @DisplayName("consecutive dots in domain returns CONSECUTIVE_DOTS")
        void consecutiveDotsInDomain() {
            ValidationResult result = validator.isValidFormat("user@example..com");
            assertThat(result.isValid()).isFalse();
            assertThat(result.getErrorCode()).isEqualTo(ValidationErrorCode.CONSECUTIVE_DOTS);
        }
    }

    // -----------------------------------------------------------------------
    // Domain / TLD Rules
    // -----------------------------------------------------------------------

    @Nested
    @DisplayName("Domain and TLD rules")
    class DomainRules {

        @Test
        @DisplayName("missing domain after @ returns MISSING_DOMAIN")
        void missingDomain() {
            ValidationResult result = validator.isValidFormat("user@");
            assertThat(result.isValid()).isFalse();
            assertThat(result.getErrorCode()).isEqualTo(ValidationErrorCode.MISSING_DOMAIN);
        }

        @Test
        @DisplayName("domain without TLD returns MISSING_TLD")
        void missingTld() {
            ValidationResult result = validator.isValidFormat("user@example");
            assertThat(result.isValid()).isFalse();
            assertThat(result.getErrorCode()).isEqualTo(ValidationErrorCode.MISSING_TLD);
        }
    }

    // -----------------------------------------------------------------------
    // Invalid Characters
    // -----------------------------------------------------------------------

    @Nested
    @DisplayName("Invalid character detection")
    class InvalidCharacters {

        @Test
        @DisplayName("control character (tab) returns INVALID_CHARACTERS")
        void controlCharTab() {
            ValidationResult result = validator.isValidFormat("user\t@example.com");
            assertThat(result.isValid()).isFalse();
            assertThat(result.getErrorCode()).isEqualTo(ValidationErrorCode.INVALID_CHARACTERS);
        }

        @Test
        @DisplayName("null byte returns INVALID_CHARACTERS")
        void nullByte() {
            ValidationResult result = validator.isValidFormat("user\0@example.com");
            assertThat(result.isValid()).isFalse();
            assertThat(result.getErrorCode()).isEqualTo(ValidationErrorCode.INVALID_CHARACTERS);
        }

        @Test
        @DisplayName("space in email returns INVALID_CHARACTERS")
        void spaceInEmail() {
            ValidationResult result = validator.isValidFormat("user name@example.com");
            assertThat(result.isValid()).isFalse();
            assertThat(result.getErrorCode()).isEqualTo(ValidationErrorCode.INVALID_CHARACTERS);
        }

        @Test
        @DisplayName("emoji in email returns INVALID_CHARACTERS")
        void emojiInEmail() {
            ValidationResult result = validator.isValidFormat("user\uD83D\uDE00@example.com");
            assertThat(result.isValid()).isFalse();
            assertThat(result.getErrorCode()).isEqualTo(ValidationErrorCode.INVALID_CHARACTERS);
        }
    }
}
