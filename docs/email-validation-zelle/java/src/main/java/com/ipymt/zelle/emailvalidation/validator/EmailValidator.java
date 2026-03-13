package com.ipymt.zelle.emailvalidation.validator;

import com.ipymt.zelle.emailvalidation.model.enums.ValidationErrorCode;
import org.springframework.stereotype.Component;

import java.util.regex.Pattern;

/**
 * Stateless RFC 5322 simplified email format validator.
 *
 * <p>Validates email format against a comprehensive rule set:
 * <ul>
 *   <li>Must contain exactly one '@' symbol</li>
 *   <li>Local part max 64 characters</li>
 *   <li>Total length max 254 characters</li>
 *   <li>No control characters or emoji</li>
 *   <li>No leading/trailing dots in local part</li>
 *   <li>No consecutive dots</li>
 *   <li>Valid TLD (min 2 alphabetic characters)</li>
 * </ul>
 */
@Component
public class EmailValidator {

    private static final int LOCAL_MAX  = 64;
    private static final int TOTAL_MAX  = 254;

    /**
     * RFC 5322 simplified pattern.
     * Local part: alphanumeric + special chars, no leading/trailing/consecutive dots.
     * Domain: dot-separated labels, each label alphanumeric + hyphens.
     * TLD: min 2 alphabetic characters.
     */
    private static final Pattern EMAIL_PATTERN = Pattern.compile(
        "^[a-zA-Z0-9!#$%&'*+/=?^_`{|}~-]+" +
        "(?:\\.[a-zA-Z0-9!#$%&'*+/=?^_`{|}~-]+)*" +
        "@" +
        "(?:[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?\\.)" +
        "+[a-zA-Z]{2,}$"
    );

    /**
     * Result object returned by {@link #isValidFormat(String)}.
     */
    public static final class ValidationResult {
        private final boolean valid;
        private final ValidationErrorCode errorCode;

        private ValidationResult(boolean valid, ValidationErrorCode errorCode) {
            this.valid = valid;
            this.errorCode = errorCode;
        }

        public static ValidationResult valid() {
            return new ValidationResult(true, null);
        }

        public static ValidationResult invalid(ValidationErrorCode code) {
            return new ValidationResult(false, code);
        }

        public boolean isValid()                      { return valid; }
        public ValidationErrorCode getErrorCode()     { return errorCode; }
    }

    /**
     * Validates the format of the given email address.
     *
     * @param email the email string to validate (may be null or empty)
     * @return a {@link ValidationResult} indicating success or the specific error code
     */
    public ValidationResult isValidFormat(String email) {
        if (email == null || email.isBlank()) {
            return ValidationResult.invalid(ValidationErrorCode.EMPTY_EMAIL);
        }

        if (email.length() > TOTAL_MAX) {
            return ValidationResult.invalid(ValidationErrorCode.TOTAL_LENGTH_EXCEEDED);
        }

        if (containsControlOrEmojiCharacters(email)) {
            return ValidationResult.invalid(ValidationErrorCode.INVALID_CHARACTERS);
        }

        long atCount = email.chars().filter(c -> c == '@').count();
        if (atCount == 0) {
            return ValidationResult.invalid(ValidationErrorCode.MISSING_AT_SYMBOL);
        }
        if (atCount > 1) {
            return ValidationResult.invalid(ValidationErrorCode.MULTIPLE_AT_SYMBOLS);
        }

        int atIndex = email.indexOf('@');
        String localPart = email.substring(0, atIndex);
        String domain    = email.substring(atIndex + 1);

        if (localPart.isEmpty()) {
            return ValidationResult.invalid(ValidationErrorCode.MISSING_AT_SYMBOL);
        }
        if (localPart.length() > LOCAL_MAX) {
            return ValidationResult.invalid(ValidationErrorCode.LOCAL_PART_TOO_LONG);
        }
        if (localPart.startsWith(".") || localPart.endsWith(".")) {
            return ValidationResult.invalid(ValidationErrorCode.STARTS_ENDS_WITH_DOT);
        }
        if (localPart.contains("..")) {
            return ValidationResult.invalid(ValidationErrorCode.CONSECUTIVE_DOTS);
        }

        if (domain.isEmpty()) {
            return ValidationResult.invalid(ValidationErrorCode.MISSING_DOMAIN);
        }
        if (domain.contains("..")) {
            return ValidationResult.invalid(ValidationErrorCode.CONSECUTIVE_DOTS);
        }
        if (!domain.contains(".")) {
            return ValidationResult.invalid(ValidationErrorCode.MISSING_TLD);
        }

        if (!EMAIL_PATTERN.matcher(email).matches()) {
            return ValidationResult.invalid(ValidationErrorCode.INVALID_CHARACTERS);
        }

        return ValidationResult.valid();
    }

    /**
     * Returns {@code true} if the string contains ASCII control characters (0x00–0x1F, 0x7F)
     * or Unicode code points outside the Basic Multilingual Plane (surrogate pairs — used for emoji).
     */
    private boolean containsControlOrEmojiCharacters(String input) {
        for (int i = 0; i < input.length(); i++) {
            char c = input.charAt(i);
            if (c <= 0x1F || c == 0x7F) {
                return true;
            }
            // Detect surrogate pairs (emoji and supplementary characters)
            if (Character.isHighSurrogate(c) || Character.isLowSurrogate(c)) {
                return true;
            }
        }
        return false;
    }
}
