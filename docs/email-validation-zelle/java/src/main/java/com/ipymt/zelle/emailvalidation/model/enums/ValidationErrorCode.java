package com.ipymt.zelle.emailvalidation.model.enums;

/**
 * Enumeration of all possible email validation error codes.
 * Each code maps to a user-facing message in the message bundle.
 */
public enum ValidationErrorCode {
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
