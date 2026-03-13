package com.ipymt.zelle.emailvalidation.model.enums;

/**
 * Zelle network registration status for a given email address.
 */
public enum ZelleRegistrationStatus {
    /** Email is enrolled in the Zelle network. */
    REGISTERED,

    /** Email is not enrolled in the Zelle network. */
    NOT_REGISTERED,

    /** Registration status could not be determined (API unavailable, timeout, etc.). */
    UNKNOWN
}
