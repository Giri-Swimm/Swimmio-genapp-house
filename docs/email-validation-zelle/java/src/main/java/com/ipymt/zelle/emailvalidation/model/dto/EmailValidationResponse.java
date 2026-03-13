package com.ipymt.zelle.emailvalidation.model.dto;

/**
 * Response body for the POST /api/v1/email/validate endpoint.
 */
public class EmailValidationResponse {

    private boolean valid;
    private String errorCode;
    private String errorMessage;

    private EmailValidationResponse() {}

    public static EmailValidationResponse valid() {
        EmailValidationResponse r = new EmailValidationResponse();
        r.valid = true;
        return r;
    }

    public static EmailValidationResponse invalid(String errorCode, String errorMessage) {
        EmailValidationResponse r = new EmailValidationResponse();
        r.valid = false;
        r.errorCode = errorCode;
        r.errorMessage = errorMessage;
        return r;
    }

    public boolean isValid() { return valid; }
    public String getErrorCode() { return errorCode; }
    public String getErrorMessage() { return errorMessage; }
}
