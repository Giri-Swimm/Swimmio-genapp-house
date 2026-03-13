package com.ipymt.zelle.emailvalidation.exception;

/**
 * Thrown when the Zelle EWS API returns an error response or is unreachable.
 */
public class ZelleApiException extends RuntimeException {

    public ZelleApiException(String message) {
        super(message);
    }

    public ZelleApiException(String message, Throwable cause) {
        super(message, cause);
    }
}
