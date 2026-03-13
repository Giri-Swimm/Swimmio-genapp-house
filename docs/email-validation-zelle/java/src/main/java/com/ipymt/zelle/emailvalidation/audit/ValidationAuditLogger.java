package com.ipymt.zelle.emailvalidation.audit;

import com.ipymt.zelle.emailvalidation.model.enums.ValidationErrorCode;
import com.ipymt.zelle.emailvalidation.model.enums.ZelleRegistrationStatus;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.stereotype.Component;

/**
 * Structured audit logger for email validation events.
 *
 * <p>Logs session ID and outcomes only — no PII (no raw email addresses) is written to logs.
 */
@Component
public class ValidationAuditLogger {

    private static final Logger log = LoggerFactory.getLogger(ValidationAuditLogger.class);

    /**
     * Logs the result of a format validation event.
     *
     * @param sessionId  session correlation ID (non-PII)
     * @param errorCode  the error code if validation failed; {@code null} if valid
     * @param isValid    whether the email passed format validation
     */
    public void log(String sessionId, ValidationErrorCode errorCode, boolean isValid) {
        try {
            MDC.put("sessionId", sanitiseForLog(sessionId));
            log.info("EMAIL_VALIDATION valid={} errorCode={}", isValid, errorCode);
        } finally {
            MDC.clear();
        }
    }

    /**
     * Logs the result of a Zelle registration check.
     *
     * @param sessionId  session correlation ID (non-PII)
     * @param status     the Zelle registration status
     * @param latencyMs  API call latency in milliseconds
     */
    public void logZelleCheck(String sessionId, ZelleRegistrationStatus status, long latencyMs) {
        try {
            MDC.put("sessionId", sanitiseForLog(sessionId));
            log.info("ZELLE_CHECK status={} latencyMs={}", status, latencyMs);
        } finally {
            MDC.clear();
        }
    }

    /**
     * Sanitises a value before writing to MDC to prevent log injection.
     */
    private String sanitiseForLog(String value) {
        if (value == null) return "unknown";
        // Strip newlines and carriage returns to prevent log injection
        return value.replaceAll("[\r\n]", "_");
    }
}
