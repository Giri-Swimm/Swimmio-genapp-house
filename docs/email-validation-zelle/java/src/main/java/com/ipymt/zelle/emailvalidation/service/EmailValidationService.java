package com.ipymt.zelle.emailvalidation.service;

import com.ipymt.zelle.emailvalidation.audit.ValidationAuditLogger;
import com.ipymt.zelle.emailvalidation.model.dto.EmailValidationResponse;
import com.ipymt.zelle.emailvalidation.model.dto.ZelleCheckResponse;
import com.ipymt.zelle.emailvalidation.model.enums.ZelleRegistrationStatus;
import com.ipymt.zelle.emailvalidation.resolver.ErrorMessageResolver;
import com.ipymt.zelle.emailvalidation.validator.EmailValidator;
import com.ipymt.zelle.emailvalidation.validator.EmailValidator.ValidationResult;
import org.springframework.stereotype.Service;

import java.util.Locale;

/**
 * Orchestration service for email format validation and Zelle registration checks.
 */
@Service
public class EmailValidationService {

    private final EmailValidator emailValidator;
    private final EmailSanitiser sanitiser;
    private final ZelleRegistryService zelleRegistryService;
    private final ErrorMessageResolver messageResolver;
    private final ValidationAuditLogger auditLogger;

    public EmailValidationService(EmailValidator emailValidator,
                                  EmailSanitiser sanitiser,
                                  ZelleRegistryService zelleRegistryService,
                                  ErrorMessageResolver messageResolver,
                                  ValidationAuditLogger auditLogger) {
        this.emailValidator = emailValidator;
        this.sanitiser = sanitiser;
        this.zelleRegistryService = zelleRegistryService;
        this.messageResolver = messageResolver;
        this.auditLogger = auditLogger;
    }

    /**
     * Sanitises and validates the email address format.
     *
     * @param rawEmail  the raw email string from the request body
     * @param sessionId session correlation ID for audit logging
     * @param locale    the user's preferred locale for error messages
     * @return {@link EmailValidationResponse}
     */
    public EmailValidationResponse validate(String rawEmail, String sessionId, Locale locale) {
        String sanitised = sanitiser.sanitise(rawEmail);
        ValidationResult result = emailValidator.isValidFormat(sanitised);
        auditLogger.log(sessionId, result.getErrorCode(), result.isValid());

        if (result.isValid()) {
            return EmailValidationResponse.valid();
        }
        String message = messageResolver.resolve(result.getErrorCode(), locale);
        return EmailValidationResponse.invalid(result.getErrorCode().name(), message);
    }

    /**
     * Sanitises the email, re-validates format, then checks Zelle registration status.
     *
     * @param rawEmail  the raw email string from the request body
     * @param sessionId session correlation ID for audit logging
     * @param locale    the user's preferred locale for error messages
     * @return {@link ZelleCheckResponse}
     */
    public ZelleCheckResponse checkZelleRegistration(String rawEmail,
                                                     String sessionId,
                                                     Locale locale) {
        String sanitised = sanitiser.sanitise(rawEmail);
        ValidationResult formatResult = emailValidator.isValidFormat(sanitised);

        if (!formatResult.isValid()) {
            // Should not happen if client-side validation is working, but guard server-side
            return ZelleCheckResponse.notRegistered();
        }

        ZelleRegistrationStatus status = zelleRegistryService.checkRegistration(sanitised, sessionId);

        return switch (status) {
            case REGISTERED     -> ZelleCheckResponse.registered();
            case NOT_REGISTERED -> ZelleCheckResponse.notRegistered();
            case UNKNOWN        -> ZelleCheckResponse.unknown();
        };
    }
}
