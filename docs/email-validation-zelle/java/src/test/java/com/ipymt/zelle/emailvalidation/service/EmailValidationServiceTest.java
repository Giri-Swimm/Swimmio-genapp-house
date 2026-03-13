package com.ipymt.zelle.emailvalidation.service;

import com.ipymt.zelle.emailvalidation.audit.ValidationAuditLogger;
import com.ipymt.zelle.emailvalidation.model.dto.EmailValidationResponse;
import com.ipymt.zelle.emailvalidation.model.dto.ZelleCheckResponse;
import com.ipymt.zelle.emailvalidation.model.enums.ValidationErrorCode;
import com.ipymt.zelle.emailvalidation.model.enums.ZelleRegistrationStatus;
import com.ipymt.zelle.emailvalidation.resolver.ErrorMessageResolver;
import com.ipymt.zelle.emailvalidation.validator.EmailValidator;
import com.ipymt.zelle.emailvalidation.validator.EmailValidator.ValidationResult;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Locale;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * Unit tests for {@link EmailValidationService}.
 */
@ExtendWith(MockitoExtension.class)
@DisplayName("EmailValidationService")
class EmailValidationServiceTest {

    @Mock private EmailValidator emailValidator;
    @Mock private EmailSanitiser sanitiser;
    @Mock private ZelleRegistryService zelleRegistryService;
    @Mock private ErrorMessageResolver messageResolver;
    @Mock private ValidationAuditLogger auditLogger;

    private EmailValidationService service;

    @BeforeEach
    void setUp() {
        service = new EmailValidationService(
                emailValidator, sanitiser, zelleRegistryService, messageResolver, auditLogger);
    }

    // --- validate() tests ---------------------------------------------------

    @Test
    @DisplayName("validate() returns valid response for well-formed email")
    void validateReturnsValidForGoodEmail() {
        when(sanitiser.sanitise("user@example.com")).thenReturn("user@example.com");
        when(emailValidator.isValidFormat("user@example.com"))
                .thenReturn(ValidationResult.valid());

        EmailValidationResponse response =
                service.validate("user@example.com", "sess-001", Locale.ENGLISH);

        assertThat(response.isValid()).isTrue();
        assertThat(response.getErrorCode()).isNull();
        assertThat(response.getErrorMessage()).isNull();
    }

    @Test
    @DisplayName("validate() returns invalid response with errorCode and message")
    void validateReturnsInvalidForBadEmail() {
        when(sanitiser.sanitise("userexample.com")).thenReturn("userexample.com");
        when(emailValidator.isValidFormat("userexample.com"))
                .thenReturn(ValidationResult.invalid(ValidationErrorCode.MISSING_AT_SYMBOL));
        when(messageResolver.resolve(ValidationErrorCode.MISSING_AT_SYMBOL, Locale.ENGLISH))
                .thenReturn("Email address must include an '@' symbol.");

        EmailValidationResponse response =
                service.validate("userexample.com", "sess-002", Locale.ENGLISH);

        assertThat(response.isValid()).isFalse();
        assertThat(response.getErrorCode()).isEqualTo("MISSING_AT_SYMBOL");
        assertThat(response.getErrorMessage()).isEqualTo("Email address must include an '@' symbol.");
    }

    @Test
    @DisplayName("validate() calls audit logger")
    void validateCallsAuditLogger() {
        when(sanitiser.sanitise(any())).thenReturn("user@example.com");
        when(emailValidator.isValidFormat(any())).thenReturn(ValidationResult.valid());

        service.validate("user@example.com", "sess-003", Locale.ENGLISH);

        verify(auditLogger).log(eq("sess-003"), isNull(), eq(true));
    }

    @Test
    @DisplayName("validate() sanitises input before validating")
    void validateSanitisesFirst() {
        when(sanitiser.sanitise("  user@example.com  ")).thenReturn("user@example.com");
        when(emailValidator.isValidFormat("user@example.com")).thenReturn(ValidationResult.valid());

        service.validate("  user@example.com  ", "sess-004", Locale.ENGLISH);

        verify(sanitiser).sanitise("  user@example.com  ");
        verify(emailValidator).isValidFormat("user@example.com");
    }

    // --- checkZelleRegistration() tests ------------------------------------

    @Test
    @DisplayName("checkZelleRegistration() returns REGISTERED response")
    void zelleCheckReturnsRegistered() {
        when(sanitiser.sanitise("user@example.com")).thenReturn("user@example.com");
        when(emailValidator.isValidFormat("user@example.com")).thenReturn(ValidationResult.valid());
        when(zelleRegistryService.checkRegistration("user@example.com", "sess-005"))
                .thenReturn(ZelleRegistrationStatus.REGISTERED);

        ZelleCheckResponse response =
                service.checkZelleRegistration("user@example.com", "sess-005", Locale.ENGLISH);

        assertThat(response.getRegistrationStatus()).isEqualTo(ZelleRegistrationStatus.REGISTERED);
        assertThat(response.isCanProceed()).isTrue();
        assertThat(response.isRequiresAcknowledgement()).isFalse();
    }

    @Test
    @DisplayName("checkZelleRegistration() returns NOT_REGISTERED response")
    void zelleCheckReturnsNotRegistered() {
        when(sanitiser.sanitise("unknown@example.com")).thenReturn("unknown@example.com");
        when(emailValidator.isValidFormat("unknown@example.com")).thenReturn(ValidationResult.valid());
        when(zelleRegistryService.checkRegistration("unknown@example.com", "sess-006"))
                .thenReturn(ZelleRegistrationStatus.NOT_REGISTERED);

        ZelleCheckResponse response =
                service.checkZelleRegistration("unknown@example.com", "sess-006", Locale.ENGLISH);

        assertThat(response.getRegistrationStatus()).isEqualTo(ZelleRegistrationStatus.NOT_REGISTERED);
        assertThat(response.isCanProceed()).isFalse();
    }

    @Test
    @DisplayName("checkZelleRegistration() returns UNKNOWN with requiresAcknowledgement=true on API failure")
    void zelleCheckReturnsUnknownOnApiFailure() {
        when(sanitiser.sanitise("user@example.com")).thenReturn("user@example.com");
        when(emailValidator.isValidFormat("user@example.com")).thenReturn(ValidationResult.valid());
        when(zelleRegistryService.checkRegistration("user@example.com", "sess-007"))
                .thenReturn(ZelleRegistrationStatus.UNKNOWN);

        ZelleCheckResponse response =
                service.checkZelleRegistration("user@example.com", "sess-007", Locale.ENGLISH);

        assertThat(response.getRegistrationStatus()).isEqualTo(ZelleRegistrationStatus.UNKNOWN);
        assertThat(response.isCanProceed()).isTrue();
        assertThat(response.isRequiresAcknowledgement()).isTrue();
    }

    @Test
    @DisplayName("checkZelleRegistration() returns NOT_REGISTERED if format is invalid (server-side guard)")
    void zelleCheckReturnsFalseIfFormatInvalid() {
        when(sanitiser.sanitise("bad-email")).thenReturn("bad-email");
        when(emailValidator.isValidFormat("bad-email"))
                .thenReturn(ValidationResult.invalid(ValidationErrorCode.MISSING_AT_SYMBOL));

        ZelleCheckResponse response =
                service.checkZelleRegistration("bad-email", "sess-008", Locale.ENGLISH);

        assertThat(response.isCanProceed()).isFalse();
        verifyNoInteractions(zelleRegistryService);
    }
}
