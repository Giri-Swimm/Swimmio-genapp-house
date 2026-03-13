package com.ipymt.zelle.emailvalidation.resolver;

import com.ipymt.zelle.emailvalidation.model.enums.ValidationErrorCode;
import org.springframework.context.MessageSource;
import org.springframework.stereotype.Component;

import java.util.Locale;

/**
 * Resolves locale-aware user-facing error messages for validation error codes.
 */
@Component
public class ErrorMessageResolver {

    private final MessageSource messageSource;

    public ErrorMessageResolver(MessageSource messageSource) {
        this.messageSource = messageSource;
    }

    /**
     * Returns the localised error message for the given {@link ValidationErrorCode}.
     *
     * @param code   the validation error code
     * @param locale the desired locale; falls back to English if not found
     * @return human-readable error message string
     */
    public String resolve(ValidationErrorCode code, Locale locale) {
        if (code == null) return "";
        String key = "email.error." + code.name();
        return messageSource.getMessage(key, null, code.name(), locale);
    }
}
