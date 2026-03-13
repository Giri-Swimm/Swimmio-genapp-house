package com.ipymt.zelle.emailvalidation.controller;

import com.ipymt.zelle.emailvalidation.model.dto.EmailValidationRequest;
import com.ipymt.zelle.emailvalidation.model.dto.EmailValidationResponse;
import com.ipymt.zelle.emailvalidation.model.dto.ZelleCheckRequest;
import com.ipymt.zelle.emailvalidation.model.dto.ZelleCheckResponse;
import com.ipymt.zelle.emailvalidation.service.EmailValidationService;
import jakarta.validation.Valid;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.Locale;

/**
 * REST controller exposing email format validation and Zelle registration check endpoints.
 *
 * <ul>
 *   <li>POST /api/v1/email/validate       — format validation</li>
 *   <li>POST /api/v1/email/zelle-check    — Zelle registration verification</li>
 * </ul>
 */
@RestController
@RequestMapping("/api/v1/email")
@Validated
public class EmailValidationController {

    private final EmailValidationService validationService;

    public EmailValidationController(EmailValidationService validationService) {
        this.validationService = validationService;
    }

    /**
     * Validates the email address format.
     *
     * @param request  the validation request containing the email and optional session ID
     * @param locale   resolved from the Accept-Language header by Spring
     * @return 200 OK with an {@link EmailValidationResponse} indicating valid/invalid state
     */
    @PostMapping("/validate")
    public ResponseEntity<EmailValidationResponse> validateFormat(
            @RequestBody @Valid EmailValidationRequest request,
            Locale locale) {

        EmailValidationResponse response =
                validationService.validate(request.getEmail(), request.getSessionId(), locale);
        return ResponseEntity.ok(response);
    }

    /**
     * Checks whether the email address is registered with the Zelle network.
     *
     * @param request  the check request containing the email and optional session ID
     * @param locale   resolved from the Accept-Language header by Spring
     * @return 200 OK with a {@link ZelleCheckResponse}
     */
    @PostMapping("/zelle-check")
    public ResponseEntity<ZelleCheckResponse> checkZelleRegistration(
            @RequestBody @Valid ZelleCheckRequest request,
            Locale locale) {

        ZelleCheckResponse response =
                validationService.checkZelleRegistration(
                        request.getEmail(), request.getSessionId(), locale);
        return ResponseEntity.ok(response);
    }
}
