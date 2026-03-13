package com.ipymt.zelle.emailvalidation.model.dto;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

/**
 * Request body for the POST /api/v1/email/zelle-check endpoint.
 */
public class ZelleCheckRequest {

    @NotNull(message = "Email must not be null")
    @Size(max = 320, message = "Email must not exceed 320 characters")
    private String email;

    private String sessionId;

    public ZelleCheckRequest() {}

    public ZelleCheckRequest(String email, String sessionId) {
        this.email = email;
        this.sessionId = sessionId;
    }

    public String getEmail() { return email; }
    public void setEmail(String email) { this.email = email; }

    public String getSessionId() { return sessionId; }
    public void setSessionId(String sessionId) { this.sessionId = sessionId; }
}
