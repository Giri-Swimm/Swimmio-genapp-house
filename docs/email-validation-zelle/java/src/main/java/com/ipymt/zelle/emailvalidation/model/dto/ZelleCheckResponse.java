package com.ipymt.zelle.emailvalidation.model.dto;

import com.ipymt.zelle.emailvalidation.model.enums.ZelleRegistrationStatus;

/**
 * Response body for the POST /api/v1/email/zelle-check endpoint.
 */
public class ZelleCheckResponse {

    private ZelleRegistrationStatus registrationStatus;
    private boolean canProceed;
    private String message;
    private boolean requiresAcknowledgement;

    public ZelleCheckResponse() {}

    public ZelleCheckResponse(ZelleRegistrationStatus registrationStatus,
                              boolean canProceed,
                              String message,
                              boolean requiresAcknowledgement) {
        this.registrationStatus = registrationStatus;
        this.canProceed = canProceed;
        this.message = message;
        this.requiresAcknowledgement = requiresAcknowledgement;
    }

    public static ZelleCheckResponse registered() {
        return new ZelleCheckResponse(
                ZelleRegistrationStatus.REGISTERED, true, "Registered with Zelle", false);
    }

    public static ZelleCheckResponse notRegistered() {
        return new ZelleCheckResponse(
                ZelleRegistrationStatus.NOT_REGISTERED, false,
                "This email address is not registered with Zelle. " +
                "Please check the address or ask the recipient to enroll.", false);
    }

    public static ZelleCheckResponse unknown() {
        return new ZelleCheckResponse(
                ZelleRegistrationStatus.UNKNOWN, true,
                "Unable to verify Zelle registration. You may still proceed.", true);
    }

    public ZelleRegistrationStatus getRegistrationStatus() { return registrationStatus; }
    public boolean isCanProceed() { return canProceed; }
    public String getMessage() { return message; }
    public boolean isRequiresAcknowledgement() { return requiresAcknowledgement; }
}
