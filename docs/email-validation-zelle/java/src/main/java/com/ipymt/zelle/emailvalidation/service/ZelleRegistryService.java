package com.ipymt.zelle.emailvalidation.service;

import com.github.benmanes.caffeine.cache.Cache;
import com.ipymt.zelle.emailvalidation.audit.ValidationAuditLogger;
import com.ipymt.zelle.emailvalidation.client.ZelleRegistryClient;
import com.ipymt.zelle.emailvalidation.exception.ZelleApiException;
import com.ipymt.zelle.emailvalidation.model.enums.ZelleRegistrationStatus;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * Orchestrates Zelle registration checks with in-memory caching and resilience.
 *
 * <p>The email address is hashed with SHA-256 before being passed to the Zelle API
 * to comply with PII handling policies.
 */
@Service
public class ZelleRegistryService {

    private static final Logger log = LoggerFactory.getLogger(ZelleRegistryService.class);

    private final ZelleRegistryClient zelleClient;
    private final Cache<String, ZelleRegistrationStatus> registrationCache;
    private final ValidationAuditLogger auditLogger;

    public ZelleRegistryService(ZelleRegistryClient zelleClient,
                                Cache<String, ZelleRegistrationStatus> registrationCache,
                                ValidationAuditLogger auditLogger) {
        this.zelleClient = zelleClient;
        this.registrationCache = registrationCache;
        this.auditLogger = auditLogger;
    }

    /**
     * Checks whether the given email address is registered with the Zelle network.
     *
     * <p>Checks the in-memory cache first. On a cache miss, delegates to
     * {@link ZelleRegistryClient}. On any API error the method returns
     * {@link ZelleRegistrationStatus#UNKNOWN} (graceful degradation).
     *
     * @param email the normalised email address (already sanitised)
     * @param sessionId session correlation identifier (for audit logging; not PII)
     * @return {@link ZelleRegistrationStatus} — never {@code null}
     */
    public ZelleRegistrationStatus checkRegistration(String email, String sessionId) {
        String hash = computeSha256(email.toLowerCase().trim());

        ZelleRegistrationStatus cached = registrationCache.getIfPresent(hash);
        if (cached != null) {
            log.debug("Zelle cache HIT for sessionId={}", sessionId);
            return cached;
        }

        long start = System.currentTimeMillis();
        ZelleRegistrationStatus status;
        try {
            status = zelleClient.lookup(hash);
        } catch (ZelleApiException ex) {
            log.warn("Zelle API error for sessionId={}: {}", sessionId, ex.getMessage());
            status = ZelleRegistrationStatus.UNKNOWN;
        } catch (Exception ex) {
            log.warn("Unexpected error during Zelle check sessionId={}: {}", sessionId, ex.getMessage());
            status = ZelleRegistrationStatus.UNKNOWN;
        }

        long latency = System.currentTimeMillis() - start;
        auditLogger.logZelleCheck(sessionId, status, latency);
        registrationCache.put(hash, status);
        return status;
    }

    /**
     * Computes the SHA-256 hex digest of the input string.
     */
    String computeSha256(String input) {
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            byte[] hashBytes = digest.digest(input.getBytes(StandardCharsets.UTF_8));
            StringBuilder hexBuilder = new StringBuilder(hashBytes.length * 2);
            for (byte b : hashBytes) {
                hexBuilder.append(String.format("%02x", b));
            }
            return hexBuilder.toString();
        } catch (NoSuchAlgorithmException e) {
            // SHA-256 is guaranteed present in all Java SE implementations
            throw new IllegalStateException("SHA-256 algorithm not available", e);
        }
    }
}
