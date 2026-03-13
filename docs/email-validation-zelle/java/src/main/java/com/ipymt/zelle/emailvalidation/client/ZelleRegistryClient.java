package com.ipymt.zelle.emailvalidation.client;

import com.ipymt.zelle.emailvalidation.exception.ZelleApiException;
import com.ipymt.zelle.emailvalidation.model.enums.ZelleRegistrationStatus;
import io.github.resilience4j.circuitbreaker.annotation.CircuitBreaker;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.reactive.function.client.WebClientResponseException;

import java.time.Duration;
import java.util.Map;

/**
 * HTTP client for the Zelle Early Warning Services (EWS) Recipient Lookup API.
 *
 * <p>Protected by a Resilience4j circuit breaker named "zelleRegistry".
 * On circuit open or any 5xx error the fallback method returns
 * {@link ZelleRegistrationStatus#UNKNOWN}.
 */
@Component
public class ZelleRegistryClient {

    private static final Logger log = LoggerFactory.getLogger(ZelleRegistryClient.class);

    private static final Duration TIMEOUT = Duration.ofSeconds(5);

    private final WebClient zelleWebClient;

    @Value("${zelle.api.base-url}")
    private String baseUrl;

    public ZelleRegistryClient(WebClient zelleWebClient) {
        this.zelleWebClient = zelleWebClient;
    }

    /**
     * Looks up the Zelle registration status for the given hashed email token.
     *
     * @param hashedEmail SHA-256 hex of the normalised email address
     * @return {@link ZelleRegistrationStatus}
     * @throws ZelleApiException if the API returns a 4xx or unrecoverable error
     */
    @CircuitBreaker(name = "zelleRegistry", fallbackMethod = "fallback")
    public ZelleRegistrationStatus lookup(String hashedEmail) {
        ZelleApiResponse response = zelleWebClient.post()
                .uri("/v1/recipient/lookup")
                .bodyValue(Map.of("tokenizedEmail", hashedEmail))
                .retrieve()
                .onStatus(
                    status -> status.is4xxClientError(),
                    clientResponse -> clientResponse.bodyToMono(String.class)
                            .map(body -> new ZelleApiException("Zelle API client error: " + body))
                )
                .onStatus(
                    status -> status.is5xxServerError(),
                    clientResponse -> clientResponse.bodyToMono(String.class)
                            .map(body -> new ZelleApiException("Zelle API server error: " + body))
                )
                .bodyToMono(ZelleApiResponse.class)
                .timeout(TIMEOUT)
                .block();

        if (response == null) {
            return ZelleRegistrationStatus.UNKNOWN;
        }
        return mapToStatus(response.getStatus());
    }

    /**
     * Circuit breaker fallback — invoked when the circuit is open or a non-recoverable
     * exception occurs after retries.
     */
    @SuppressWarnings("unused")
    public ZelleRegistrationStatus fallback(String hashedEmail, Throwable t) {
        log.warn("Zelle circuit breaker fallback triggered: {}", t.getMessage());
        return ZelleRegistrationStatus.UNKNOWN;
    }

    private ZelleRegistrationStatus mapToStatus(String apiStatus) {
        if (apiStatus == null) return ZelleRegistrationStatus.UNKNOWN;
        return switch (apiStatus.toUpperCase()) {
            case "FOUND", "ACTIVE" -> ZelleRegistrationStatus.REGISTERED;
            case "NOT_FOUND"       -> ZelleRegistrationStatus.NOT_REGISTERED;
            default                -> ZelleRegistrationStatus.UNKNOWN;
        };
    }

    /**
     * Internal DTO for deserialising the Zelle EWS API response.
     */
    static class ZelleApiResponse {
        private String status;
        public String getStatus() { return status; }
        public void setStatus(String status) { this.status = status; }
    }
}
