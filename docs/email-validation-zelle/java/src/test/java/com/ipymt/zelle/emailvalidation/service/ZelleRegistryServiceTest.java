package com.ipymt.zelle.emailvalidation.service;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.ipymt.zelle.emailvalidation.audit.ValidationAuditLogger;
import com.ipymt.zelle.emailvalidation.client.ZelleRegistryClient;
import com.ipymt.zelle.emailvalidation.exception.ZelleApiException;
import com.ipymt.zelle.emailvalidation.model.enums.ZelleRegistrationStatus;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.concurrent.TimeUnit;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

/**
 * Unit tests for {@link ZelleRegistryService}.
 */
@ExtendWith(MockitoExtension.class)
@DisplayName("ZelleRegistryService")
class ZelleRegistryServiceTest {

    @Mock
    private ZelleRegistryClient zelleClient;

    @Mock
    private ValidationAuditLogger auditLogger;

    private Cache<String, ZelleRegistrationStatus> cache;
    private ZelleRegistryService service;

    @BeforeEach
    void setUp() {
        cache = Caffeine.newBuilder()
                .maximumSize(100)
                .expireAfterWrite(60, TimeUnit.SECONDS)
                .build();
        service = new ZelleRegistryService(zelleClient, cache, auditLogger);
    }

    @Test
    @DisplayName("returns REGISTERED when Zelle client returns REGISTERED")
    void returnsRegistered() {
        when(zelleClient.lookup(anyString())).thenReturn(ZelleRegistrationStatus.REGISTERED);

        ZelleRegistrationStatus result = service.checkRegistration("user@example.com", "sess-001");

        assertThat(result).isEqualTo(ZelleRegistrationStatus.REGISTERED);
        verify(zelleClient).lookup(anyString());
    }

    @Test
    @DisplayName("returns NOT_REGISTERED when Zelle client returns NOT_REGISTERED")
    void returnsNotRegistered() {
        when(zelleClient.lookup(anyString())).thenReturn(ZelleRegistrationStatus.NOT_REGISTERED);

        ZelleRegistrationStatus result = service.checkRegistration("unknown@example.com", "sess-002");

        assertThat(result).isEqualTo(ZelleRegistrationStatus.NOT_REGISTERED);
    }

    @Test
    @DisplayName("returns UNKNOWN when Zelle API throws ZelleApiException (graceful degradation)")
    void returnsUnknownOnApiException() {
        when(zelleClient.lookup(anyString())).thenThrow(new ZelleApiException("Service unavailable"));

        ZelleRegistrationStatus result = service.checkRegistration("user@example.com", "sess-003");

        assertThat(result).isEqualTo(ZelleRegistrationStatus.UNKNOWN);
    }

    @Test
    @DisplayName("returns UNKNOWN on unexpected runtime exception")
    void returnsUnknownOnRuntimeException() {
        when(zelleClient.lookup(anyString())).thenThrow(new RuntimeException("Timeout"));

        ZelleRegistrationStatus result = service.checkRegistration("user@example.com", "sess-004");

        assertThat(result).isEqualTo(ZelleRegistrationStatus.UNKNOWN);
    }

    @Test
    @DisplayName("second call for same email uses cache, does not call Zelle client again")
    void cacheHitSkipsClientCall() {
        when(zelleClient.lookup(anyString())).thenReturn(ZelleRegistrationStatus.REGISTERED);

        service.checkRegistration("user@example.com", "sess-005");
        service.checkRegistration("user@example.com", "sess-006");  // should hit cache

        // Zelle client should be called only once
        verify(zelleClient, times(1)).lookup(anyString());
    }

    @Test
    @DisplayName("different emails are treated as independent cache entries")
    void differentEmailsHaveIndependentCacheEntries() {
        when(zelleClient.lookup(anyString()))
                .thenReturn(ZelleRegistrationStatus.REGISTERED)
                .thenReturn(ZelleRegistrationStatus.NOT_REGISTERED);

        ZelleRegistrationStatus r1 = service.checkRegistration("a@example.com", "sess-007");
        ZelleRegistrationStatus r2 = service.checkRegistration("b@example.com", "sess-008");

        assertThat(r1).isEqualTo(ZelleRegistrationStatus.REGISTERED);
        assertThat(r2).isEqualTo(ZelleRegistrationStatus.NOT_REGISTERED);
        verify(zelleClient, times(2)).lookup(anyString());
    }

    @Test
    @DisplayName("SHA-256 hash is case-insensitive (same hash for upper and lower case email)")
    void sha256IsCaseInsensitive() {
        String lower = service.computeSha256("user@example.com");
        String upper = service.computeSha256("USER@EXAMPLE.COM");
        // The service normalises to lowercase before hashing
        assertThat(lower).isEqualTo(upper);
    }

    @Test
    @DisplayName("SHA-256 hash is 64 hex characters")
    void sha256HasCorrectLength() {
        String hash = service.computeSha256("user@example.com");
        assertThat(hash).hasSize(64);
        assertThat(hash).matches("[0-9a-f]{64}");
    }

    @Test
    @DisplayName("audit logger is called for each check")
    void auditLoggerCalled() {
        when(zelleClient.lookup(anyString())).thenReturn(ZelleRegistrationStatus.REGISTERED);

        service.checkRegistration("user@example.com", "sess-009");

        verify(auditLogger).logZelleCheck(eq("sess-009"), eq(ZelleRegistrationStatus.REGISTERED), anyLong());
    }
}
