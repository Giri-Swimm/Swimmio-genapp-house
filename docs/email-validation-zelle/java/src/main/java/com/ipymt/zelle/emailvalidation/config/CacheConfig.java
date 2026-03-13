package com.ipymt.zelle.emailvalidation.config;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.ipymt.zelle.emailvalidation.model.enums.ZelleRegistrationStatus;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.concurrent.TimeUnit;

/**
 * Spring configuration for the in-memory Caffeine cache used by
 * {@link com.ipymt.zelle.emailvalidation.service.ZelleRegistryService}.
 */
@Configuration
public class CacheConfig {

    /**
     * Provides a Caffeine cache keyed on SHA-256 hashed email tokens.
     * TTL: 60 seconds. Maximum 10 000 entries to bound memory usage.
     */
    @Bean
    public Cache<String, ZelleRegistrationStatus> zelleRegistrationCache() {
        return Caffeine.newBuilder()
                .maximumSize(10_000)
                .expireAfterWrite(60, TimeUnit.SECONDS)
                .recordStats()
                .build();
    }
}
