package com.ipymt.zelle.emailvalidation.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.reactive.function.client.WebClient;

/**
 * Spring configuration for the Zelle Early Warning Services HTTP client.
 */
@Configuration
public class ZelleClientConfig {

    @Value("${zelle.api.base-url}")
    private String baseUrl;

    @Value("${zelle.api.key}")
    private String apiKey;

    @Bean
    public WebClient zelleWebClient() {
        return WebClient.builder()
                .baseUrl(baseUrl)
                .defaultHeader("Authorization", "Bearer " + apiKey)
                .defaultHeader("Content-Type", "application/json")
                .build();
    }
}
