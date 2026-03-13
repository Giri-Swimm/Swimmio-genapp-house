package com.ipymt.zelle.emailvalidation.service;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit tests for {@link EmailSanitiser}.
 */
@DisplayName("EmailSanitiser")
class EmailSanitiserTest {

    private EmailSanitiser sanitiser;

    @BeforeEach
    void setUp() {
        sanitiser = new EmailSanitiser();
    }

    @Test
    @DisplayName("null input returns empty string")
    void nullInputReturnsEmpty() {
        assertThat(sanitiser.sanitise(null)).isEqualTo("");
    }

    @Test
    @DisplayName("leading and trailing whitespace is trimmed")
    void trimsWhitespace() {
        assertThat(sanitiser.sanitise("  user@example.com  ")).isEqualTo("user@example.com");
    }

    @Test
    @DisplayName("control characters are removed")
    void removesControlCharacters() {
        String input = "user\u0000\u001F@example.com";
        assertThat(sanitiser.sanitise(input)).isEqualTo("user@example.com");
    }

    @Test
    @DisplayName("HTML tags are stripped")
    void stripsHtmlTags() {
        String input = "<script>alert(1)</script>user@example.com";
        assertThat(sanitiser.sanitise(input)).isEqualTo("alert(1)user@example.com");
    }

    @Test
    @DisplayName("input over 320 chars is truncated to 320")
    void truncatesOverLength() {
        String input = "a".repeat(400) + "@example.com";
        String result = sanitiser.sanitise(input);
        assertThat(result.length()).isEqualTo(320);
    }

    @Test
    @DisplayName("normal valid email is unchanged")
    void normalEmailUnchanged() {
        assertThat(sanitiser.sanitise("user@example.com")).isEqualTo("user@example.com");
    }

    @Test
    @DisplayName("containsDangerousPatterns detects script injection")
    void detectsScriptInjection() {
        assertThat(sanitiser.containsDangerousPatterns("<script>alert()</script>")).isTrue();
    }

    @Test
    @DisplayName("containsDangerousPatterns detects SQL injection")
    void detectsSqlInjection() {
        assertThat(sanitiser.containsDangerousPatterns("user'; drop table users--")).isTrue();
    }

    @Test
    @DisplayName("containsDangerousPatterns returns false for clean input")
    void cleanInputReturnsFalse() {
        assertThat(sanitiser.containsDangerousPatterns("user@example.com")).isFalse();
    }

    @Test
    @DisplayName("containsDangerousPatterns returns false for null")
    void nullReturnsFalse() {
        assertThat(sanitiser.containsDangerousPatterns(null)).isFalse();
    }
}
