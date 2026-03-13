package com.ipymt.zelle.emailvalidation.service;

import org.springframework.stereotype.Component;

/**
 * Sanitises raw email input to prevent injection attacks and normalise the value
 * before format validation or downstream processing.
 */
@Component
public class EmailSanitiser {

    private static final int HARD_MAX_LENGTH = 320;

    /**
     * Sanitises the raw input string:
     * <ol>
     *   <li>Returns empty string for null input.</li>
     *   <li>Strips leading and trailing whitespace.</li>
     *   <li>Removes ASCII control characters (0x00–0x1F, 0x7F).</li>
     *   <li>Removes HTML/script tags.</li>
     *   <li>Hard-truncates to {@value #HARD_MAX_LENGTH} characters.</li>
     * </ol>
     *
     * @param rawInput the raw email string from user input
     * @return sanitised email string (never null)
     */
    public String sanitise(String rawInput) {
        if (rawInput == null) {
            return "";
        }

        String result = rawInput.trim();
        result = removeControlCharacters(result);
        result = removeHtmlTags(result);

        if (result.length() > HARD_MAX_LENGTH) {
            result = result.substring(0, HARD_MAX_LENGTH);
        }

        return result;
    }

    /**
     * Checks for obvious SQL or script injection patterns.
     * Used as a secondary server-side guard.
     *
     * @param input sanitised input to check
     * @return {@code true} if dangerous patterns are detected
     */
    public boolean containsDangerousPatterns(String input) {
        if (input == null || input.isBlank()) {
            return false;
        }
        String lower = input.toLowerCase();
        return lower.contains("<script")
            || lower.contains("javascript:")
            || lower.contains("' or ")
            || lower.contains("'; drop")
            || lower.contains("--")
            || lower.contains("/*")
            || lower.contains("*/")
            || lower.contains("xp_")
            || lower.contains("exec(");
    }

    private String removeControlCharacters(String input) {
        StringBuilder sb = new StringBuilder(input.length());
        for (int i = 0; i < input.length(); i++) {
            char c = input.charAt(i);
            if (c > 0x1F && c != 0x7F) {
                sb.append(c);
            }
        }
        return sb.toString();
    }

    private String removeHtmlTags(String input) {
        return input.replaceAll("<[^>]*>", "");
    }
}
