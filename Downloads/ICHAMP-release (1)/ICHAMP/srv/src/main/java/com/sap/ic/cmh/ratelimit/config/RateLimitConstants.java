package com.sap.ic.cmh.ratelimit.config;

public class RateLimitConstants {

    public static final Integer DEFAULT_API_RATE_LIMIT = 60;
    public static final Integer TOO_MANY_REQUESTS = 429;

    private RateLimitConstants() {
        this.getClass();
    }
}
