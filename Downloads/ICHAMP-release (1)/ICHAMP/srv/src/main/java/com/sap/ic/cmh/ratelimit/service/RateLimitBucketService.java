package com.sap.ic.cmh.ratelimit.service;

import io.github.bucket4j.Bandwidth;
import io.github.bucket4j.Bucket;
import io.github.bucket4j.Bucket4j;
import io.github.bucket4j.Refill;
import java.time.Duration;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import com.sap.ic.cmh.ratelimit.config.RateLimitConstants;
import org.springframework.stereotype.Service;

@Service
public class RateLimitBucketService {

    private final Map<String, Bucket> cache = new ConcurrentHashMap<>();

    public Bucket resolveBucket(String tenantId) {
        return cache.computeIfAbsent(tenantId, this::newBucket);
    }

    private Bucket newBucket(String apiKey) {
        return Bucket4j.builder().addLimit(getLimit()).build();
    }

    private Bandwidth getLimit() {
        return Bandwidth.classic(
                RateLimitConstants.DEFAULT_API_RATE_LIMIT,
                Refill.intervally(
                        RateLimitConstants.DEFAULT_API_RATE_LIMIT, Duration.ofSeconds(1)));
    }
}
