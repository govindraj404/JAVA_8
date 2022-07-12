package com.sap.ic.cmh.auditlog;

import com.sap.ic.cmh.utils.ThreadContext;
import org.springframework.stereotype.Component;

@Component
public class AuditOldDataContext<T> implements ThreadContext<T> {
private final ThreadLocal<T> localOldData = new ThreadLocal<>();

    @Override
    public void set(T oldData) {
        localOldData.set(oldData);
    }

    @Override
    public void reset() {
        localOldData.remove();
    }

    @Override
    public T get() {
        return localOldData.get();
    }
    
}
