package com.sap.ic.cmh.utils;

public interface ThreadContext<T> {

    void set(T t);

    void reset();

    T get();
}
