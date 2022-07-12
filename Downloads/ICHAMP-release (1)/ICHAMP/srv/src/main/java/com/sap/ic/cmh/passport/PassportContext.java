package com.sap.ic.cmh.passport;

import com.sap.ic.cmh.utils.ThreadContext;
import com.sap.jdsr.passport.DSRPassport;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class PassportContext implements ThreadContext<DSRPassport> {

    private final ThreadLocal<DSRPassport> localPassport = new ThreadLocal<>();

    @Override
    public void set(DSRPassport passport) {
        log.debug("Storing local passport '{}'.", PassportHelper.createPassportHexString(passport));
        localPassport.set(passport);
    }

    @Override
    public void reset() {
        log.debug("Reset local passport.");
        localPassport.remove();
    }

    @Override
    public DSRPassport get() {
        return localPassport.get();
    }

}
