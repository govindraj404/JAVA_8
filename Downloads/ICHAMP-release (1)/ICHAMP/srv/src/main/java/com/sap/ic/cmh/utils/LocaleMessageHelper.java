package com.sap.ic.cmh.utils;


import com.sap.cds.services.runtime.CdsRuntime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

@Component
public class LocaleMessageHelper {

	@Autowired
    CdsRuntime runtime;
    /**
     * Get the localized meesage for the given key from i18 properties
     *
     * @param key
     * @return globalized  message
     */
    public String getMessage(String key) {
        return runtime.getLocalizedMessage(key, null, LocaleContextHolder.getLocale());
    }
}
