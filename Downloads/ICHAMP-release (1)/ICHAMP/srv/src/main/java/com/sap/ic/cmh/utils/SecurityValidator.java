package com.sap.ic.cmh.utils;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.parser.Parser;
import org.jsoup.safety.Whitelist;
import org.springframework.stereotype.Component;
import org.jsoup.nodes.Document.OutputSettings;

@Component
public class SecurityValidator {

    public boolean isValidText(String sTextToBeValidated) {
        boolean validValue = true;
        if (sTextToBeValidated != null) {
            Document.OutputSettings settings = new OutputSettings();
            settings.prettyPrint(false);
            String cleanedValue = Jsoup.clean(sTextToBeValidated, "", Whitelist.none(), settings);
            cleanedValue = Parser.unescapeEntities(cleanedValue, true);
            validValue = cleanedValue.equals(sTextToBeValidated);
        }
        if (sTextToBeValidated != null && validValue && sTextToBeValidated.matches("[`~!@#$%^&*+=><|;)(}{]")) {
            validValue = false;
        }
        return validValue;
    }

}