package com.sap.ic.cmh.pdm;

import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.context.MessageSource;
import org.springframework.context.MessageSourceAware;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.util.FileCopyUtils;
import org.springframework.web.bind.annotation.*;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
@Controller
@RequestMapping("/pdm")
public class AnnotationController implements MessageSourceAware {
    private MessageSource messageSource;
    private static final String ANNOTATION_CONTROLLER = "AnnotationController";
    private static final String MESSAGE_KEY_NOT_FOUND = "messageKey-not-found";
    private static final String IO_EXCEPTION_MESSAGE = "I/O exception occurred with exception: {}";
    public static final Logger logger = LoggerHelper.getLogger(AnnotationController.class);
    /*getBusinessPartnerMetadata method is used for the getting metadata for Business Partner */

    @GetMapping(value = "/PDMBusinessPartnerAnnotations", produces = MediaType.APPLICATION_JSON_VALUE)
    @ResponseBody
    public String getBusinessPartnerMetadata(@RequestHeader("accept-language") Locale locale) {

        return getDataFromResource("security/pdm/pdmBusinessPartnerAnnotations.json", locale);

    }
    @GetMapping(value = "/PDMBtpUserAnnotations", produces = MediaType.APPLICATION_JSON_VALUE)
    @ResponseBody
    public String getBtpUserMetadata(@RequestHeader("accept-language") Locale locale) {

        return getDataFromResource("security/pdm/pdmBtpUserAnnotations.json", locale);

    }

    /*getTextBundle method is used for retrieve value from I18n file */

    @GetMapping(value = "/texts/{localeTextBundle}", produces = MediaType.TEXT_PLAIN_VALUE)
    @ResponseBody
    public String getTextBundle(@PathVariable String localeTextBundle) throws PDMException {
        InputStream inputStream = null;
        LoggerHelper.logMethodEntry(logger, ANNOTATION_CONTROLLER, "getTextBundle");
        try {
            String resourcePath = "i18n/" + localeTextBundle;
            Resource resource = new ClassPathResource(resourcePath);
            if (!resource.exists()) {
                resource = new ClassPathResource("i18n/i18n.properties");
            }
            inputStream = resource.getInputStream();
            byte[] bdata = FileCopyUtils.copyToByteArray(inputStream);
            return new String(bdata, StandardCharsets.UTF_8);
        } catch (IOException e) {
            throw new PDMException("File not found", e);
        } finally {
            if (inputStream != null)
                safeClose(inputStream);
        }

    }

    /*getDataFromResource method is used for retrieve metadata for Business partner */
    public synchronized String getDataFromResource(String classPathResource, Locale locale) {
        InputStream inputStream = null;
        LoggerHelper.logMethodEntry(logger, ANNOTATION_CONTROLLER, "getDataFromResource");
        try {
            Resource resource = new ClassPathResource(classPathResource);
            inputStream = resource.getInputStream();
            byte[] bdata = FileCopyUtils.copyToByteArray(inputStream);
            String data = new String(bdata, StandardCharsets.UTF_8);
            Pattern pattern = Pattern.compile("\\{(.*?)\\}");
            Matcher matcher = pattern.matcher(data);
            while (matcher.find()) {
                String key = matcher.group();
                key = key.substring(key.indexOf('{') + 1, key.lastIndexOf('}'));
                String message = this.messageSource.getMessage(key, null, MESSAGE_KEY_NOT_FOUND, locale);
                if (MESSAGE_KEY_NOT_FOUND.equals(message))
                    data = data.replace("{" + key + "}", key.toLowerCase());
                else
                    data = data.replace("{" + key + "}", message);
            }
            return data;
        } catch (IOException e) {
            logger.error(IO_EXCEPTION_MESSAGE);

        } finally {
            if (inputStream != null)
                safeClose(inputStream);
        }
        LoggerHelper.logMethodExit(logger, ANNOTATION_CONTROLLER, "getDataFromResource");
        return "";
    }


    @Override
    public synchronized void setMessageSource(MessageSource messageSource) {
        this.messageSource = messageSource;
    }

    public static void safeClose(InputStream inputStream) {
        if (inputStream != null) {
            try {
                inputStream.close();
            } catch (IOException e) {
                LoggerHelper.logExceptionWithMessage(logger, IO_EXCEPTION_MESSAGE, e);
            }
        }
    }

}