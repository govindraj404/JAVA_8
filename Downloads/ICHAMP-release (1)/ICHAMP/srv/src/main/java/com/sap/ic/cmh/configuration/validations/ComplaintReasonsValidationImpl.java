package com.sap.ic.cmh.configuration.validations;

import cds.gen.configurationservice.ComplaintReasons;
import cds.gen.configurationservice.ComplaintReasons_;

import com.sap.cds.Result;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.configuration.persistency.ComplaintReasonsDao;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.utils.datavalidation.DataValidator;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.sap.ic.cmh.gen.MessageKeys;

@Component
public class ComplaintReasonsValidationImpl implements ComplaintReasonsValidation {

    @Autowired
    ComplaintReasonsDao complaintReasonsDao;

    @Autowired
    private DataValidator dataValidator;

    @Autowired
    Messages messages;

    public static final Logger logger = LoggerHelper.getLogger(ComplaintReasonsValidationImpl.class);
    private static final String COMPLAINT_REASON_VALIDATION_IMPL = "ComplaintReasonsValidationImpl";

    /**
     * Validate Complaint Reason attributes before creation and updation
     *
     * @public
     */
    @Override
    public void validateComplaintReason(ComplaintReasons reasons){
        LoggerHelper.logMethodEntry(logger, COMPLAINT_REASON_VALIDATION_IMPL, "validateComplaintReason");
        checkInputsSanitized(reasons);
        validationOnComplaintReasonCodeExists(reasons);
        LoggerHelper.logMethodExit(logger, COMPLAINT_REASON_VALIDATION_IMPL, "validateComplaintReason");
    }

    /**
     * check Complaint Reason attributes for sanitization
     *
     * @public
     */
    public void checkInputsSanitized(ComplaintReasons reasons) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_REASON_VALIDATION_IMPL, "checkInputsSanitized");
        logger.info("Inside ComplaintReasonsValidationImpl , method checkInputsSanitized");
        if (null == reasons.getCode() || StringUtils.isBlank(reasons.getCode())) {
            messages.error(MessageKeys.COMPLAINT_REASON_CODE_IS_MANDATORY)
                    .target("in", ComplaintReasons_.class,
                            ComplaintReasons_::code);
        } else {
            dataValidator.validateData(reasons.getCode(),
                    MessageKeys.INVALID_COMPLAINT_REASON_CODE,
                    ComplaintReasons_.class,
                    ComplaintReasons_::code, true, true);
        }

        logger.info("Inside ComplaintReasonsValidationImpl , validating description");
        dataValidator.validateData(reasons.getDescription(),
                MessageKeys.COMPLAINT_REASON_DESCRIPTION_ERROR,
                ComplaintReasons_.class,
                ComplaintReasons_::description,false,true);
        LoggerHelper.logMethodExit(logger, COMPLAINT_REASON_VALIDATION_IMPL, "checkInputsSanitized");
    }

    /**
     * Validate Complaint Reason code exists, error message to be given in that case before creation
     *
     * @public
     */
    public void validationOnComplaintReasonCodeExists(ComplaintReasons reasons) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_REASON_VALIDATION_IMPL, "validationOnComplaintReasonCodeExists");
        Result complaintReasons =  complaintReasonsDao.getComplaintReasonCodeAndIdByCode(reasons.getCode());

        if (complaintReasons.list().size()>0 && !complaintReasons.first().get().get("ID").toString().equals(reasons.getId())) {
            messages.error(MessageKeys.DUPLICATE_COMPLAINT_REASON_CODE)
                    .target("in", ComplaintReasons_.class, ComplaintReasons_::code);
        }
        LoggerHelper.logMethodExit(logger, COMPLAINT_REASON_VALIDATION_IMPL, "validationOnComplaintReasonCodeExists");
    }

}
