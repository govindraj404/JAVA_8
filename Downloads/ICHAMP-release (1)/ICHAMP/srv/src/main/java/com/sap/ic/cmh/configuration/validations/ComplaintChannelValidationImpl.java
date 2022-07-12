package com.sap.ic.cmh.configuration.validations;

import cds.gen.configurationservice.ComplaintChannels_;
import cds.gen.configurationservice.ComplaintChannels;

import com.sap.cds.Result;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.configuration.persistency.ComplaintChannelDao;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.utils.datavalidation.DataValidator;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class ComplaintChannelValidationImpl implements ComplaintChannelValidation {

    public static final Logger logger = LoggerHelper.getLogger(ComplaintChannelValidationImpl.class);
    private static final String COMPLAINT_CHANNEL_VALIDATION_IMPL = "ComplaintChannelValidationImpl";

    @Autowired
    DataValidator dataValidator;

    @Autowired
    Messages messages;

    @Autowired
    ComplaintChannelDao complaintChannelDao;

    /**
     *
     * validate complaint channel configs being created or updated.
     *
     * @public
     */
    @Override
    public void validateComplaintChannel(ComplaintChannels channels){
        LoggerHelper.logMethodEntry(logger, COMPLAINT_CHANNEL_VALIDATION_IMPL, "validateComplaintChannel");
        checkInputsSanitized(channels);
        validationComplaintChannelCodeExists(channels);
        LoggerHelper.logMethodExit(logger, COMPLAINT_CHANNEL_VALIDATION_IMPL, "validateComplaintChannel");
    }

    /**
     *
     * check the code and description fields for validity.
     *
     * @public
     */
    public void checkInputsSanitized(ComplaintChannels channels) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_CHANNEL_VALIDATION_IMPL, "checkInputsSanitized");
        logger.info("Inside ComplaintChannelValidationImpl , method checkInputsSanitized");
        if (null == channels.getCode() || StringUtils.isBlank(channels.getCode())) {
            messages.error(MessageKeys.COMPLAINT_CHANNEL_CODE_IS_MANDATORY)
                    .target("in", ComplaintChannels_.class,
                            ComplaintChannels_::code);
        }
        logger.info("Inside ComplaintChannelValidationImpl , validating channel code");
        if (null != channels.getCode() && StringUtils.isNotBlank(channels.getCode())) {
            dataValidator.validateData(channels.getCode(),
                    MessageKeys.INVALID_COMPLAINT_CHANNEL_CODE,
                    ComplaintChannels_.class,
                    ComplaintChannels_::code, true, true);
        }

        logger.info("Inside ComplaintChannelValidationImpl , validating description");
        dataValidator.validateData(channels.getDescription(),
                MessageKeys.COMPLAINT_CHANNEL_DESCRIPTION_ERROR,
                ComplaintChannels_.class,
                ComplaintChannels_::description,false,true);
        LoggerHelper.logMethodExit(logger, COMPLAINT_CHANNEL_VALIDATION_IMPL, "checkInputsSanitized");
    }

    /**
     *
     * validate if the complaint channel code already exists, to avoid duplicate code being created.
     *
     * @public
     */
    public void validationComplaintChannelCodeExists(ComplaintChannels channels) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_CHANNEL_VALIDATION_IMPL, "validationComplaintChannelCodeExists");
        Result complaintChannels =  complaintChannelDao.getAllComplaintChannelsCodeAndIDByCode(channels.getCode());
        if (complaintChannels.list().size()>0 && !complaintChannels.first().get().get("ID").toString().equals(channels.getId())) {
            messages.error(MessageKeys.DUPLICATE_COMPLAINT_CHANNEL_CODE)
                    .target("in", ComplaintChannels_.class, ComplaintChannels_::code);
        }
        LoggerHelper.logMethodExit(logger, COMPLAINT_CHANNEL_VALIDATION_IMPL, "validationComplaintChannelCodeExists");
    }
}
