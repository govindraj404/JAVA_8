package com.sap.ic.cmh.configuration.validations;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import cds.gen.configurationservice.TargetTypes;
import cds.gen.configurationservice.TargetTypes_;
import io.micrometer.core.instrument.util.StringUtils;
import java.util.Objects;
import com.sap.cds.Result;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.configuration.persistency.TargetDocumentCategoryDao;
import com.sap.ic.cmh.configuration.service.TargetTypeConfigurationService;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.SecurityValidator;
import com.sap.ic.cmh.utils.datavalidation.DataValidator;
import com.sap.ic.cmh.utils.LoggerHelper;

@Component
public class TargetTypeConfigurationValidationImpl implements TargetTypeConfigurationValidation {

    @Autowired
    TargetTypeConfigurationService targetTypeService;

    @Autowired
    Messages messages;

    @Autowired
    SecurityValidator securityValidator;

    @Autowired
    TargetDocumentCategoryDao targetDocumentCategoryDao;

    @Autowired
    DataValidator dataValidator;

    private static final Logger logger = LoggerFactory.getLogger(TargetTypeConfigurationValidationImpl.class);
    private static final String TARGET_TYPE_VALIDATION_IMPL = "TargetTypeValidationImpl";
    
    	/**
     * Validate TargetTypes and its individual attributes
     *
     * @param {@link TargetTypes} targetTypes
     *
     * @public
     */
    @Override
    public void validateTargetType(TargetTypes targetType){
        LoggerHelper.logMethodEntry(logger, TARGET_TYPE_VALIDATION_IMPL, "validateTargetTypes");
        validateTargetTypeCode(targetType);
        validateTargetDocumentaryCode(targetType);
        validateDescription(targetType);
        LoggerHelper.logMethodExit(logger, TARGET_TYPE_VALIDATION_IMPL, "validateTargetTypes");
    }

    private void validateDescription(TargetTypes targetType) {
        if(StringUtils.isNotBlank(targetType.getDescription()) && !securityValidator.isValidText(targetType.getDescription())){
            messages.error(MessageKeys.INVALID_TARGET_TYPE_DESCRIPTION).target("in", TargetTypes_.class,
            TargetTypes_::description);
        }
    }

    private void validateTargetDocumentaryCode(TargetTypes targetType) {
        if(StringUtils.isBlank(targetType.getTargetDocumentCategoryCode())){
            messages.error(MessageKeys.TARGET_DOCUMENT_CATERGORY_IS_MANDATORY).target("in", TargetTypes_.class,
            TargetTypes_::targetDocumentCategory_code);
        }
        else if(StringUtils.isNotBlank(targetType.getTargetDocumentCategoryCode()) && !isValidTargetDocumentCategory(targetType)){
            messages.error(MessageKeys.INVALID_TARGET_DOCUMENT_CATERGORY).target("in", TargetTypes_.class,
            TargetTypes_::targetDocumentCategory_code);
        }
    }

    private void validateTargetTypeCode(TargetTypes targetType) {
        if(StringUtils.isBlank(targetType.getCode())){
            messages.error(MessageKeys.TARGET_TYPE_IS_MANDATORY).target("in", TargetTypes_.class,
            TargetTypes_::code);
        }

        else if(StringUtils.isNotBlank(targetType.getCode())){
            dataValidator.validateData(targetType.getCode(),
            MessageKeys.TARGET_TYPE_VALIDATION_ERROR,
            TargetTypes_.class,
            TargetTypes_::code, true, true);
            if(isExistsTargetTypeCode(targetType)){
                messages.error(MessageKeys.TARGET_TYPE_EXIST).target("in", TargetTypes_.class,
                TargetTypes_::code);
            }
        }
    }

    private boolean isValidTargetDocumentCategory(TargetTypes targetTypes){
        Result targetDocumentCategoryResult = targetDocumentCategoryDao.getTargetDocumentCategoryBasedOnCode(targetTypes.getTargetDocumentCategoryCode());
        return (targetDocumentCategoryResult.first().isPresent());
    }

    private boolean isExistsTargetTypeCode(TargetTypes targetTypes){ 
        Result targetTypeConfigurationResult = targetTypeService.getTargetTypeConfigurationIdBasedOnCode(targetTypes.getCode());
        return (targetTypeConfigurationResult.first().isPresent() && !Objects.equals(targetTypes.getId(), targetTypeConfigurationResult.first().get().get("ID")));
    }

}
