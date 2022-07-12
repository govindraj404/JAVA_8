package com.sap.ic.cmh.configuration.validations;

import java.util.Optional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.ql.Select;
import com.sap.cds.services.messages.Message.Severity;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.persistency.BusinessObjectConfigurationDao;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.SecurityValidator;

import cds.gen.configurationservice.BusinessObjectAttributes_;
import cds.gen.configurationservice.BusinessObjectConfigurations;
import cds.gen.configurationservice.BusinessObjectConfigurations_;

@Component
public class BusinessObjectConfigurationValidationImpl implements BusinessObjectConfigurationValidation {

    @Autowired
    BusinessObjectConfigurationDao businessObjectConfigurationDao;

    @Autowired
    Messages messages;

    @Autowired
    SecurityValidator securityValidator;

    @Autowired
    ConfigurationFieldsValidation configurationFieldsValidation;

    @Autowired
    PersistenceService db;

    private static final Logger LOG = LoggerFactory.getLogger(BusinessObjectConfigurationValidationImpl.class);

    /**
     * Common method to call BusinessObjectConfigurations validation of destination and sub-item attribute
     *
     * @param {@link BusinessObjectConfigurations} businessObjectConfigurations
     *
     * @public
     */
    @Override
    public void validateBusinessObjectConfiguration(BusinessObjectConfigurations businessObjectConfigurations){
        if(!validateBusinessObjectType(businessObjectConfigurations.getBusinessObjectTypeCode(),businessObjectConfigurations.getBusinessObjectAttributeCode())){
                messages.error(MessageKeys.BO_TYPE_NOT_MATCHING).target("in", BusinessObjectConfigurations_.class,
                        BusinessObjectConfigurations_::businessObjectType_code);
        }
        if(!configurationFieldsValidation.validateDestination(businessObjectConfigurations.getDestination())){
            messages.error(MessageKeys.DESTINATION_IS_MANDATORY).target("in", BusinessObjectConfigurations_.class,
                    BusinessObjectConfigurations_::destination);
        }
        if(!configurationFieldsValidation.validateDestinationValue(businessObjectConfigurations.getDestination())){
            messages.error(MessageKeys.DESTINATION_IS_INVALID).target("in", BusinessObjectConfigurations_.class,
            BusinessObjectConfigurations_::destination);
        }
        if(!configurationFieldsValidation.validateComplaintType(businessObjectConfigurations.getComplaintTypeCode())){
            messages.error(MessageKeys.COMPLAINT_TYPE_IS_MANDATORY).target("in", BusinessObjectConfigurations_.class,
                    BusinessObjectConfigurations_::complaintType_code);
        }
        if(!configurationFieldsValidation.validateBusinessObjectType(businessObjectConfigurations.getBusinessObjectTypeCode())){
            messages.error(MessageKeys.BO_TYPE_IS_MANDATORY).target("in", BusinessObjectConfigurations_.class,
                    BusinessObjectConfigurations_::businessObjectType_code);
        }
        if(!configurationFieldsValidation.validateBusinessObjectAttribute(businessObjectConfigurations.getBusinessObjectAttributeCode())){
            messages.error(MessageKeys.BO_ATTRIBUTES_IS_MANDATORY).target("in", BusinessObjectConfigurations_.class,
                    BusinessObjectConfigurations_::businessObjectAttribute_code);
        }
        if(configurationFieldsValidation.validateBusinessObjectValue(businessObjectConfigurations.getBusinessObjectValue())){
            messages.error(MessageKeys.VALUE_IS_MANDATORY).target("in", BusinessObjectConfigurations_.class,
                    BusinessObjectConfigurations_::businessObjectValue);
        }
        if(!configurationFieldsValidation.validateBusinessObjectValueIfExist(businessObjectConfigurations.getBusinessObjectValue())){
            messages.error(MessageKeys.INVALID_BUSINESS_OBJECT_VALUE).target("in", BusinessObjectConfigurations_.class,
                    BusinessObjectConfigurations_::businessObjectValue);
        }
        if (messages.stream().noneMatch(message -> message.getSeverity() == Severity.ERROR)) {
            Result businessObjectConfig = businessObjectConfigurationDao.getBusinessObjectConfigBasedOnDestinationAndBO(businessObjectConfigurations.getComplaintTypeCode(), businessObjectConfigurations.getDestination(), businessObjectConfigurations.getBusinessObjectTypeCode(), businessObjectConfigurations.getBusinessObjectAttributeCode());
            Optional<Row> businessObjectConfigFirst = businessObjectConfig.first();
            if (businessObjectConfigFirst.isPresent() && !businessObjectConfigurations.getId().equals(businessObjectConfigFirst.get().get("ID").toString())) {
                messages.error(MessageKeys.ONLY_ONE_COMPLAINT_TYPE_PER_DESTINATION).target("in", BusinessObjectConfigurations_.class,
                        BusinessObjectConfigurations_::destination);
            }
        }
    }

    public boolean validateBusinessObjectType(String boType, String boAttr){
        LOG.info("Business Object Type is {}",boType);
        LOG.info("Business Object Attribute is {}",boAttr);
        Result businessObjectAttributes = db.run(Select.from(BusinessObjectAttributes_.class)
                .where(b -> b.businessObjectAttribute().eq(boAttr)));
        if(businessObjectAttributes.first().isPresent()){
            for (Row row : businessObjectAttributes) {
                String bObjectType = row.get("businessObjectType").toString();
                if(boType.equals(bObjectType)){
                    LOG.info("Business Object Type for selected BO Attribute is {}",bObjectType);
                    return true;
                }
            }
        }
        return false;
    }
}
