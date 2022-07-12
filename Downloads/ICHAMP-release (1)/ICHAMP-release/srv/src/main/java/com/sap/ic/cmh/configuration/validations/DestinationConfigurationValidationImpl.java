package com.sap.ic.cmh.configuration.validations;

import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.configuration.persistency.DestinationConfigurationDao;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.cds.services.messages.Message.Severity;

import com.sap.ic.cmh.utils.SecurityValidator;
import java.util.Optional;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import cds.gen.configurationservice.DestinationConfigurations;
import cds.gen.configurationservice.DestinationConfigurations_;

@Component
public class DestinationConfigurationValidationImpl implements DestinationConfigurationValidation {
    

    @Autowired
    DestinationConfigurationDao destinationConfigurationDao;

    @Autowired
    Messages messages;

    @Autowired
    ConfigurationFieldsValidation configurationFieldsValidation;

    @Autowired
    SecurityValidator securityValidator;
    
    private static final Logger logger = LoggerFactory.getLogger(DestinationConfigurationValidationImpl.class);
	private static final String DESTINAION_CONFIG_VALIDATION_IMPL = "DestinationConfigurationValidationImpl";

    /**
	 * Common method to call business validation of company code and destination
	 *
	 * @param {@link ServiceMaterials} serviceMaterials
	 *
	 * @public
	 */
    @Override
    public void validateDestinationConfiguration(DestinationConfigurations destinationConfiguration){
    	LoggerHelper.logMethodEntry(logger, DESTINAION_CONFIG_VALIDATION_IMPL, "validateDestinationConfiguration");
        if(!configurationFieldsValidation.validateDestination(destinationConfiguration.getDestination())){
            messages.error(MessageKeys.DESTINATION_IS_MANDATORY).target("in", DestinationConfigurations_.class,
                    DestinationConfigurations_::destination);
        }
        if(StringUtils.isNotBlank(destinationConfiguration.getNavigationDestination()) && !configurationFieldsValidation.validateDestinationValue(destinationConfiguration.getNavigationDestination())){
            messages.error(MessageKeys.NAVIGATION_DESTINATION_IS_INVALID).target("in", DestinationConfigurations_.class,
                    DestinationConfigurations_::navigationDestination);
        }
        if(StringUtils.isNotBlank(destinationConfiguration.getDestination()) && !configurationFieldsValidation.validateDestinationValue(destinationConfiguration.getDestination())){
            messages.error(MessageKeys.DESTINATION_IS_INVALID).target("in", DestinationConfigurations_.class,
                    DestinationConfigurations_::destination);
        }
        if(!configurationFieldsValidation.validateCompanyCode(destinationConfiguration.getCompanyCodeId())){
            messages.error(MessageKeys.COMPANY_CODE_IS_MANDATORY).target("in",DestinationConfigurations_.class,
                    DestinationConfigurations_::companyCode_ID);
        }
        if(!configurationFieldsValidation.validateBusinessObjectType(destinationConfiguration.getBusinessObjectTypeCode())){
            messages.error(MessageKeys.BO_TYPE_IS_MANDATORY).target("in", DestinationConfigurations_.class,
                    DestinationConfigurations_::businessObjectType_code);
        }
        if(destinationConfiguration.getDescription()!=null && !securityValidator.isValidText(destinationConfiguration.getDescription())){
            messages.error(MessageKeys.INVALID_DESTINATION_DESCRIPTION).target("in", DestinationConfigurations_.class,
                    DestinationConfigurations_::description);
        }
        if (messages.stream().noneMatch(message -> message.getSeverity() == Severity.ERROR)) {
        Result destinationConfigurations = destinationConfigurationDao.getDestinationConfigBasedOnCompanyAndDestination(destinationConfiguration.getCompanyCodeId(), destinationConfiguration.getBusinessObjectTypeCode());
        Optional<Row> destinationConfigurationsFirst = destinationConfigurations.first();    
        if(destinationConfigurationsFirst.isPresent() && !destinationConfiguration.getId().equals(destinationConfigurationsFirst.get().get("ID").toString())) {
            messages.error(MessageKeys.ONLY_ONE_COMPANY_CODE_PER_DESTINATION).target("in", DestinationConfigurations_.class,
                    DestinationConfigurations_::companyCode_ID);
        }
    }
        LoggerHelper.logMethodExit(logger, DESTINAION_CONFIG_VALIDATION_IMPL, "validateDestinationConfiguration");
    }
}
