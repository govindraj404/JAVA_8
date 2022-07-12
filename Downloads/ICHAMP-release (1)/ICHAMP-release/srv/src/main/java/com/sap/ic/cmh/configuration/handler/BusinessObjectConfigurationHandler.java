package com.sap.ic.cmh.configuration.handler;

import java.util.List;
import java.util.Map;
import java.util.Optional;

import com.sap.ic.cmh.utils.CqnAnalyzerUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.sap.cds.Row;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.After;
import com.sap.cds.services.handler.annotations.Before;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.configuration.persistency.BusinessObjectConfigurationDao;
import com.sap.ic.cmh.configuration.validations.BusinessObjectConfigurationValidation;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.cds.services.cds.CdsDeleteEventContext;
import com.sap.ic.cmh.auditlog.AuditLogDifference;
import com.sap.ic.cmh.auditlog.ObjectDiff;
import com.sap.ic.cmh.configuration.service.ConfigurationService;



import cds.gen.configurationservice.BusinessObjectConfigurations;
import cds.gen.configurationservice.BusinessObjectConfigurations_;

@Component
@ServiceName("ConfigurationService")
public class BusinessObjectConfigurationHandler implements EventHandler {

	@Autowired
    BusinessObjectConfigurationDao businessObjectConfigurationDao;

    @Autowired
    BusinessObjectConfigurationValidation validator;

    @Autowired
    Messages messages;

    @Autowired
	private AuditLogHelper auditLogHelper;

    @Autowired
    private CqnAnalyzerUtil cqnAnalyzerUtil;

    @Autowired
    private AuditLogDifference<BusinessObjectConfigurations> auditLogDifference;

    @Autowired
    ConfigurationService configurationService;

    
    
	 private static final Logger logger = LoggerFactory.getLogger(BusinessObjectConfigurationHandler.class);
     private static final String BUSINESS_OBJECT_CONFIG_HANDLER = "BusinessObjectConfigurationHandler";
     private static final String ENTITY_NAME = "BusinessObjectConfiguration";
    
    /**
	 * Before Create event of Business Object Configuration perform validations
	 * 
	 * @param {@link BusinessObjectConfiguration} businessObjectConfiguration
	 * 
	 * @public
	 */
    @Before(event = {CdsService.EVENT_CREATE,CdsService.EVENT_UPDATE}, entity = BusinessObjectConfigurations_.CDS_NAME)
    public void beforeBusinessObjectConfigurationCreateUpdate(BusinessObjectConfigurations businessObjectConfiguration) {
        LoggerHelper.logMethodEntry(logger, BUSINESS_OBJECT_CONFIG_HANDLER, "beforeBusinessObjectConfigurationCreateUpdate");
        validator.validateBusinessObjectConfiguration(businessObjectConfiguration);
        messages.throwIfError();
        LoggerHelper.logMethodExit(logger, BUSINESS_OBJECT_CONFIG_HANDLER, "beforeBusinessObjectConfigurationCreateUpdate");
    }

    /**
	 * On Create event of Business Object Configuration perform set default
	 * values
	 * 
	 * @param {@link BusinessObjectConfiguration} businessObjectConfiguration
	 * 
	 * @public
	 */
    @On(event = {CdsService.EVENT_CREATE}, entity = BusinessObjectConfigurations_.CDS_NAME)
    public void onCreateBusinessObjectConfiguration(BusinessObjectConfigurations businessObjectConfiguration) {
        LoggerHelper.logMethodEntry(logger, BUSINESS_OBJECT_CONFIG_HANDLER, "onBusinessObjectConfigurationCreate");
        Optional<Row> getBusinessObjectConfigurations = businessObjectConfigurationDao.getBusinessObjectConfigurations().first();
		Integer sequenceNumber = (getBusinessObjectConfigurations.isPresent()&&null!=getBusinessObjectConfigurations.get().get("identifier")) ? Integer.parseInt(getBusinessObjectConfigurations.get().get("identifier").toString()) + 1 : 1;
        businessObjectConfiguration.setIdentifier(sequenceNumber);
        LoggerHelper.logMethodExit(logger, BUSINESS_OBJECT_CONFIG_HANDLER, "onBusinessObjectConfigurationCreate");
    }

    @After(event = { CdsService.EVENT_CREATE, CdsService.EVENT_UPDATE }, entity = BusinessObjectConfigurations_.CDS_NAME )
    public void afterBusinessObjectCreateUpdate(BusinessObjectConfigurations data){
        logUpsert(data);
    }

    @Before(event= { CdsService.EVENT_UPDATE }, entity = BusinessObjectConfigurations_.CDS_NAME )
    public void beforeBusinessObjectCreateUpdate(BusinessObjectConfigurations data){
        setOldAuditData(data);
    }

    @After(event = CdsService.EVENT_DELETE, entity = BusinessObjectConfigurations_.CDS_NAME)
    public void afterBusinessObjectDeletion(CdsDeleteEventContext context ) {
        Map<String, Object> keys = cqnAnalyzerUtil.provideTargetKeys(context);
        logDelete((String)keys.get(BusinessObjectConfigurations.ID));
    }

    public void setOldAuditData(BusinessObjectConfigurations businessObjectConfiguration) {
        BusinessObjectConfigurations oldData = configurationService.getBusinessObjectConfigurationsDetails(businessObjectConfiguration.getId());
        auditLogDifference.setOldData(oldData);
    } 
    
    public void logUpsert(BusinessObjectConfigurations businessObjectConfiguration) {
        BusinessObjectConfigurations newData = configurationService.getBusinessObjectConfigurationsDetails(businessObjectConfiguration.getId());
        List<ObjectDiff> diffList = auditLogDifference.getDifference(newData);
        
        Map<String, String> entityInfoMap =
                auditLogHelper.buildEntityInfoMap(
                        ENTITY_NAME,
                        newData.getId());

        auditLogHelper.logUpsertAuditData(diffList, entityInfoMap);
    }

    public void logDelete(String id) {

        ObjectDiff diffList = null;
        Map<String, String> entityInfoMap =
                auditLogHelper.buildEntityInfoMap(
                        ENTITY_NAME, id);
        logger.info("****************" + entityInfoMap);
        auditLogHelper.logDeleteAuditData(diffList, entityInfoMap);
    }
}
