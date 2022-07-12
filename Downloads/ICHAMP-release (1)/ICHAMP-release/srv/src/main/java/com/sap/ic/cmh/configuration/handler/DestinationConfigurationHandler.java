package com.sap.ic.cmh.configuration.handler;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import com.sap.cds.Row;
import com.sap.cds.services.cds.CdsDeleteEventContext;
import com.sap.cds.services.cds.CdsReadEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.After;
import com.sap.cds.services.handler.annotations.Before;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;

import com.sap.ic.cmh.configuration.persistency.DestinationConfigurationDao;
import com.sap.ic.cmh.configuration.service.ConfigurationService;
import com.sap.ic.cmh.configuration.validations.DestinationConfigurationValidation;
import com.sap.ic.cmh.utils.CqnAnalyzerUtil;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.auditlog.AuditLogHelper;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import cds.gen.configurationservice.DestinationConfigurations;
import cds.gen.configurationservice.DestinationConfigurations_;
import cds.gen.configurationservice.Destinations_;
import cds.gen.configurationservice.Destinations;
import com.sap.ic.cmh.auditlog.AuditLogDifference;
import com.sap.ic.cmh.auditlog.ObjectDiff;

@Component
@ServiceName("ConfigurationService")
public class DestinationConfigurationHandler implements EventHandler {
	
	@Autowired
    DestinationConfigurationDao destinationConfigurationDao;

    @Autowired
    DestinationConfigurationValidation validator;

    @Autowired
    Messages messages;

    @Autowired
    private AuditLogHelper auditLogHelper;

    @Autowired
    private CqnAnalyzerUtil cqnAnalyzerUtil;

    @Autowired
    ConfigurationService configurationService;

    @Autowired
    private AuditLogDifference<DestinationConfigurations> auditLogDifference;
    
    private static final Logger logger = LoggerFactory.getLogger(DestinationConfigurationHandler.class);
    private static final String DESTINATION_CONFIG_HANDLER = "DestinationConfigurationHandler";
    private static final String ENTITY_NAME = "DestinationConfiguration";

    /**
	 * Before Create event of Destination Configuration perform validations
	 * 
	 * @param {@link DestinationConfiguration} destinationConfiguration
	 * 
	 * @public
	 */
    @Before(event = {CdsService.EVENT_CREATE,CdsService.EVENT_UPDATE}, entity = DestinationConfigurations_.CDS_NAME)
    public void beforeDestinationConfigurationCreateUpdate(DestinationConfigurations destinationConfiguration) {
    	LoggerHelper.logMethodEntry(logger, DESTINATION_CONFIG_HANDLER, "beforeDestinationConfigurationCreateUpdate");
        validator.validateDestinationConfiguration(destinationConfiguration);
        messages.throwIfError();
        LoggerHelper.logMethodExit(logger, DESTINATION_CONFIG_HANDLER, "beforeDestinationConfigurationCreateUpdate");
    }

    /**
	 * On Create event of Destination Configuration perform set default
	 * values
	 * 
	 * @param {@link DestinationConfiguration} destinationConfiguration
	 * 
	 * @public
	 */
    @On(event = {CdsService.EVENT_CREATE}, entity = DestinationConfigurations_.CDS_NAME)
    public void onCreateDestinationConfiguration(DestinationConfigurations destinationConfiguration) {
        LoggerHelper.logMethodEntry(logger, DESTINATION_CONFIG_HANDLER, "onDestinationConfigurationCreate");
        Optional<Row> destinationConfigurationFirst = destinationConfigurationDao.getDestinationConfiguration().first();
        Integer sequenceNumber = (destinationConfigurationFirst.isPresent()&&null!=destinationConfigurationFirst.get().get("identifier")) ? Integer.parseInt(destinationConfigurationFirst.get().get("identifier").toString()) + 1 : 1;
        destinationConfiguration.setIdentifier(sequenceNumber);
        LoggerHelper.logMethodExit(logger, DESTINATION_CONFIG_HANDLER, "onDestinationConfigurationCreate");
        
    }

     /**
	 * After read event of Destination Configuration perform get all destination from cf and set to Destinations API endpoint
     * 
	 * @param {@link scpCfDestinationLoader} destinationService
	 * 
	 * @public
	 */
    @After(event = {CdsService.EVENT_READ}, entity = Destinations_.CDS_NAME)
    public List<Destinations> afterDestinationConfigurationRead(CdsReadEventContext context) {
       LoggerHelper.logMethodEntry(logger, DESTINATION_CONFIG_HANDLER, "afterDestinationConfigurationRead");
       return configurationService.getallDestinationsFromBTP();
    }

    @Before(event = { CdsService.EVENT_UPDATE }, entity = DestinationConfigurations_.CDS_NAME )
    public void beforeDestinationConfigurationUpdate(DestinationConfigurations data){
        setOldAuditData(data);
    }


    @After(event = { CdsService.EVENT_CREATE, CdsService.EVENT_UPDATE }, entity = DestinationConfigurations_.CDS_NAME )
    public void afterDestinationConfigurationCreateUpdate(DestinationConfigurations data){
        logUpsert(data);
    }

    @After(event = CdsService.EVENT_DELETE, entity = DestinationConfigurations_.CDS_NAME)
    public void afterDestinationConfigurationsDeletion(CdsDeleteEventContext context) {
        Map<String, Object> keys = cqnAnalyzerUtil.provideTargetKeys(context);
        logDelete((String)keys.get(DestinationConfigurations.ID));
    }

    public void setOldAuditData(DestinationConfigurations destinationConfiguration) {
        DestinationConfigurations oldData = configurationService.getDestinationConfigDetails(destinationConfiguration.getId());
        auditLogDifference.setOldData(oldData);
    }
    
    public void logUpsert(DestinationConfigurations destinationConfiguration) {
        DestinationConfigurations newData = configurationService.getDestinationConfigDetails(destinationConfiguration.getId());
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
        auditLogHelper.logDeleteAuditData(diffList, entityInfoMap);
    }

}
