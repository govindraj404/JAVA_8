package com.sap.ic.cmh.configuration.handler;

import cds.gen.configurationservice.ClaimStatusMappings;
import cds.gen.configurationservice.ClaimStatusMappings_;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import com.sap.cds.Row;
import com.sap.cds.services.cds.CdsDeleteEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.After;
import com.sap.cds.services.handler.annotations.Before;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.configuration.persistency.ClaimStatusMappingsDao;
import com.sap.ic.cmh.configuration.validations.ClaimStatusMappingsValidation;
import com.sap.ic.cmh.utils.CqnAnalyzerUtil;
import com.sap.ic.cmh.utils.LoggerHelper;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import com.sap.ic.cmh.auditlog.AuditLogDifference;
import com.sap.ic.cmh.auditlog.ObjectDiff;
import com.sap.ic.cmh.configuration.service.ConfigurationService;

@Component
@ServiceName("ConfigurationService")
public class ClaimStatusMappingsHandler implements EventHandler {

    @Autowired
    ClaimStatusMappingsValidation claimStatusValidation;

    @Autowired
    Messages messages;

    @Autowired
    private AuditLogHelper auditLogHelper;
    @Autowired
    ClaimStatusMappingsDao claimStatusMappingsDao;
    @Autowired
    private CqnAnalyzerUtil cqnAnalyzerUtil;
    @Autowired
    private AuditLogDifference<ClaimStatusMappings> auditLogDifference;

    @Autowired
    ConfigurationService configurationService;
    
    private static final String ENTITY_NAME = "ClaimStatusMappings";
    private static final Logger logger = LoggerFactory.getLogger(ClaimStatusMappingsHandler.class);
    private static final String CLAIM_STATUS_MAPPING_HANDLER = "ClaimStatusMappingsHandler";

    /**
     * Before Create event of Claim Statuses perform validations
     *
     * @param {@link ClaimStatusMappings} claimStatusMappings
     *
     * @public
     */
    @Before(event = {CdsService.EVENT_CREATE, CdsService.EVENT_UPDATE}, entity = ClaimStatusMappings_.CDS_NAME)
    public void beforeCreateClaimStatus(ClaimStatusMappings claimStatusMappings) {
    	LoggerHelper.logMethodEntry(logger, CLAIM_STATUS_MAPPING_HANDLER, "beforeCreateClaimStatus");
        claimStatusValidation.validateClaimStatuses(claimStatusMappings);
        messages.throwIfError();
        LoggerHelper.logMethodExit(logger, CLAIM_STATUS_MAPPING_HANDLER, "beforeCreateClaimStatus");
    }
    
    @On(event = {CdsService.EVENT_CREATE}, entity = ClaimStatusMappings_.CDS_NAME)
    public void onCreateClaimStatus(ClaimStatusMappings claimStatusMappings) {
    	LoggerHelper.logMethodEntry(logger, CLAIM_STATUS_MAPPING_HANDLER, "onCreateClaimStatus");
    	Optional<Row> getClaimStatusMappings = claimStatusMappingsDao.getClaimStatusMappingsBasedOnIdentifier().first();
		Integer sequenceNumber = (getClaimStatusMappings.isPresent()&&null!=getClaimStatusMappings.get().get("identifier")) ? Integer.parseInt(getClaimStatusMappings.get().get("identifier").toString()) + 1 : 1;
		claimStatusMappings.setIdentifier(sequenceNumber);
		LoggerHelper.logMethodExit(logger, CLAIM_STATUS_MAPPING_HANDLER, "onCreateClaimStatus");
    }

    @After(event = { CdsService.EVENT_CREATE, CdsService.EVENT_UPDATE }, entity = ClaimStatusMappings_.CDS_NAME )
    public void afterClaimStatusCreateUpdate(ClaimStatusMappings data){
    	LoggerHelper.logMethodEntry(logger, CLAIM_STATUS_MAPPING_HANDLER, "afterClaimStatusCreateUpdate");
        logUpsert(data);
        LoggerHelper.logMethodExit(logger, CLAIM_STATUS_MAPPING_HANDLER, "afterClaimStatusCreateUpdate");
    }

    @Before(event = {CdsService.EVENT_UPDATE }, entity = ClaimStatusMappings_.CDS_NAME)
    public void beforeClaimStatusUpdate(ClaimStatusMappings data){
        setOldAuditData(data);
    }
    
    

    @After(event = CdsService.EVENT_DELETE, entity = ClaimStatusMappings_.CDS_NAME)
    public void afterClaimStatusDeletion(CdsDeleteEventContext context) {
    	LoggerHelper.logMethodEntry(logger, CLAIM_STATUS_MAPPING_HANDLER, "afterClaimStatusDeletion");
        Map<String, Object> keys = cqnAnalyzerUtil.provideTargetKeys(context);
        logDelete((String)keys.get(ClaimStatusMappings.ID));
        LoggerHelper.logMethodExit(logger, CLAIM_STATUS_MAPPING_HANDLER, "afterClaimStatusDeletion");
    }




    public void setOldAuditData(ClaimStatusMappings claimStatusMappings) {
        ClaimStatusMappings oldData = configurationService.getclaimStatusMappingsDetails(claimStatusMappings.getId());
        auditLogDifference.setOldData(oldData);
    } 
    
    public void logUpsert(ClaimStatusMappings claimStatusMappings) {
        ClaimStatusMappings newData = configurationService.getclaimStatusMappingsDetails(claimStatusMappings.getId());
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
