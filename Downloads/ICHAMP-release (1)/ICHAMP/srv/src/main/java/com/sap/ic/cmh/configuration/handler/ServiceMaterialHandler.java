package com.sap.ic.cmh.configuration.handler;

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
import com.sap.ic.cmh.configuration.persistency.ServiceMaterialDao;
import com.sap.ic.cmh.configuration.validations.ServiceMaterialValidation;
import com.sap.ic.cmh.configuration.validations.ServiceMaterialUnitValidation;
import com.sap.ic.cmh.utils.CqnAnalyzerUtil;
import com.sap.ic.cmh.utils.LoggerHelper;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import cds.gen.configurationservice.ServiceMaterials;
import cds.gen.configurationservice.ServiceMaterials_;
import com.sap.ic.cmh.auditlog.AuditLogDifference;
import com.sap.ic.cmh.auditlog.ObjectDiff;
import com.sap.ic.cmh.configuration.service.ConfigurationService;

@Component
@ServiceName("ConfigurationService")
public class ServiceMaterialHandler implements EventHandler {

    @Autowired
    ServiceMaterialDao serviceMaterialDao;

    @Autowired
    ServiceMaterialValidation serviceMaterialValidator;

    @Autowired
    Messages messages;

    @Autowired
    private AuditLogHelper auditLogHelper;

    @Autowired
    private CqnAnalyzerUtil cqnAnalyzerUtil;

    @Autowired
    ServiceMaterialUnitValidation serviceMaterialUnitValidator;

    @Autowired
    private AuditLogDifference<ServiceMaterials> auditLogDifference;

    @Autowired
    ConfigurationService configurationService;
    
    private static final Logger logger = LoggerFactory.getLogger(ServiceMaterialHandler.class);
    private static final String SERVICE_MATERIAL_HANDLER = "ServiceMaterialHandler";
    private static final String ENTITY_NAME = "ServiceMaterial";


    /**
	 * Before Create event of Service Materials perform validations
	 * 
	 * @param {@link ServiceMaterials} serviceMaterials
	 * 
	 * @public
	 */
    @Before(event = {CdsService.EVENT_CREATE,CdsService.EVENT_UPDATE}, entity = ServiceMaterials_.CDS_NAME)
    public void beforeServiceMaterialCreateUpdate(ServiceMaterials serviceMaterial) {
    	LoggerHelper.logMethodEntry(logger, SERVICE_MATERIAL_HANDLER, "beforeServiceMaterialCreateUpdate");
        serviceMaterialValidator.validateServiceMaterials(serviceMaterial);
        serviceMaterialUnitValidator.validateServiceMaterialUnits(serviceMaterial);
        messages.throwIfError();
        LoggerHelper.logMethodEntry(logger, SERVICE_MATERIAL_HANDLER, "beforeServiceMaterialCreateUpdate");
    }

    /**
	 * On Create event of Service Materials perform set default
	 * values
	 * 
	 * @param {@link ServiceMaterials} serviceMaterials
	 * 
	 * @public
	 */
    @On(event = {CdsService.EVENT_CREATE}, entity = ServiceMaterials_.CDS_NAME)
    public void onCreateServiceMaterial(ServiceMaterials serviceMaterial) {
        LoggerHelper.logMethodEntry(logger, SERVICE_MATERIAL_HANDLER, "onServiceMaterialCreate");
        Optional<Row> serviceMaterialFirst = serviceMaterialDao.getServiceMaterials().first();
        Integer sequenceNumber = (serviceMaterialFirst.isPresent()&&null!=serviceMaterialFirst.get().get("identifier")) ? Integer.parseInt(serviceMaterialFirst.get().get("identifier").toString()) + 1 : 1;
        serviceMaterial.setIdentifier(sequenceNumber);
        LoggerHelper.logMethodEntry(logger, SERVICE_MATERIAL_HANDLER, "onServiceMaterialCreate");
    }

    
    @Before(event = {CdsService.EVENT_UPDATE }, entity = ServiceMaterials_.CDS_NAME)
    public void beforeServiceMaterialUpdate(ServiceMaterials data){
        setOldAuditData(data);
    }

    @After(event = { CdsService.EVENT_CREATE, CdsService.EVENT_UPDATE }, entity = ServiceMaterials_.CDS_NAME )
    public void afterServiceMaterialCreateUpdate(ServiceMaterials data){
        logUpsert(data);
    }

    @After(event = CdsService.EVENT_DELETE, entity = ServiceMaterials_.CDS_NAME)
    public void afterServiceMaterialsDeletion(CdsDeleteEventContext context) {
        Map<String, Object> keys = cqnAnalyzerUtil.provideTargetKeys(context);
        logDelete((String)keys.get(ServiceMaterials.ID));
    }

    public void setOldAuditData(ServiceMaterials serviceMaterial) {
        ServiceMaterials oldData = configurationService.getServiceMaterialsDetails(serviceMaterial.getId());
        auditLogDifference.setOldData(oldData);
    } 
    
    public void logUpsert(ServiceMaterials serviceMaterial) {
        ServiceMaterials newData = configurationService.getServiceMaterialsDetails(serviceMaterial.getId());
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
