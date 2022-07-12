package com.sap.ic.cmh.managequalitynotification.handler;

import java.util.UUID;
import java.util.List;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.auditlog.AuditLogDifference;
import org.apache.commons.collections.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.Update;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.After;
import com.sap.cds.services.handler.annotations.Before;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.ic.cmh.complaint.persistency.ComplaintsDao;
import com.sap.ic.cmh.configuration.service.ConfigurationService;
import com.sap.ic.cmh.qualitynotification.handler.QualityNotificationHandler;
import com.sap.ic.cmh.utils.CommonFunctions;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.managequalitynotificationservice.ManageQualityNotificationService_;
import cds.gen.managequalitynotificationservice.QualityNotifications;
import cds.gen.managequalitynotificationservice.QualityNotifications_;
import cds.gen.masterdataservice.BusinessPartners;
import io.micrometer.core.instrument.util.StringUtils;
import com.sap.ic.cmh.configuration.service.MessageService;
import com.sap.cds.services.messages.Messages;
import cds.gen.managequalitynotificationservice.BusinessObjectStatuses;
import com.sap.ic.cmh.managequalitynotification.service.ManageQualityNotificationService;
import com.sap.ic.cmh.qualitynotification.service.QualityNotificationService;
import java.util.ArrayList;
import cds.gen.qualitynotificationservice.Defects;
import com.sap.ic.cmh.configuration.model.MasterData;
import cds.gen.managecomplaintservice.Complaints;
import com.sap.ic.cmh.businessobjects.service.BusinessObjectServiceImpl;

@Component
@ServiceName(ManageQualityNotificationService_.CDS_NAME)
public class ManageQualityNotificationHandler implements EventHandler {

    @Autowired
    QualityNotificationHandler qualityNotifcationHandler;

    @Autowired
    CommonFunctions commonFunctions;

    @Autowired
	QualityNotificationService qualityNotificationService;

    @Autowired
    ComplaintsDao complaintDao;

    @Autowired
    ConfigurationService configurationService;

    @Autowired
    MessageService messageService;

    @Autowired
    Messages messages;

    @Autowired
    BusinessObjectServiceImpl businessObjectService;

    @Autowired
    ManageQualityNotificationService manageQualityNotificationService;
    
    @Autowired
    @Qualifier(ManageQualityNotificationService_.CDS_NAME)
	private CdsService cdsService;

    @Autowired
	AuditLogHelper auditLogHelper;

    @Autowired
	private AuditLogDifference<cds.gen.qualitynotificationservice.QualityNotifications> auditLogDifference;

    @Autowired
	private AuditLogDifference<Defects> auditLogDifferenceDefects;

    private static final String MANAGE_QUALITY_NOTIFICATION_HANDLER = "ManageQualityNotificationHandler";
    private static final Logger logger = LoggerFactory.getLogger(ManageQualityNotificationHandler.class);
    private static volatile String qualityNotificationId;
    private static final String BO_TYPE = Constants.QUALITYNOTIFICATION_CODE;
    private static final boolean IS_ACTIVE_QN = true;

    /**
     * This method is used to call the BEFORE-CREATE event of draft Quality
     * Notification Handler 
     * 1. Create/Update automatic QN
     * 2. validate all mandatory fields required to create Quality
     * Notification through CPI
     * 
     * @param manageQualityNotifications
     */
    @Before(event = { CdsService.EVENT_CREATE }, entity = QualityNotifications_.CDS_NAME)
    public void beforeManageQualityNotificationCreate(QualityNotifications manageQualityNotifications,CdsCreateEventContext context) {
        LoggerHelper.logMethodEntry(logger, MANAGE_QUALITY_NOTIFICATION_HANDLER,
                "beforeManageQualityNotificationCreate");
        qualityNotificationId = UUID.randomUUID().toString();
        String qnId = manageQualityNotificationService.checkIfActiveQNExistsBasedOnNumber
        (null!=manageQualityNotifications.getIdentifier() ? manageQualityNotifications.getIdentifier() : "");
        if(StringUtils.isNotBlank(qnId)){
            updateQualityNotifications(manageQualityNotifications, context, qnId);
           
        }else{
        	//Create QN manually if complaint code exists in payload
        	//else create automatic complaint and QN
           if (StringUtils.isNotBlank(manageQualityNotifications.getComplaintCode())) {
            beforCreateManualQualityNotifications(manageQualityNotifications);

        } else {
            beforeCreateAutomaticQualityNotifications(manageQualityNotifications);
        }
    }
        LoggerHelper.logMethodExit(logger, MANAGE_QUALITY_NOTIFICATION_HANDLER,
                "beforeManageQualityNotificationCreate");

    }
    
    /**
     * Fetch IDs for all Master data based on the codes
     * @param manageQualityNotifications
     */
	public void beforeCreateAutomaticQualityNotifications(QualityNotifications manageQualityNotifications) {
		MasterData masterDataDetailsByCodes = manageQualityNotificationService
		        .getMasterDataIdsBasedOnDetails(manageQualityNotifications);
		if (masterDataDetailsByCodes.getMaterial() != null) {
		    manageQualityNotifications.setMaterialId(masterDataDetailsByCodes.getMaterial().getId());
		}
		if (masterDataDetailsByCodes.getPlants() != null) {
		    manageQualityNotifications.setPlantId(masterDataDetailsByCodes.getPlants().getId());
		}
		if (masterDataDetailsByCodes.getSupplier() != null) {
		    manageQualityNotifications.setSupplierId(masterDataDetailsByCodes.getSupplier().getId());
		}
		if (masterDataDetailsByCodes.getPersonResponsible() != null) {
		    manageQualityNotifications
		            .setPersonResponsibleId(masterDataDetailsByCodes.getPersonResponsible().getId());
		}
		if (masterDataDetailsByCodes.getPurchaseOrg() != null) {
		    manageQualityNotifications
		            .setPurchasingOrganizationId(masterDataDetailsByCodes.getPurchaseOrg().getId());
		}
	}
    
    /**
     * Create Quality Notification through CPI
     * @param manageQualityNotifications
     */
	public void beforCreateManualQualityNotifications(QualityNotifications manageQualityNotifications) {
		setMasterDataDetails(manageQualityNotifications);
		cds.gen.qualitynotificationservice.Defects defects = commonFunctions
		        .convertManageDefectsToDraftDefects(manageQualityNotifications.getDefect());

		cds.gen.qualitynotificationservice.QualityNotifications qualityNotifications = commonFunctions
		        .convertManageQualitynotificationtoQualityNotifications(manageQualityNotifications);
		qualityNotifications.setId(qualityNotificationId);
		qualityNotifications.setDefect(defects);
		qualityNotifications.setStatusCode(Constants.STATUS_NEW);
		Result comp = complaintDao.getComplaintBasedOnCode(manageQualityNotifications.getComplaintCode());
		if (comp.first().isPresent()) {
		    qualityNotifications.setComplaintId(comp.first().get().get("ID").toString());
		    manageQualityNotifications.setComplaintId(comp.first().get().get("ID").toString());
		}
		qualityNotifcationHandler.beforeQualityNotificationCreate(qualityNotifications);
		manageQualityNotifications.setQnType(qualityNotifications.getQnType());
		manageQualityNotifications.setPersonResponsibleRole(qualityNotifications.getPersonResponsibleRole());
		manageQualityNotifications.setSupplierRole(qualityNotifications.getSupplierRole());
		manageQualityNotifications.setMaterialId(qualityNotifications.getMaterialId());
		manageQualityNotifications.setSupplierId(qualityNotifications.getSupplierId());
		manageQualityNotifications.setPlantId(qualityNotifications.getPlantId());
		manageQualityNotifications.setPurchasingOrganizationId(qualityNotifications.getPurchasingOrganizationId());
		manageQualityNotifications.setCompanyId(qualityNotifications.getCompanyId());
		manageQualityNotifications.setQuantity(qualityNotifications.getQuantity());
		manageQualityNotifications.setUnit(qualityNotifications.getUnit());
	}
    
    /**
     * Update the Quality Notification since QN already exists
     * @param manageQualityNotifications
     * @param context
     * @param qnId
     */
	public void updateQualityNotifications(QualityNotifications manageQualityNotifications,
			CdsCreateEventContext context, String qnId) {
		logger.info("QN Update ....");
		manageQualityNotifications.setId(qnId);
		CqnUpdate update = Update.entity(QualityNotifications_.class).data(manageQualityNotifications);
         logger.info("QN Updated ");
         context.setResult(cdsService.run(update));
         context.setCompleted();
	}

    /**
     * This method is used to call the ON-CREATE event of draft Quality Notification
     * Handler create Quality Notification in the configured destination Link the
     * Business object with complaint Insert business object status
     * 
     * @param manageQualityNotifications
     */
    @On(event = CdsService.EVENT_CREATE, entity = QualityNotifications_.CDS_NAME)
    public void onManageQualityNotificationCreate(QualityNotifications manageQualityNotifications) {
        LoggerHelper.logMethodEntry(logger, MANAGE_QUALITY_NOTIFICATION_HANDLER, "onManageQualityNotificationCreate");
        logger.info("start of On handler");

        cds.gen.qualitynotificationservice.QualityNotifications qualityNotifications = commonFunctions
                .convertManageQualitynotificationtoQualityNotifications(manageQualityNotifications);

        qualityNotifications.setId(qualityNotificationId);
        logger.info("manageQualityNotifications onManageQualityNotificationCreate :: {} ",
                manageQualityNotifications.getId());
        if (qualityNotifications.getIdentifier() == null) {
            qualityNotifcationHandler.onQualityNotificationCreate(qualityNotifications);
            manageQualityNotifications.setIdentifier(qualityNotifications.getIdentifier());
            manageQualityNotifications.setDefect(qualityNotifications.getDefect());
            manageQualityNotifications.setStatusCode(qualityNotifications.getStatusCode());
            manageQualityNotifications.setBusinessObjectStatuses(qualityNotifications.getBusinessObjectStatuses());
        } else {
            Complaints complaint = messageService.createComplaintAndQN(manageQualityNotifications);
            manageQualityNotifications.setComplaintId(complaint.getId());
            manageQualityNotifications.setCompanyId(complaint.getCompanyCodeId());
            qualityNotifications.setComplaintId(complaint.getId());
            qualityNotifications.setCompanyId(complaint.getCompanyCodeId());
            qualityNotifcationHandler.updateBusinessObjectIdInBusinessObjects(qualityNotifications, BO_TYPE);
            qualityNotifcationHandler.setBOAndComplaintStatusOnCreateUpdateQn(qualityNotifications,
                    qualityNotifications.getStatusCode(), IS_ACTIVE_QN);
            List<BusinessObjectStatuses> mapQNStatus = manageQualityNotificationService
                    .mapQNStatus(manageQualityNotifications, manageQualityNotifications.getBusinessObjectStatuses());
            manageQualityNotifications
                    .setBusinessObjectStatuses(!CollectionUtils.isEmpty(mapQNStatus) ? mapQNStatus : new ArrayList<>());
            qualityNotifications.setStatusCode(manageQualityNotifications.getStatusCode());
            logger.info("QN Header Status :: {} ", qualityNotifications.getStatusCode());
            manageQualityNotifications.setId(qualityNotifications.getId());
            businessObjectService.checkIfCreatedStatusExists(Constants.QN_STATUS_CREATED,
                    Constants.QUALITYNOTIFICATION_CODE, manageQualityNotifications.getId(), IS_ACTIVE_QN);
        }

        logger.info("qualityNotifications onManageQualityNotificationCreate :: ", qualityNotifications.getId());
        LoggerHelper.logMethodExit(logger, MANAGE_QUALITY_NOTIFICATION_HANDLER, "onManageQualityNotificationCreate");

        

        logger.info("end of On handler");
    }

    /**
     * This method is used to call the AFTER-CREATE and AFTER-UPDATE event of draft
     * Quality Notification Handler update Stream status according to the business
     * object status
     * 
     * @param manageQualityNotifications
     */
    @After(event = { CdsService.EVENT_CREATE }, entity = QualityNotifications_.CDS_NAME)
    public void afterManageQualityNotificationCreateUpdate(QualityNotifications manageQualityNotifications) {
        LoggerHelper.logMethodEntry(logger, MANAGE_QUALITY_NOTIFICATION_HANDLER,
                "afterManageQualityNotificationCreateUpdate");

        logger.info("start of after handler");
        cds.gen.qualitynotificationservice.QualityNotifications qualityNotifications = commonFunctions
                .convertManageQualitynotificationtoQualityNotifications(manageQualityNotifications);

        logger.info("afterQualityNotificationCreateUpdate :: {}  ", manageQualityNotifications.getId());
        qualityNotifcationHandler.afterQualityNotificationCreateUpdate(qualityNotifications);
        qualityNotifcationHandler.logUpsert(qualityNotifications);
        qualityNotifcationHandler.logUpsertDefect(qualityNotifications);
        LoggerHelper.logMethodExit(logger, MANAGE_QUALITY_NOTIFICATION_HANDLER,
                "afterManageQualityNotificationCreateUpdate");
        logger.info("end of after handler");
    }
    /**
     * Fetch Contact person and Person Responsible IDs based on their codes
     * @param manageQualityNotifications
     */
    private void setMasterDataDetails(QualityNotifications manageQualityNotifications) {
        BusinessPartners supplierPerson = configurationService
                .validateSupplierContactPerson(manageQualityNotifications.getContactPersonCode());
        if (supplierPerson != null) {
            manageQualityNotifications.setContactPersonId(supplierPerson.getId());
        }
        BusinessPartners personResponsible = configurationService
                .validatePersonResponsibleCode(manageQualityNotifications.getPersonResponsibleCode());
        if (personResponsible != null) {
            manageQualityNotifications.setPersonResponsibleId(personResponsible.getId());
        }
    }

    @Before(event = CdsService.EVENT_UPDATE, entity = QualityNotifications_.CDS_NAME)
	public void beforeManageQualityNotificationUpdate(QualityNotifications manageQualityNotifications) {
		LoggerHelper.logMethodEntry(logger, MANAGE_QUALITY_NOTIFICATION_HANDLER, "beforeManageQualityNotificationUpdate");
        logger.info("inside before event of update QN ");
        QualityNotifications qualityNotificationsToBeUpdated = Struct.create(QualityNotifications.class);
        qualityNotificationsToBeUpdated.putAll(manageQualityNotifications);
        QualityNotifications qualityNotificationsFromDB = manageQualityNotificationService.getQualityNotificationDetailsFromActive(manageQualityNotifications.getId());
        manageQualityNotifications.putAll(qualityNotificationsFromDB);
        manageQualityNotifications.putAll(qualityNotificationsToBeUpdated);
		manageQualityNotifications
				.putAll(manageQualityNotificationService.updateQualityNotification(manageQualityNotifications));
        setOldAuditData(manageQualityNotifications);
		setOldDefectAuditData(manageQualityNotifications);
        LoggerHelper.logMethodExit(logger, MANAGE_QUALITY_NOTIFICATION_HANDLER, "beforeManageQualityNotificationUpdate");
	}

    @On(event = CdsService.EVENT_UPDATE, entity = QualityNotifications_.CDS_NAME)
	public void onManageQualityNotificationUpdate(QualityNotifications manageQualityNotifications) {

        LoggerHelper.logMethodEntry(logger, MANAGE_QUALITY_NOTIFICATION_HANDLER, "onManageQualityNotificationUpdate");
		cds.gen.qualitynotificationservice.QualityNotifications qualityNotifications = commonFunctions
                .convertManageQualitynotificationtoQualityNotifications(manageQualityNotifications);
                List<BusinessObjectStatuses> mapQNStatus = manageQualityNotificationService
                .mapQNStatus(manageQualityNotifications, manageQualityNotifications.getBusinessObjectStatuses());
        manageQualityNotifications
                .setBusinessObjectStatuses(!CollectionUtils.isEmpty(mapQNStatus) ? mapQNStatus : new ArrayList<>());

        qualityNotifications.setStatusCode(manageQualityNotifications.getStatusCode());
                qualityNotifcationHandler.setBOAndComplaintStatusOnCreateUpdateQn(qualityNotifications,
                        "", IS_ACTIVE_QN);
         logger.info("BO Status after update ::  {} ",manageQualityNotifications.getBusinessObjectStatuses().size());
		LoggerHelper.logMethodExit(logger, MANAGE_QUALITY_NOTIFICATION_HANDLER, "onManageQualityNotificationUpdate");
	}
	
	

	@After(event = CdsService.EVENT_UPDATE, entity = QualityNotifications_.CDS_NAME)
	public void afterManageQualityNotificationUpdate(QualityNotifications manageQualityNotifications) {
                LoggerHelper.logMethodEntry(logger, MANAGE_QUALITY_NOTIFICATION_HANDLER,
				"afterManageQualityNotificationUpdate");
		cds.gen.qualitynotificationservice.QualityNotifications qualityNotifications = commonFunctions
                .convertManageQualitynotificationtoQualityNotifications(manageQualityNotifications);
		qualityNotifcationHandler.updateStreamStatus(qualityNotifications, Constants.QUALITYNOTIFICATION_CODE, IS_ACTIVE_QN);
        qualityNotifcationHandler.logUpsert(qualityNotifications);
        qualityNotifcationHandler.logUpsertDefect(qualityNotifications);
		logger.info("Automatic Creation of Quality Notification Business Object is completed successfully.");
		LoggerHelper.logMethodExit(logger, MANAGE_QUALITY_NOTIFICATION_HANDLER,
				"afterManageQualityNotificationUpdate");
	}

    public void setOldAuditData(QualityNotifications qn) {
		cds.gen.qualitynotificationservice.QualityNotifications oldData = qualityNotificationService.getQualityNotificationDetails(qn.getId());
		auditLogDifference.setOldData(oldData);
	}

	public void setOldDefectAuditData(QualityNotifications qn) {
		Defects oldData = qualityNotificationService.getDefectBasedOnQN(qn.getId());
		auditLogDifferenceDefects.setOldData(oldData);
	}
}
