package com.sap.ic.cmh.managequalitynotification.service;

import java.util.ArrayList;
import java.util.List;

import com.sap.ic.cmh.configuration.model.MasterData;

import cds.gen.managequalitynotificationservice.BusinessObjectStatuses;
import cds.gen.managequalitynotificationservice.QualityNotifications;
import cds.gen.masterdataservice.PurchaseOrganizations;

import com.sap.ic.cmh.configuration.service.ConfigurationService;
import org.springframework.beans.factory.annotation.Autowired;
import cds.gen.com.sap.ic.cmh.qualitynotificationstatusmapping.QualityNotificationStatusMappings;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.sap.ic.cmh.utils.Constants;
import com.sap.cds.Struct;
import com.sap.ic.cmh.businessobjects.persistency.BusinessObjectDao;
import com.sap.ic.cmh.businessobjects.service.BusinessObjectService;
import org.springframework.stereotype.Service;
import com.sap.ic.cmh.utils.LoggerHelper;
import cds.gen.returnpurchaseorderservice.ReturnPurchaseOrders;
import cds.gen.supplierissueprocessservice.Supplier8DProcesses;
import cds.gen.claimservice.Claims;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;

import com.sap.cds.Result;
import com.sap.ic.cmh.claim.service.ClaimService;
import com.sap.ic.cmh.complaint.service.ComplaintService;
import com.sap.ic.cmh.managecomplaint.service.ManageComplaintService;
import com.sap.ic.cmh.qualitynotification.persistency.QualityNotificationDao;
import com.sap.ic.cmh.qualitynotification.service.QualityNotificationService;
import com.sap.ic.cmh.returnpo.service.ReturnPurchaseOrderService;
import com.sap.ic.cmh.supplierissueprocess.service.EightDService;
import com.sap.ic.cmh.utils.CommonFunctions;
import cds.gen.managequalitynotificationservice.Defects;


@Service
public class ManageQualityNotificationServiceImpl implements ManageQualityNotificationService {

    @Autowired
	ConfigurationService configService;

    @Autowired
    BusinessObjectDao businessObjectDao;


    @Autowired
	ClaimService claimService;
	@Autowired
	ReturnPurchaseOrderService returnPurchaseOrderService;
	@Autowired
	ManageComplaintService manageComplaintService;
	@Autowired
	QualityNotificationService qualityNotificationService;
	@Autowired
	QualityNotificationDao qualityNotificationDao;

	@Autowired
	CommonFunctions commonFunctions;

	@Autowired
	EightDService eightDService;
	@Autowired
	ComplaintService complaintService;

	@Autowired
	BusinessObjectService businessObjectService;

    private static final Logger logger = LoggerFactory.getLogger(ManageQualityNotificationServiceImpl.class);
    private static final String MANAGE_QUALITY_NOTIF_SERVICE = "ManageQualityNotificationServiceImpl";
    private static final String MAP_QN_STATUS = "mapQNStatus";
    private static final String UPDATE_QN = "updateQualityNotification";
    
    /**
     * Fetch IDs for all Master data based on the codes
     */
    @Override
    public MasterData getMasterDataIdsBasedOnDetails(QualityNotifications manageQualityNotification) {
        return configService.getMasterDataDetailsByCodes(manageQualityNotification.getMaterialCode(),
        manageQualityNotification.getPlantCode(), manageQualityNotification.getSupplierCode(),
        manageQualityNotification.getPersonResponsibleCode(),manageQualityNotification.getPurchaseOrganisationCode());
    }
    
    /**
     * Derivation of Status logic for Quality Notifications
     */
    @Override
    public List<BusinessObjectStatuses> mapQNStatus(QualityNotifications manageQualityNotification,
            List<BusinessObjectStatuses> statusListFromBackend) {
    	LoggerHelper.logMethodEntry(logger, MANAGE_QUALITY_NOTIF_SERVICE, MAP_QN_STATUS);
        List<QualityNotificationStatusMappings> allQualityNotificationStatusMappings = businessObjectDao
                .getAllQualityNotificationStatusMappings();
        List<BusinessObjectStatuses> boStatuses = new ArrayList<>();

        statusListFromBackend.stream().forEach(qnStatus -> {
            logger.info("Backend status code from event mesh {} ", qnStatus.getBusinessObjectStatusCode());
            QualityNotificationStatusMappings qnStatusMappings = allQualityNotificationStatusMappings.stream()
                    .filter(qnStatusMapping -> qnStatus.getBusinessObjectStatusCode()
                            .equalsIgnoreCase(qnStatusMapping.getCode()))
                    .findAny().orElse(null);
            if (null != qnStatusMappings) {
                logger.info("Backend status code in mapping : {} ", qnStatusMappings.getCode());
                logger.info("CMH status code in mapping : {} ", qnStatusMappings.getStatusCode());

                BusinessObjectStatuses boStatus = Struct.create(BusinessObjectStatuses.class);
                // for create scenario, id will be null. so no need to check in BO status
                // directly set the backend status value
                if (null != manageQualityNotification.getId()) {
                     setBusinessObjectStatusForQNUpdate(manageQualityNotification, boStatuses,
                    qnStatus,qnStatusMappings);
                } else {
                    logger.info("Status for Creation of automatic QN ");
                    setBusinessObjectStatuses(boStatus, qnStatus.getBusinessObjectStatusCode());
                    // set the current status code
                    manageQualityNotification.setStatusCode(boStatus.getBusinessObjectStatusCode());
                    boStatuses.add(boStatus);
                }

            }
        });
        logger.info("Status List Size is : {} ", boStatuses.size());
        LoggerHelper.logMethodExit(logger, MANAGE_QUALITY_NOTIF_SERVICE, MAP_QN_STATUS);
        return boStatuses;
    }
    /**
     * Find existing Business object status and update the status
     * @param manageQualityNotifications
     * @param boStatuses
     * @param qnStatus
     * @param qnStatusMappings
     */
    public void setBusinessObjectStatusForQNUpdate(QualityNotifications manageQualityNotifications,
			List<BusinessObjectStatuses> boStatuses, BusinessObjectStatuses qnStatus,
			QualityNotificationStatusMappings qnStatusMappings) {
    	LoggerHelper.logMethodEntry(logger, MANAGE_QUALITY_NOTIF_SERVICE, MAP_QN_STATUS);
		Result result = businessObjectDao.findBOStatusCode(manageQualityNotifications.getId(),
		        qnStatusMappings.getStatusCode());
            //check if the status already exists in the BO table
		List<BusinessObjectStatuses> boStatusList = result.first().isPresent()
				? result.listOf(BusinessObjectStatuses.class)
				: new ArrayList<>();
		logger.info("Size of Business Object status for QN: {} ", boStatuses.size());
		if(CollectionUtils.isEmpty(boStatusList)) {
		   List<BusinessObjectStatuses> existingBOStatusList = findExistingBusinessObjectStatusList(manageQualityNotifications.getId());
		   logger.info("Finding existing BO Status for QN ");
		   if(!CollectionUtils.isEmpty(existingBOStatusList)) {
		    logger.info("Size of Existing Business Object status for QN: {} ", existingBOStatusList.size());
		    setBusinessObjectStatuses(qnStatus,qnStatus.getBusinessObjectStatusCode());
		    qnStatus.setParent(manageQualityNotifications.getId());
		    //set the current status code
		    manageQualityNotifications.setStatusCode(qnStatus.getBusinessObjectStatusCode());
		    logger.info("Status Code for Update is : {} ",qnStatus.getBusinessObjectStatusCode());
		    boStatuses.add(qnStatus);
		    boStatuses.addAll(existingBOStatusList);
		   }
		 
		}else{
		    logger.info("Status already exists for update ");
		    boStatuses.addAll(boStatusList);
		}
	LoggerHelper.logMethodExit(logger, MANAGE_QUALITY_NOTIF_SERVICE, MAP_QN_STATUS);
	}
    /**
     * Find the Existing Business Object Codes for the QN ID
     * @param qualityNotificationId
     * @return
     */
    public List<BusinessObjectStatuses> findExistingBusinessObjectStatusList(String qualityNotificationId){
        Result result=businessObjectDao.findBOStatusBasedOnBOId(qualityNotificationId);
        return  result.first().isPresent()
                                ? result.listOf(BusinessObjectStatuses.class)
                                : new ArrayList<>();
        }
    /**
     * Set Business Object Status Code and Business Object Type Code
     * @param boStatus
     * @param backendStatusCode
     */
    public void setBusinessObjectStatuses(BusinessObjectStatuses boStatus, String backendStatusCode){
    	LoggerHelper.logMethodEntry(logger, MANAGE_QUALITY_NOTIF_SERVICE, MAP_QN_STATUS);
        String statusCode =getStatus(backendStatusCode);
        boStatus.setBusinessObjectStatusCode(statusCode);
        boStatus.setBusinessObjectType(Constants.QUALITYNOTIFICATION_CODE);
        LoggerHelper.logMethodExit(logger, MANAGE_QUALITY_NOTIF_SERVICE, MAP_QN_STATUS);
    }
    /**
     * Determine CMH Cockpit status based on the backend status
     * @param backendStatusCode
     * @return
     */
    public String getStatus(String backendStatusCode){
        return businessObjectService.determineQualityNotificationStatus(backendStatusCode);
      }
      
      /**
       * Update Quality Notification
       */
      @Override
      public QualityNotifications updateQualityNotification(QualityNotifications qualityNotifications) {
		LoggerHelper.logMethodEntry(logger, MANAGE_QUALITY_NOTIF_SERVICE, UPDATE_QN);
		QualityNotifications qualityNotificationsToBeUpdated = Struct.create(QualityNotifications.class);
		logger.info("updateQualityNotification :: {} ", qualityNotifications.getId());
		if (qualityNotifications.getId() != null) {
            logger.info("id not null");
			QualityNotifications qualityNotificationDetailsFromDB = getQualityNotificationDetailsFromActive(qualityNotifications.getId());
			Defects defectDetailsFromDB = getDefectBasedOnQNFromActive(qualityNotifications.getId());
            Claims claim = claimService.getClaim(qualityNotificationDetailsFromDB.getComplaintId());
			ReturnPurchaseOrders returnPurchaseOrder = returnPurchaseOrderService
					.getReturnPurchaseOrderDetailsBasedOnComplaintId(
							qualityNotificationDetailsFromDB.getComplaintId());
			Supplier8DProcesses supplier8D = eightDService
					.getEightDDetailsBasedOnComplaintId(qualityNotificationDetailsFromDB.getComplaintId());
			qualityNotifications.setComplaintId(qualityNotificationDetailsFromDB.getComplaintId());
			cds.gen.complaintservice.Complaints complaintDetails = complaintService.getComplaintCreationTypeAndCompanyCode(qualityNotificationDetailsFromDB.getComplaintId());
			logger.info("Complaint ID set from QN {}  ", qualityNotificationDetailsFromDB.getComplaintId());
			if (claim != null || returnPurchaseOrder != null || supplier8D != null) {
			    getMasterDataCodeByIds(qualityNotificationDetailsFromDB);
			    qualityNotificationsToBeUpdated.putAll(qualityNotificationDetailsFromDB);
				if (supplier8D == null && returnPurchaseOrder != null) {
					logger.info("Supplier8D null :: ");
					updatePurchaseOrderAndItem(qualityNotifications, qualityNotificationsToBeUpdated);
				} else if (returnPurchaseOrder == null && supplier8D != null) {
					logger.info("Supplier8D not null & RPO NUll :: ");
					updatePurchaseOrganisation(qualityNotifications, qualityNotificationsToBeUpdated,complaintDetails);
				} else if (supplier8D == null && returnPurchaseOrder == null) {
					logger.info("Supplier8D  null & RPO NUll :: ");
					updatePurchaseOrganisation(qualityNotifications, qualityNotificationsToBeUpdated,complaintDetails);
					updatePurchaseOrderAndItem(qualityNotifications, qualityNotificationsToBeUpdated);
				}else {
                    //(supplier8D != null && returnPurchaseOrder != null) 
				}
			} else {
                logger.info("all BOs are null ");
				updateQNWhenAllBOsAreNull(qualityNotifications, qualityNotificationsToBeUpdated,complaintDetails);
			}
			// Defect
            qualityNotificationsToBeUpdated.setDefect(defectDetailsFromDB);
            // Complaint update
            manageComplaintService.updateMasterDataForComplaints(qualityNotificationsToBeUpdated);
		}
		LoggerHelper.logMethodExit(logger, MANAGE_QUALITY_NOTIF_SERVICE,
				UPDATE_QN);
		return qualityNotificationsToBeUpdated;
	}
    
      /**
       * Update all master data since only Quality Notification is created for the complaint
       * @param qualityNotifications
       * @param qualityNotificationsToBeUpdated
       * @param
       **/
	public void updateQNWhenAllBOsAreNull(QualityNotifications qualityNotifications,
			QualityNotifications qualityNotificationsToBeUpdated,
			cds.gen.complaintservice.Complaints complaintDetails) {
		LoggerHelper.logMethodEntry(logger, MANAGE_QUALITY_NOTIF_SERVICE, UPDATE_QN);
		// MasterData conversion
		MasterData masterDataDetailsByCodes = getMasterDataIdsBasedOnDetails(qualityNotifications);     
		if(null!=masterDataDetailsByCodes){
            qualityNotifications.setMaterialId(null!=masterDataDetailsByCodes.getMaterial() ? 
            masterDataDetailsByCodes.getMaterial().getId() : null);
		 
		 if(masterDataDetailsByCodes.getPlants()!=null) {
		    qualityNotifications.setPlantId(masterDataDetailsByCodes.getPlants().getId());
		 }
		 if(masterDataDetailsByCodes.getSupplier()!=null) {
		    qualityNotifications.setSupplierId(masterDataDetailsByCodes.getSupplier().getId());
		 }
		 if(masterDataDetailsByCodes.getPersonResponsible()!=null) {
		    qualityNotifications.setPersonResponsibleId(masterDataDetailsByCodes.getPersonResponsible().getId());
		 }
		 updatePurchaseOrgWhenBOsAreNull(qualityNotifications, complaintDetails, masterDataDetailsByCodes);
		

		if (qualityNotifications.getUnit() == null && qualityNotifications.getMaterialId() != null
		&& masterDataDetailsByCodes.getMaterial().getBaseUnitOfMeasureCode() != null) {
		    qualityNotifications.setUnit(masterDataDetailsByCodes.getMaterial().getBaseUnitOfMeasureCode());
		}
       }
		   
		
		if(null!=complaintDetails) {
		    qualityNotifications.setCompanyId(complaintDetails.getCompanyCodeId());
		    logger.info("Company id :: {} ", qualityNotifications.getCompanyId());
		}
		qualityNotificationsToBeUpdated.putAll(qualityNotifications);
		LoggerHelper.logMethodExit(logger, MANAGE_QUALITY_NOTIF_SERVICE, UPDATE_QN);
	}

	public void updatePurchaseOrgWhenBOsAreNull(QualityNotifications qualityNotifications,
			cds.gen.complaintservice.Complaints complaintDetails, MasterData masterDataDetailsByCodes) {
		if(null!=masterDataDetailsByCodes.getPurchaseOrg()) {
			 qualityNotifications.setPurchasingOrganizationId(masterDataDetailsByCodes.getPurchaseOrg().getId());
		 }else if(null==masterDataDetailsByCodes.getPurchaseOrg()&&
				 null!=complaintDetails&&null!=complaintDetails.getCreationType()&&
				 complaintDetails.getCreationType().equalsIgnoreCase(Constants.COMPLAINT_MANUAL)) {
			 qualityNotifications.setPurchasingOrganizationId(complaintDetails.getPurchasingOrganizationId()); 
		 }else {
			 qualityNotifications.setPurchasingOrganizationId(null);
		 }
	}
    
	/**
	 * Fetch Master Data Codes based on their IDs
	 * @param qualityNotificationDetailsFromDB
	 */
	public void getMasterDataCodeByIds(QualityNotifications qualityNotificationDetailsFromDB) {
		LoggerHelper.logMethodEntry(logger, MANAGE_QUALITY_NOTIF_SERVICE, UPDATE_QN);
		MasterData masterDataDetailsCodes = configService.getMasterDataDetails(qualityNotificationDetailsFromDB.getMaterialId(), 
				qualityNotificationDetailsFromDB.getPlantId(), qualityNotificationDetailsFromDB.getSupplierId(),
				qualityNotificationDetailsFromDB.getPurchasingOrganizationId());
		qualityNotificationDetailsFromDB.setMaterialCode(null != masterDataDetailsCodes.getMaterial() ? masterDataDetailsCodes.getMaterial().getMaterialCode() : "");
		qualityNotificationDetailsFromDB.setPlantCode(null!=masterDataDetailsCodes.getPlants() ? masterDataDetailsCodes.getPlants().getPlant() : "");
		qualityNotificationDetailsFromDB.setSupplierCode(null!=masterDataDetailsCodes.getSupplier() ? masterDataDetailsCodes.getSupplier().getBusinessPartnerNumber() : "" );
		qualityNotificationDetailsFromDB.setPurchaseOrganisationCode(null!=masterDataDetailsCodes.getPurchaseOrg()
				? masterDataDetailsCodes.getPurchaseOrg().getPurchaseOrganization() : "");
		LoggerHelper.logMethodExit(logger, MANAGE_QUALITY_NOTIF_SERVICE, UPDATE_QN);
	}
    
	/**
	 * Update Purchase Order Number and Item Number 
	 * when Supplier 8D is not yet created
	 * @param qualityNotifications
	 * @param qualityNotificationsToBeUpdated
	 */
	private void updatePurchaseOrderAndItem(QualityNotifications qualityNotifications,
			QualityNotifications qualityNotificationsToBeUpdated) {
		LoggerHelper.logMethodEntry(logger, MANAGE_QUALITY_NOTIF_SERVICE, UPDATE_QN);
		qualityNotificationsToBeUpdated
				.setPurchaseOrderItem(qualityNotifications.getPurchaseOrderItem());
		qualityNotificationsToBeUpdated
				.setPurchaseOrderNumber(qualityNotifications.getPurchaseOrderNumber());
		LoggerHelper.logMethodExit(logger, MANAGE_QUALITY_NOTIF_SERVICE, UPDATE_QN);
	}
    
	/**
	 * Update Purchase Organisation when Return Purchase Order 
	 * is not yet created
	 * @param qualityNotifications
	 * @param qualityNotificationsToBeUpdated
	 */
	private void updatePurchaseOrganisation(QualityNotifications qualityNotifications,
			QualityNotifications qualityNotificationsToBeUpdated,cds.gen.complaintservice.Complaints complaintDetails) {
		LoggerHelper.logMethodEntry(logger, MANAGE_QUALITY_NOTIF_SERVICE, UPDATE_QN);
		logger.info("qualityNotifications.getPurchaseOrganisationCode() in update :: {} ", qualityNotifications.getPurchaseOrganisationCode());
		String purchOrgCode = "";
        String purchaseOrganizationId = null;
        if(StringUtils.isNotBlank(qualityNotifications.getPurchaseOrganisationCode())){
            PurchaseOrganizations purchaseOrganisation = configService.getPurchaseOrganisation(qualityNotifications.getPurchaseOrganisationCode());
            purchOrgCode = null!=purchaseOrganisation ? purchaseOrganisation.getPurchaseOrganization() : "";
            purchaseOrganizationId = null!=purchaseOrganisation ? purchaseOrganisation.getId(): null;
            
			qualityNotificationsToBeUpdated.setPurchasingOrganizationId(purchaseOrganizationId);
			qualityNotificationsToBeUpdated.setPurchaseOrganisationCode(purchOrgCode);
        }else if(qualityNotifications.getPurchaseOrganisationCode().equalsIgnoreCase("")
        		&& null!=complaintDetails && null!=complaintDetails.getCreationType() &&
        		complaintDetails.getCreationType().equalsIgnoreCase(Constants.COMPLAINT_MANUAL)){
            qualityNotificationsToBeUpdated.setPurchasingOrganizationId(complaintDetails.getPurchasingOrganizationId());
        }else {
        	qualityNotificationsToBeUpdated.setPurchasingOrganizationId(purchaseOrganizationId);
			qualityNotificationsToBeUpdated.setPurchaseOrganisationCode(purchOrgCode);
        }
        LoggerHelper.logMethodExit(logger, MAP_QN_STATUS, UPDATE_QN);
	}

    /**
     * Get details of Active Quality Notification based on ID
     * @param qnId
     * @return
     */
	public QualityNotifications getQualityNotificationDetailsFromActive(
			String qnId) {
		Result qualityNotificationDetails = qualityNotificationDao.getQualityNotificationDetailsFromActive(qnId);
		return qualityNotificationDetails.first().isPresent()
				? qualityNotificationDetails.listOf(QualityNotifications.class)
						.get(0)
				: null;
	}
    
	/**
	 * Get details of Active Defect based on Quality Notification ID
	 * @param qnId
	 * @return
	 */
    private Defects getDefectBasedOnQNFromActive(String qnId){
        Result defectDetails = qualityNotificationDao.getDefectBasedOnQNFromActive(qnId);
		return defectDetails.first().isPresent()
				? defectDetails.listOf(Defects.class)
						.get(0)
				: null;
    }
    
    /**
     * Check If Active Quality Notification exists based on QN Number
     */
	@Override
	public String checkIfActiveQNExistsBasedOnNumber(String qualityNotificationNumber) {
		String qnNumber = "";
		if(StringUtils.isNotBlank(qualityNotificationNumber)) {
			Result checkIfActiveQNExistsBasedOnNumberResult = qualityNotificationDao.checkIfActiveQNExistsBasedOnNumber(qualityNotificationNumber);
			qnNumber =  checkIfActiveQNExistsBasedOnNumberResult.first().isPresent()
					? checkIfActiveQNExistsBasedOnNumberResult.list().get(0).get(QualityNotifications.ID).toString()
					: "";
		}
		return qnNumber;
		
	}

    
}
