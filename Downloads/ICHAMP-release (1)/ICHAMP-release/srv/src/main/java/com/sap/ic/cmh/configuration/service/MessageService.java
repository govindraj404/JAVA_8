package com.sap.ic.cmh.configuration.service;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.springframework.util.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.Insert;
import com.sap.cds.ql.Update;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.messages.Messages;
import com.sap.cloud.sdk.datamodel.odata.client.exception.ODataException;
import com.sap.ic.cmh.configuration.persistency.DestinationConfigurationDao;
import com.sap.ic.cmh.claim.service.ClaimService;
import com.sap.ic.cmh.configuration.persistency.ConfigurationDao;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.managecomplaint.service.ManageComplaintService;
import com.sap.ic.cmh.qualitynotification.service.QualityNotificationService;
import com.sap.ic.cmh.masterdata.common.service.MasterDataService;
import com.sap.ic.cmh.returnpo.service.ReturnPurchaseOrderService;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import cds.gen.masterdataservice.Plants;
import cds.gen.masterdataservice.Plants_;
import cds.gen.manageclaimservice.Claims;
import cds.gen.manageclaimservice.Claims_;
import cds.gen.managecomplaintservice.Complaints;
import cds.gen.managequalitynotificationservice.Defects;
import cds.gen.managequalitynotificationservice.QualityNotifications;
import cds.gen.managequalitynotificationservice.QualityNotifications_;
import cds.gen.managereturnpurchaseorderservice.BusinessObjectStatuses;
import cds.gen.managereturnpurchaseorderservice.ReturnPurchaseOrders;
import cds.gen.managereturnpurchaseorderservice.ReturnPurchaseOrders_;

@Service
public class MessageService {

	@Autowired
	ManageComplaintService manageComplaintService;

	@Autowired
	QualityNotificationService qualityNotificationService;
	@Autowired
	ClaimService claimService;
	@Autowired
	ReturnPurchaseOrderService returnPurchaseOrderService;
	@Autowired
	MasterDataService masterDataService;
	@Autowired
    DestinationConfigurationDao destinationConfigDao;
    @Autowired
    ConfigurationDao configurationDao;
    @Autowired
    Messages messages;
    @Autowired
    @Qualifier("ManageClaimService")
    CdsService claimCdsService;
    @Autowired
    @Qualifier("ManageQualityNotificationService")
    CdsService qnCdsService;
    @Autowired
    @Qualifier("ManageReturnPurchaseOrderService")
    CdsService returnOrderCdsService;

	private static final Logger logger = LoggerFactory.getLogger(MessageService.class);
	private static final String MESSAGE_SERVICE = "MessageService";


	public Complaints createComplaintAndQN(QualityNotifications manageQualityNotifications) {
		LoggerHelper.logMethodEntry(logger, MESSAGE_SERVICE, "createComplaintAndQN");
        Complaints complaint;
		try {
			logger.info("[create complaint][MessageService] before calling createComplaint method");
			complaint = manageComplaintService.createComplaintAutomatic(manageQualityNotifications);
		} catch (ODataException e) {
			logger.error("Issue While Automatic creation of Complaint and QualityNotification");
			return null;
		}
		LoggerHelper.logMethodExit(logger, MESSAGE_SERVICE, "createComplaintAndQN");
		return complaint;
	}

	/**
	 * Check if Business Object exists in our DB
	 * 
	 * @param boNumber
	 * @param boType
	 * @return
	 */
	public String checkIfBOExists(String boNumber, String boType) {
		LoggerHelper.logMethodEntry(logger, MESSAGE_SERVICE, "checkIfBOExists");
		logger.info("Business Object type : {} ", boType);
        String businessObjectId = "";
		switch (boType) {

		case Constants.QUALITYNOTIFICATION_CODE:
			businessObjectId = qualityNotificationService.checkIfQNExistsBasedOnNumber(boNumber);
			break;
		case Constants.CLAIM_CODE:
			businessObjectId = claimService.checkIfClaimExistsBasedOnNumber(boNumber);
			break;
		case Constants.RETURNPO_CODE:
			businessObjectId = returnPurchaseOrderService.checkIfReturnPOExistsBasedOnNumber(boNumber);
			break;
		default:
			break;

		}
		LoggerHelper.logMethodExit(logger, MESSAGE_SERVICE, "checkIfBOExists");
		return businessObjectId;
	}

	/**
	 * Get the configured destination based on company code
	 * @param plant
	 * @return
	 */
	public String fetchConfiguredDestination(String plant) {
        String companyId = getCompanyCodeBasedOnPlant(plant);

        logger.info("Plant is {} ",plant);
        logger.info("company id is {} ",companyId);
		String destination ="";
		Result destinationConfigs =
		        destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(
		        		companyId, Constants.QUALITYNOTIFICATION_CODE);
		if (destinationConfigs!=null && destinationConfigs.first().isPresent()) {
			 destination = destinationConfigs.list().get(0).get(Constants.DESTINATION).toString();
        }
        logger.info("company id is {} ",companyId);
		return destination;
    }
    
        
    /**
	 * Get the company code based on plant
	 * @param plant
	 * @return
	 */
	private String getCompanyCodeBasedOnPlant(String plant) {
        String plantId=getPlantIdBasedOnCode(plant);
        messages.throwIfError();
        logger.info("plant id : {} ",plantId);
		return  StringUtils.isNotBlank(plantId) ? masterDataService.getCompanyCodeBasedOnPlants(plantId).getCompanyCodeIDId() : "";
		
    }
    /**
     * Get the plant id based on code
     */
	private String getPlantIdBasedOnCode(String plant) {
		String plantId = "";
		if(StringUtils.isNotBlank(plant)) {
			Result plantResult = configurationDao.getPlantDataBasedOnPlant(plant);
			Plants plantDataBasedOnPlant = plantResult.first().isPresent() ? plantResult.listOf(Plants.class).get(0) : null;
			if (null != plantDataBasedOnPlant) {
				plantId = plantDataBasedOnPlant.getId();
			} else {
				messages.error(MessageKeys.PLANT_DOES_NOT_EXIST).target("in", Plants_.class,
						Plants_::plant);
			}
		}else {
			messages.error(MessageKeys.PLANT_IS_MANDATORY).target("in", Plants_.class, Plants_::plant);
		}
		return plantId;
	}
	
	/**
	 * Form Odata Payload for respective Business Object type and perform create/update 
	 * @param requestPaylodMap
	 * @param boTypeCode
	 */
	public void formPayloadAndUpdateOdata(Map<String, Object> requestPaylodMap, String boTypeCode) {
		logger.info("Automatic Create/Update invoked for :: {} ",boTypeCode);
		switch(boTypeCode) {
		case Constants.QUALITYNOTIFICATION_CODE:
            QualityNotifications qualityNotifications = formQualityNotificationPayload(requestPaylodMap);
            if(null!=qualityNotifications){
                logger.info("QN is set");
                CqnInsert insert = Insert.into(QualityNotifications_.class).entry(qualityNotifications);
                qnCdsService.run(insert);
                logger.info("QN is created/updated");
            }
			break;
			
		case Constants.CLAIM_CODE:
			Claims claim = formClaimPayload(requestPaylodMap);
			if(null!=claim) {
				logger.info("Claim is set");
				CqnUpdate update = Update.entity(Claims_.class).data(claim);
				claimCdsService.run(update);
				logger.info("Claim is updated");
			}
			break;
			
		case Constants.RETURNPO_CODE: 
			ReturnPurchaseOrders returnOrders = formReturnPurchaseOrderPayload(requestPaylodMap);
			if(null!=returnOrders) {
				logger.info("RPO is set");
				CqnUpdate update = Update.entity(ReturnPurchaseOrders_.class).data(returnOrders);
				returnOrderCdsService.run(update);
				logger.info("RPO is updated");
			}
			break;
		default:
			break;
		}		
	}

    /**
     * Form Odata Payload for QualityNotifications
     * @param qnRequestPaylodMap
     * @return
     */
    public QualityNotifications formQualityNotificationPayload(Map<String, Object> qnRequestPaylodMap){
        QualityNotifications qualityNotifications = Struct.create(QualityNotifications.class);
        if(!CollectionUtils.isEmpty(qnRequestPaylodMap)) {
        	logger.info("qnRequestPaylodMap is not empty");
        qualityNotifications.setIdentifier((String)qnRequestPaylodMap.get("QMNUM"));
        qualityNotifications.setQnType((String)qnRequestPaylodMap.get("QMART"));
        qualityNotifications.setMaterialCode((String)qnRequestPaylodMap.get("MATNR"));
        qualityNotifications.setPlantCode((String)qnRequestPaylodMap.get("MAWERK"));
        qualityNotifications.setPurchaseOrganisationCode((String)qnRequestPaylodMap.get("EKORG"));
        if(null != qnRequestPaylodMap.get("MGFRD")){
         String quantity = String.valueOf(qnRequestPaylodMap.get("MGFRD"));
            qualityNotifications.setQuantity(new BigDecimal(quantity));
        }
        qualityNotifications.setUnit((String)qnRequestPaylodMap.get("MGEIN"));
        qualityNotifications.setSupplierCode((String)qnRequestPaylodMap.get("LIFNUM"));
        qualityNotifications.setPurchaseOrderNumber((String)qnRequestPaylodMap.get("EBELN"));
        qualityNotifications.setPurchaseOrderItem((String)qnRequestPaylodMap.get("EBELP"));
        qualityNotifications.setReferenceNumber((String)qnRequestPaylodMap.get("REFNUM"));

        List<Object> defectList=(List<Object>)qnRequestPaylodMap.get("QMFE");
        logger.info("defectList: {}",defectList);
        if(!CollectionUtils.isEmpty(defectList)) {
            setDefectDetailsFromPayload(defectList,qualityNotifications);  
          } 
        }
        return qualityNotifications;
    }
	
    /**
     * Form Defect structure for QualityNotifications
     * @param defectList
     * @param qualityNotifications
     */
    public void setDefectDetailsFromPayload(List<Object> defectList, QualityNotifications qualityNotifications) {
        Defects defects = Struct.create(Defects.class);
        Map<String, Object> defectMap=(Map<String, Object>) defectList.get(0);
        logger.info("defectMap: {}",defectMap);
        defects.setDefectGroupCode((String)defectMap.get("FEGRP"));
        defects.setDefectCodeCode((String)defectMap.get("FECOD"));
        defects.setIdentifier((String)defectMap.get("FENUM"));
        defects.setDescription((String)defectMap.get("FETXT"));
       
        qualityNotifications.setDefect(defects);
        List<Object> statusList=(List<Object>)defectMap.get("JEST");
        logger.info("statusList : {} ",statusList);      
        if (!CollectionUtils.isEmpty(statusList)) {     
            setSystemStatusFromPayload(statusList,qualityNotifications);
        } 
    }
    
    /**
     * Form BusinessObjectStatuses structure for QualityNotifications
     * @param statusList
     * @param qualityNotifications
     */
    public void setSystemStatusFromPayload(List<Object> statusList, QualityNotifications qualityNotifications) {
        List<cds.gen.managequalitynotificationservice.BusinessObjectStatuses> boStatusList=new ArrayList<>();
            for(int i=0;i<statusList.size();i++){
            Map<String, Object> statusMap=(Map<String, Object>) statusList.get(i);
            cds.gen.managequalitynotificationservice.BusinessObjectStatuses boStatuses=Struct.create(cds.gen.managequalitynotificationservice.BusinessObjectStatuses.class);
            boStatuses.setBusinessObjectStatusCode((String)statusMap.get("STAT"));
            boStatusList.add(boStatuses);
            List<Object> businessPartnerList=(List<Object>)statusMap.get("IHPA");
                if (!CollectionUtils.isEmpty(businessPartnerList)) {   
                setPersonResponsibleFromPayload(businessPartnerList,qualityNotifications);
                }
        } 
        qualityNotifications.setBusinessObjectStatuses(boStatusList);
    }

    /**
     * Set Person Responsible for QualityNotifications
     * @param businessPartnerList
     * @param qualityNotifications
     */
    public void setPersonResponsibleFromPayload(List<Object> businessPartnerList, QualityNotifications qualityNotifications) {
        Map<String, Object> businessPartnerMap = (Map<String, Object>) businessPartnerList.get(0);
		if (!CollectionUtils.isEmpty(businessPartnerMap)) {
			qualityNotifications.setPersonResponsibleCode((String) businessPartnerMap.get("ERNAM"));
		}
    }
    
	/**
	 * Form Odata payload for ReturnPurchaseOrders
	 * 
	 * @param requestPaylodMap
	 * @return
	 */
	public ReturnPurchaseOrders formReturnPurchaseOrderPayload(Map<String, Object> requestPaylodMap) {

		ReturnPurchaseOrders returnOrders = Struct.create(ReturnPurchaseOrders.class);
		if (!CollectionUtils.isEmpty(requestPaylodMap)) {
			String rpoNumber = (String) requestPaylodMap.get("rpoNumber");
			String returnPurchaseOrderId = checkIfBOExists(rpoNumber, Constants.RETURNPO_CODE);
			if (null != returnPurchaseOrderId) {
				returnOrders.setIdentifier(rpoNumber);
				returnOrders.setId(returnPurchaseOrderId);
				List<Object> rpoBackendStatusList = (List<Object>) requestPaylodMap.get("rpoBackendStatusList");
				List<BusinessObjectStatuses> boStatusList = new ArrayList<>();
				logger.info("rpoBackendStatusList size:: {} ", rpoBackendStatusList.size());
				for (int i = 0; i < rpoBackendStatusList.size(); i++) {
					logger.info("inside status");
					BusinessObjectStatuses boStatus = Struct.create(BusinessObjectStatuses.class);
					Map<String, Object> rpoBackendStatusMap = (Map<String, Object>) rpoBackendStatusList.get(i);
					String statusCode = (String) rpoBackendStatusMap.get("statusCode");
					logger.info("Status code:: {} ", statusCode);
					boStatus.setBusinessObjectStatusCode(statusCode);
					boStatusList.add(boStatus);
				}
				returnOrders.setBusinessObjectStatuses(boStatusList);
			}
		}
		return returnOrders;
	}
    
	/**
	 * Form Odata payload for Claims
	 * 
	 * @param claimMessageMap
	 * @return
	 */
	public Claims formClaimPayload(Map<String, Object> claimMessageMap) {
		Claims claim = Struct.create(Claims.class);
		if (!CollectionUtils.isEmpty(claimMessageMap)) {
			String claimNumber = (String) claimMessageMap.get("CLAIM_NUMBER");
			String claimStatus = (String) claimMessageMap.get("CLAIM_STATUS");
			String claimId = checkIfBOExists(claimNumber, Constants.CLAIM_CODE);
			logger.info("Claim ID :: {}  ", claimId);
			logger.info("claimStatus:: {}  ", claimStatus);
			if (StringUtils.isNotBlank(claimId) && StringUtils.isNotBlank(claimStatus)) {
				claim.setStatusCode(claimStatus);
				claim.setIdentifier(claimNumber);
				claim.setId(claimId);
			}
		}
		return claim;
	}
}