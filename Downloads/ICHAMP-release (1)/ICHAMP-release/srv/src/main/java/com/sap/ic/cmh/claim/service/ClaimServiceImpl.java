package com.sap.ic.cmh.claim.service;

import java.io.IOException;
import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Random;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Message.Severity;
import com.sap.cds.services.messages.Messages;
import com.sap.cloud.sdk.cloudplatform.connectivity.ScpCfDestinationLoader;
import com.sap.ic.cmh.businessobjects.service.BusinessObjectService;
import com.sap.ic.cmh.claim.model.ClaimDTO;
import com.sap.ic.cmh.claim.model.ClaimItem;
import com.sap.ic.cmh.claim.model.ClaimPricing;
import com.sap.ic.cmh.claim.model.ResponseModel;
import com.sap.ic.cmh.claim.model.binary_relation.BinaryRelationDataModel;
import com.sap.ic.cmh.claim.persistency.ClaimDao;
import com.sap.ic.cmh.claim.validations.ClaimValidation;
import com.sap.ic.cmh.configuration.model.MasterData;
import com.sap.ic.cmh.configuration.persistency.BusinessObjectConfigurationDao;
import com.sap.ic.cmh.configuration.persistency.ConditionTypeDao;
import com.sap.ic.cmh.configuration.persistency.DestinationConfigurationDao;
import com.sap.ic.cmh.configuration.persistency.ServiceMaterialDao;
import com.sap.ic.cmh.configuration.service.ConfigurationService;
import com.sap.ic.cmh.costcollector.service.CostCollectorService;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.masterdata.unitofmeasure.service.UnitOfMeasureService;
import com.sap.ic.cmh.network.service.HttpService;
import com.sap.ic.cmh.qualitynotification.service.QualityNotificationService;
import com.sap.ic.cmh.utils.CommonFunctions;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import cds.gen.claimservice.Claims;
import cds.gen.configurationservice.BusinessObjectConfigurations;
import cds.gen.configurationservice.ConditionTypes;
import cds.gen.configurationservice.ServiceMaterials;
import cds.gen.costcollectorservice.CostCollectors;
import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.masterdataservice.UnitOfMeasures;
import cds.gen.qualitynotificationservice.QualityNotifications;

@Service
public class ClaimServiceImpl implements ClaimService {

    @Autowired
    ClaimDao claimDao;

    @Autowired
    ClaimValidation claimValidator;

    @Autowired
    ConfigurationService configurationService;

    @Autowired
    CommonFunctions commonFunctions;

    @Autowired
    CostCollectorService costCollectorService;

    @Autowired
    DestinationConfigurationDao destinationConfigDao;

    @Autowired
    BusinessObjectConfigurationDao businessObjectConfigurationDao;

    @Autowired
    QualityNotificationService qualityNotificationService;

    @Autowired
    HttpService httpService;

    @Autowired
    ServiceMaterialDao serviceMaterialDao;

    @Autowired
    ConditionTypeDao conditionTypeDao;

    @Autowired
    BusinessObjectService businessObjectService;

    @Autowired
    Messages messages;

    @Autowired
    UnitOfMeasureService unitOfMeasureService;

    private Random random = new SecureRandom();
    private static final Logger logger = LoggerFactory.getLogger(ClaimServiceImpl.class);
    private static final String CLAIM_SERVICE_IMPL = "ClaimServiceImpl";

    /**
     * Get Claim based on Complaint ID
     * 
     * @param complaintID
     * @return Claim
     */
    @Override
    public Claims getClaim(String complaintID) {
        Result result = claimDao.getClaim(complaintID);
        return (result != null && result.first().isPresent()) ? result.listOf(Claims.class).get(0) : null;
    }

    /**
     * Get Claim based on ID
     * 
     * @param claimID
     * @return Claim
     */
    @Override
    public Claims getClaimBasedOnId(String claimID) {
        LoggerHelper.logMethodEntry(logger, CLAIM_SERVICE_IMPL, "getClaimBasedOnId");
        logger.info("Get Claim Based on ID");
        Result result = claimDao.getClaimBasedOnId(claimID);
        if (result != null && result.first().isPresent()) {
            return result.listOf(Claims.class).get(0);
        }
        LoggerHelper.logMethodExit(logger, CLAIM_SERVICE_IMPL, "getClaimBasedOnId");
        return null;
    }

    /**
     * Create claim at the configured destination
     */
    @Override
    public String createClaimAtDestination(Claims claim) {
        LoggerHelper.logMethodEntry(logger, CLAIM_SERVICE_IMPL, "createClaimAtDestination");
        String claimNumber = "";
        String destination = "";
        String endPoint = Constants.CLAIM_APPENDED_URL;
        ResponseModel responseModel = null;
        logger.info("Inside Create of Claim method");
        destination = fetchConfiguredDestination(claim, destination);
        String complaintId = claim.getComplaintId();
        QualityNotifications qualityNotification = qualityNotificationService
                .getQualityNotificationDetailsByComplaintId(complaintId);
        ClaimDTO claimRequest = createClaimDTO(claim, destination);
        Result destinationConfigs = destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(
                claim.getCompanyId(), Constants.QUALITYNOTIFICATION_CODE);
        Optional<Row> destinationConfigsFirst = destinationConfigs.first();
        if (destinationConfigsFirst.isPresent()) {
            String sourceDestination = destinationConfigsFirst.get().get(Constants.DESTINATION).toString();
            setRequestForDocFlow(sourceDestination, qualityNotification, claimRequest, destination);
        }

        Map<String, Object> claimRequestMap = commonFunctions.convertObjectToMap(claimRequest);
        try {
            responseModel = httpService.callCpiFlow(endPoint, claimRequestMap, destination);
            if (null != responseModel) {
                claimNumber = String.format("%012d",
                        Integer.parseInt(responseModel.getResult()));
                claim.setStatusCode(null != responseModel.getStatusCode() ? responseModel.getStatusCode() : "");
                businessObjectService.insertBusinessObjectRelations(
                        Constants.QUALITYNOTIFICATION_CODE, qualityNotification.getId(),
                        Constants.CLAIM_CODE, claim.getId());
            }
            messages.throwIfError();
        } catch (IOException e) {
            messages.error(MessageKeys.ERROR_IN_CPI_FLOW.concat(e.toString()));
            messages.throwIfError();
        }
        LoggerHelper.logMethodExit(logger, CLAIM_SERVICE_IMPL, "createClaimAtDestination");
        return claimNumber;
    }

    /**
     * Set request for document flow for Claim and QN
     */
    public void setRequestForDocFlow(String sourceDestination, QualityNotifications qualityNotification,
            ClaimDTO claimRequest, String targetDestination) {
        LoggerHelper.logMethodEntry(logger, CLAIM_SERVICE_IMPL, "setRequestForDocFlow");
        if (null != qualityNotification) {
            logger.info("Source destination QN : {} ", sourceDestination);
            logger.info("Target destination QN : {} ", targetDestination);

            ScpCfDestinationLoader scpCfDestinationLoader = new ScpCfDestinationLoader();
            String qualityNotificationNumber = String.format("%012d",
                    Integer.parseInt(qualityNotification.getIdentifier()));
            String rfcDestination = httpService.getLogicalSystem(scpCfDestinationLoader, sourceDestination);
            String sourceLogicalSystem = httpService.getTargetLogicalSystem(scpCfDestinationLoader, rfcDestination);
            String targetLogicalSystem = httpService.getTargetLogicalSystem(scpCfDestinationLoader, targetDestination);
            BinaryRelationDataModel binaryRelationDataModel = commonFunctions.setBinaryRelationDataModel(
                    qualityNotificationNumber, Constants.QUALITY_NOTIFICATION_OBJECT_TYPE, Constants.CLAIM_OBJECT_TYPE,
                    sourceLogicalSystem, targetLogicalSystem);
            claimRequest.setBinaryRelationDataModel(binaryRelationDataModel);
        }
        LoggerHelper.logMethodExit(logger, CLAIM_SERVICE_IMPL, "setRequestForDocFlow");
    }

    /**
     * Set the request for create claim to pass to CPI flow
     * 
     * @param claim
     * @return
     */
    public ClaimDTO createClaimDTO(Claims claim, String destination) {
        LoggerHelper.logMethodEntry(logger, CLAIM_SERVICE_IMPL, "createClaimDTO");
        ClaimDTO claimDTO = new ClaimDTO();
        String personResponsible = "";
        List<String> attributesList = new ArrayList<>();
              MasterData masterDataDetails = configurationService.getMasterDataDetails(claim.getMaterialId(),
                claim.getPlantId(),
                claim.getSupplierId(), claim.getPurchasingOrganizationId());
        Map<String,Object> claimRequestMap = commonFunctions.convertObjectToMap(claim);
        claimRequestMap.replace(Claims.PLANT_ID, claim.getPlantId(), masterDataDetails.getPlants().getPlant());
        claimRequestMap.replace(Claims.SUPPLIER_ID, claim.getSupplierId(), masterDataDetails.getSupplier().getBusinessPartnerNumber());
        if (StringUtils.isNotBlank(claim.getPersonResponsibleId())) {
            BusinessPartners supplier = configurationService.getSupplier(claim.getPersonResponsibleId());
            personResponsible = supplier.getBusinessPartnerNumber();
        }
        claimRequestMap.replace(Claims.PERSON_RESPONSIBLE_ID, claim.getPersonResponsibleId(),
				StringUtils.isNotBlank(personResponsible)? personResponsible : "");
        attributesList.add(Claims.ID);
        attributesList.add(Claims.REQUESTED_AMOUNT);
        attributesList.add(Claims.PAID_AMOUNT);
        attributesList.add(Claims.DECISION);
        attributesList.add(Claims.CONTACT_PERSON_ID);
        attributesList.add(Claims.REQUEST_SENT_DATE);
        attributesList.add(Claims.RESPONSE_RECEIVED_DATE);
        attributesList.add(Claims.STATUS_CODE);
        attributesList.add(Claims.COMPANY_ID);
        attributesList.add(Claims.COMPLAINT_ID);
        attributesList.add(Claims.PURCHASING_ORGANIZATION_ID);
        attributesList.add(Claims.MATERIAL_ID);
        attributesList.add(Claims.BUSINESS_OBJECT_STATUSES);
        attributesList.add(Claims.IS_CLAIM_FIELD_CONTROL);
        attributesList.add(Claims.IS_UPDATE_RESTRICTED);
        attributesList.add(Claims.S_NAVIGATION);
        commonFunctions.removeAttributes(attributesList, claimRequestMap);     
        logger.info("claimRequestMap :: {} ",claimRequestMap);
        

        List<ClaimItem> claimItemList = new ArrayList<>();
        List<ClaimPricing> claimPricingList = new ArrayList<>();
        ClaimItem costfromClaim = new ClaimItem();
        String handle = autoNumberGeneration();
        costfromClaim.setItemType(claim.getItemTypeCode());
        costfromClaim.setHandle(handle);
        costfromClaim.setQuantity(null != claim.getQuantity() ? claim.getQuantity().toString() : "");
        costfromClaim.setMaterial(masterDataDetails.getMaterial().getMaterialCode());
        if (StringUtils.isNotBlank(claim.getUnit())) {
            getIsoCodeUnit(costfromClaim, claim.getUnit());
        } else {
            costfromClaim.setUnit("");
        }
        costfromClaim.setItemKey(masterDataDetails.getMaterial().getMaterialCode());
        costfromClaim.setPartCauseDamage("X");
        setClaimItemPricingValues(claim, destination, claimItemList, claimPricingList);

        claimItemList.add(costfromClaim);
        claimDTO.setClaims(claimRequestMap);
        claimDTO.setClaimItem(claimItemList);
        claimDTO.setClaimPricing(claimPricingList);
        LoggerHelper.logMethodExit(logger, CLAIM_SERVICE_IMPL, "createClaimDTO");
        return claimDTO;
    }

    /**
     * Set claim item and claim pricing values
     * 
     * @param claim
     * @param destination
     * @param claimItemList
     * @param claimPricingList
     */
    public void setClaimItemPricingValues(Claims claim, String destination, List<ClaimItem> claimItemList,
            List<ClaimPricing> claimPricingList) {
        LoggerHelper.logMethodEntry(logger, CLAIM_SERVICE_IMPL, "setClaimItemPricingValues");
        List<CostCollectors> cost = costCollectorService.selectTransferToClaimCostCollector(claim.getComplaintId());
        if (cost != null) {
            for (CostCollectors costCollectors : cost) {
                ClaimItem claimItem = new ClaimItem();        
                claimItem.setSubItemType(costCollectors.getSubItemTypeCode());
                String handler = autoNumberGeneration();
                claimItem.setHandle(handler);
                claimItem.setItemType(costCollectors.getItemTypeCode());
                claimItem.setQuantity(
                        null != costCollectors.getQuantity() ? costCollectors.getQuantity().toString() : "1");
                claimItem.setDescription(costCollectors.getDescription());
                logger.info("Service material details are retrieved from Destination");
                setServiceMaterialConfigDetails(destination, costCollectors, claimItem);
                getIsoCodeUnit(claimItem, costCollectors.getUnitCode());
                claimItemList.add(claimItem);

                ClaimPricing claimPrice = new ClaimPricing();
                if (costCollectors.getTotalCost() != null) {
                    claimPrice.setTotalCost(costCollectors.getTotalCost().toString());
                }
                claimPrice.setRefhandle(handler);
                claimPrice.setCurrency(costCollectors.getCurrencyCode());
                Result conditionTypeConfig = conditionTypeDao.getConditionTypesBasedOnDestinationAndItemType(
                        destination,
                        costCollectors.getItemTypeCode());
                Optional<Row> conditionTypeConfigFirst = conditionTypeConfig.first();
                if (conditionTypeConfigFirst.isPresent()) {
                    claimPrice.setCondType(
                            conditionTypeConfigFirst.get().get(ConditionTypes.CONDITION_TYPE).toString());
                }
                claimPricingList.add(claimPrice);
            }

        }
        LoggerHelper.logMethodExit(logger, CLAIM_SERVICE_IMPL, "setClaimItemPricingValues");
    }

    /**
     * Fetch configured destination for Claim
     * 
     * @param claim
     * @param destination
     * @return
     */
    public String fetchConfiguredDestination(Claims claim, String destination) {
        LoggerHelper.logMethodEntry(logger, CLAIM_SERVICE_IMPL, "fetchConfiguredDestination");
        Result destinationConfigs = destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(
                claim.getCompanyId(), Constants.CLAIM_CODE);
        if (destinationConfigs != null && destinationConfigs.first().isPresent()) {
            destination = destinationConfigs.list().get(0).get(Constants.DESTINATION).toString();
        }
        LoggerHelper.logMethodExit(logger, CLAIM_SERVICE_IMPL, "fetchConfiguredDestination");
        return destination;
    }

    /**
     * Read from service material configuration and set for claim item
     * 
     * @param destination
     * @param costCollectors
     * @param claimItem
     */
    public void setServiceMaterialConfigDetails(String destination, CostCollectors costCollectors,
            ClaimItem claimItem) {
        LoggerHelper.logMethodEntry(logger, CLAIM_SERVICE_IMPL, "setServiceMaterialConfigDetails");
        Result serviceMaterialConfig = serviceMaterialDao
                .getServiceMaterialsBasedOnDestinationSubItemTypeAndItemType(destination,
                        costCollectors.getSubItemTypeCode(),
                        costCollectors.getItemTypeCode());

        Optional<Row> serviceMaterialConfigFirst = serviceMaterialConfig.first();
        if (serviceMaterialConfigFirst.isPresent()) {
            serviceMaterialConfig.first();
            String sMaterial = serviceMaterialConfigFirst.get().get(ServiceMaterials.SERVICE_MATERIAL).toString();
            logger.info("sMaterial is received.");
            claimItem.setItemKey(sMaterial);
            claimItem.setMaterial(sMaterial);
        }
        LoggerHelper.logMethodExit(logger, CLAIM_SERVICE_IMPL, "setServiceMaterialConfigDetails");
    }

    /**
     * Get the ISO code for Unit
     * 
     * @param unit
     * @return
     */
    public void getIsoCodeUnit(ClaimItem claimItem, String unit) {
        LoggerHelper.logMethodEntry(logger, CLAIM_SERVICE_IMPL, "getIsoCodeUnit");
        if (null != unit) {
        	UnitOfMeasures unitOfMeasureDetails = unitOfMeasureService.getUnitOfMeasureDetails(unit);
    		String isoCode = null!=unitOfMeasureDetails ? unitOfMeasureDetails.getISOCode() : "";
            logger.info("ISO code for unit is received");
            claimItem.setUnit(isoCode);
        } else {
            claimItem.setUnit("");
        }
        LoggerHelper.logMethodExit(logger, CLAIM_SERVICE_IMPL, "getIsoCodeUnit");
    }

    /**
     * Get the values from configuration and set it to the Claim object
     */
    @Override
    public void setConfiguredValues(Claims claim, String boType, String complaintTypeCode) {
        LoggerHelper.logMethodEntry(logger, CLAIM_SERVICE_IMPL, "setConfiguredValues");
        String destination = "";
        destination = fetchConfiguredDestination(claim, destination);
        // get the configured business object attributes based on complaint type
        // code,destination and business object type
        Result businessObjectConfigurationsResult = businessObjectConfigurationDao
                .getBusinessObjectConfigBasedOnDestinationAndBOAndDest(complaintTypeCode, boType,
                        destination);
        if (businessObjectConfigurationsResult != null && businessObjectConfigurationsResult.first().isPresent()) {
            List<BusinessObjectConfigurations> businessObjectConfigurations = businessObjectConfigurationsResult
                    .listOf(BusinessObjectConfigurations.class);
            for (int i = 0; i < businessObjectConfigurations.size(); i++) {
                if (businessObjectConfigurations.get(i).getBusinessObjectAttributeCode()
                        .equalsIgnoreCase(Claims.CLAIM_TYPE)) {
                    claim.setClaimType(
                            businessObjectConfigurations.get(i).getBusinessObjectValue());
                } else if (businessObjectConfigurations.get(i).getBusinessObjectAttributeCode()
                        .equalsIgnoreCase(Claims.SUPPLIER_ROLE)) {
                    claim.setSupplierRole(
                            businessObjectConfigurations.get(i).getBusinessObjectValue());
                } else if (businessObjectConfigurations.get(i).getBusinessObjectAttributeCode()
                        .equalsIgnoreCase(Claims.VERSION_CATEGORY)) {
                    claim.setVersionCategory(
                            businessObjectConfigurations.get(i).getBusinessObjectValue());
                } else if (businessObjectConfigurations.get(i).getBusinessObjectAttributeCode()
                        .equalsIgnoreCase(Claims.ITEM_TYPE)) {
                    claim.setItemTypeCode(
                            businessObjectConfigurations.get(i).getBusinessObjectValue());
                }
            }
        }
        LoggerHelper.logMethodExit(logger, CLAIM_SERVICE_IMPL, "setConfiguredValues");
    }

    /**
     * Random number generation for handle
     * 
     * @return
     */
    String autoNumberGeneration() {
        return String.valueOf(random.nextInt(900) + 100);
    }

    /**
     * Validate mandatory attributes for claim and
     * validate field control
     */
    @Override
    public void validateClaimFields(Claims claim) {
        LoggerHelper.logMethodEntry(logger, CLAIM_SERVICE_IMPL, "validateClaimFields");
        claimValidator.validateClaimFields(claim);
        if (messages.stream().noneMatch(message -> message.getSeverity() == Severity.ERROR)) {
            claimValidator.validateFieldControlClaim(claim);
        }
        LoggerHelper.logMethodExit(logger, CLAIM_SERVICE_IMPL, "validateClaimFields");
    }

    /**
     * Check if Claim is created for the complaint and check if
     * Claim is relevant
     */
    @Override
    public void validateIfClaimExistsForComplaint(String complaintId) {
        LoggerHelper.logMethodEntry(logger, CLAIM_SERVICE_IMPL, "validateIfClaimExistsForComplaint");
        claimValidator.validateIfClaimExistsForComplaint(complaintId);
        if (messages.stream().noneMatch(message -> message.getSeverity() == Message.Severity.ERROR)) {
            claimValidator.validateIfQualityNotificationExists(complaintId, Constants.CLAIM_CODE);
        }
        if (messages.stream().noneMatch(message -> message.getSeverity() == Message.Severity.ERROR)) {
            claimValidator.validateIfBOIsRelevant(complaintId, Constants.CLAIM_CODE);
        }
        LoggerHelper.logMethodExit(logger, CLAIM_SERVICE_IMPL, "validateIfClaimExistsForComplaint");
    }

    /**
     * Check if claim exists and validate field editability
     */
    @Override
    public void validateIfClaimExists(Claims claim) {
        LoggerHelper.logMethodEntry(logger, CLAIM_SERVICE_IMPL, "validateIfClaimExists");
        claimValidator.validateIfClaimExists(claim.getId());
        if (messages.stream().noneMatch(message -> message.getSeverity() == Severity.ERROR)) {
            claimValidator.validateFieldControlClaim(claim);
        }
        LoggerHelper.logMethodExit(logger, CLAIM_SERVICE_IMPL, "validateIfClaimExists");
    }

    @Override
    public Claims getClaimStatusAndCompanyCode(String claimId) {
        claimDao.getClaimStatusAndCompanyCode(claimId);
        return null;
    }

    @Override
    public Claims getDraftClaimByComplaintID(String complaintId) {
        Result result = claimDao.getDraftClaimByComplaintID(complaintId);
        return (result != null && result.first().isPresent()) ? result.listOf(Claims.class).get(0) : null;
    }

    @Override
    public void deleteDraftClaimByID(String claimId) {
        claimDao.deleteDraftClaimByID(claimId);
    }

	@Override
	public cds.gen.manageclaimservice.Claims getActiveClaimBasedOnId(String claimId) {
		Result result = claimDao.getActiveClaimBasedOnId(claimId);
        return (result != null && result.first().isPresent()) ? result.listOf(cds.gen.manageclaimservice.Claims.class).get(0) : null;
	}

     /**
     * Check if Claim exists in DB based on number
     */
    @Override
	public String checkIfClaimExistsBasedOnNumber(String claimNumber) {
		Result result = claimDao.checkIfClaimExistsBasedOnNumber(claimNumber);
		
		return result.first().isPresent()?result.list().get(0).get(Claims.ID).toString() : "";
	}

}
