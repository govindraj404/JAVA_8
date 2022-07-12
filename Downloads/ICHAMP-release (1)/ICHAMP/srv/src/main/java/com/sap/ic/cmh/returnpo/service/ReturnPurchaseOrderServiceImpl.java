package com.sap.ic.cmh.returnpo.service;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.services.ErrorStatuses;
import com.sap.cds.services.ServiceException;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Message.Severity;
import com.sap.cds.services.messages.Messages;
import com.sap.cloud.sdk.cloudplatform.connectivity.ScpCfDestinationLoader;
import com.sap.ic.cmh.businessobjects.persistency.BusinessObjectDao;
import com.sap.ic.cmh.businessobjects.service.BusinessObjectService;
import com.sap.ic.cmh.claim.model.ResponseModel;
import com.sap.ic.cmh.claim.model.binary_relation.BinaryRelationDataModel;
import com.sap.ic.cmh.claim.validations.ClaimValidation;
import com.sap.ic.cmh.configuration.model.MasterData;
import com.sap.ic.cmh.configuration.persistency.BusinessObjectConfigurationDao;
import com.sap.ic.cmh.configuration.persistency.DestinationConfigurationDao;
import com.sap.ic.cmh.configuration.service.ConfigurationService;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.masterdata.unitofmeasure.service.UnitOfMeasureService;
import com.sap.ic.cmh.network.service.HttpService;
import com.sap.ic.cmh.qualitynotification.service.QualityNotificationService;
import com.sap.ic.cmh.returnpo.model.ReturnOrder;
import com.sap.ic.cmh.returnpo.persistency.ReturnPurchaseOrderDao;
import com.sap.ic.cmh.returnpo.validation.ReturnPurchaseOrderValidation;
import com.sap.ic.cmh.utils.CommonFunctions;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.configurationservice.BusinessObjectConfigurations;
import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.masterdataservice.CompanyCodes;
import cds.gen.masterdataservice.UnitOfMeasures;
import cds.gen.qualitynotificationservice.QualityNotifications;
import cds.gen.returnpurchaseorderservice.ReturnPurchaseOrders;

@Service
public class ReturnPurchaseOrderServiceImpl implements ReturnPurchaseOrderService {
	@Autowired
	CommonFunctions commonFunctions;
	@Autowired
	HttpService httpService;
	@Autowired
	ReturnPurchaseOrderDao returnPurchaseOrderDao;
	@Autowired
	ConfigurationService configurationService;

	@Autowired
	ReturnPurchaseOrderValidation returnPurchaseOrderValidation;

	@Autowired
	DestinationConfigurationDao destinationConfigDao;
	@Autowired
	BusinessObjectConfigurationDao businessObjectConfigurationDao;
	@Autowired
	QualityNotificationService qualityNotificationService;
	@Autowired
	BusinessObjectService businessObjectService;
	@Autowired
	UnitOfMeasureService unitOfMeasureService;

	@Autowired
	Messages messages;
	@Autowired
	ClaimValidation claimValidation;
	@Autowired
	BusinessObjectDao businessObjectDao;

	private static final Logger logger = LoggerFactory.getLogger(ReturnPurchaseOrderServiceImpl.class);
	private static final String RETURN_PURCHASE_ORDER_SERVICE_IMPL = "ReturnPurchaseOrderServiceImpl";

	/**
	 * get the configured destination based on company code id and business object
	 * type get the configured business object attributes based on complaint type
	 * code, destination and business object type
	 */
	@Override
	public void setConfiguredValues(ReturnPurchaseOrders returnPO, String boType, String complaintTypeCode) {
		LoggerHelper.logMethodEntry(logger, RETURN_PURCHASE_ORDER_SERVICE_IMPL, "setConfiguredValues");
		String destination = "";
		Result result = destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(returnPO.getCompanyId(),
				Constants.RETURNPO_CODE);

		destination = result.first().isPresent() ? result.list().get(0).get(Constants.DESTINATION).toString() : "";
		logger.info("Destination is ", destination);

		Result businessObjectConfigurationsResult = businessObjectConfigurationDao
				.getBusinessObjectConfigBasedOnDestinationAndBOAndDest(complaintTypeCode, boType, destination);

		List<BusinessObjectConfigurations> businessObjectConfigurations = businessObjectConfigurationsResult.first()
				.isPresent() ? businessObjectConfigurationsResult.listOf(BusinessObjectConfigurations.class) : null;
		if (null != businessObjectConfigurations && !CollectionUtils.isEmpty(businessObjectConfigurations)) {
			for (int i = 0; i < businessObjectConfigurations.size(); i++) {
				if (businessObjectConfigurations.get(i).getBusinessObjectAttributeCode()
						.equalsIgnoreCase(ReturnPurchaseOrders.RETURN_PURCHASE_TYPE)) {
					returnPO.setReturnPurchaseType(businessObjectConfigurations.get(i).getBusinessObjectValue());
				} else if (businessObjectConfigurations.get(i).getBusinessObjectAttributeCode()
						.equalsIgnoreCase(ReturnPurchaseOrders.ITEM_NUMBER)) {
					returnPO.setItemNumber(businessObjectConfigurations.get(i).getBusinessObjectValue());
				}
			}

		}
		LoggerHelper.logMethodExit(logger, RETURN_PURCHASE_ORDER_SERVICE_IMPL, "setConfiguredValues");
	}

	/**
	 * Create Return Purchase Order in the configured destination
	 */
	@Override
	public void createReturPurchaseOrder(ReturnPurchaseOrders returnPO) {
		LoggerHelper.logMethodEntry(logger, RETURN_PURCHASE_ORDER_SERVICE_IMPL, "createReturPurchaseOrder");
		String destination = "";
		ResponseModel responseModel = null;
		ReturnOrder returnOrderRequest = createReturnPORequest(returnPO);
		QualityNotifications qualityNotifications = qualityNotificationService
				.getQualityNotificationDetailsByComplaintId(returnPO.getComplaintId());

		Result result = destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(returnPO.getCompanyId(),
				Constants.RETURNPO_CODE);
		destination = result.first().isPresent() ? result.list().get(0).get(Constants.DESTINATION).toString() : "";
		logger.info("Destination for creation of RPO is ", destination);
		Result destinationConfigs = destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(
				returnPO.getCompanyId(), Constants.QUALITYNOTIFICATION_CODE);
		Optional<Row> destinationConfigsFirst = destinationConfigs.first();
		if (destinationConfigsFirst.isPresent()) {
			String cpiDestination = destinationConfigsFirst.get().get(Constants.DESTINATION).toString();
			setRequestForDocFlow(cpiDestination, qualityNotifications, returnOrderRequest, destination);
		}

		Map<String, Object> returnPORequestMap = commonFunctions.convertObjectToMap(returnOrderRequest);
		try {
			responseModel = httpService.callCpiFlow(Constants.RETURN_PO_APPENDED_URL, returnPORequestMap,
					destination);
			messages.throwIfError();

		} catch (IOException e) {
			throw new ServiceException(ErrorStatuses.BAD_REQUEST, MessageKeys.ERROR_IN_CPI_FLOW);
		}
		if (null != responseModel) {
			setReturnPurchaseOrderDetails(responseModel, returnPO);
			businessObjectService.insertBusinessObjectRelations(Constants.QUALITYNOTIFICATION_CODE,
					qualityNotifications.getId(), Constants.RETURNPO_CODE, returnPO.getId());
		}

		LoggerHelper.logMethodExit(logger, RETURN_PURCHASE_ORDER_SERVICE_IMPL, "createReturPurchaseOrder");
	}
    
	/**
	 * Set request for creating document flow between QN and RPO
	 * @param sourceDestination
	 * @param qualityNotifications
	 * @param returnOrderRequest
	 * @param targetDestination
	 */
	public void setRequestForDocFlow(String sourceDestination, QualityNotifications qualityNotifications,
			ReturnOrder returnOrderRequest, String targetDestination) {

		ScpCfDestinationLoader scpCfDestinationLoader = new ScpCfDestinationLoader();
		String qualityNotificationNumber = String.format("%012d",
				Integer.parseInt(qualityNotifications.getIdentifier()));
        String rfcDestination = httpService.getLogicalSystem(scpCfDestinationLoader, sourceDestination);
        String sourceLogicalSystem = httpService.getTargetLogicalSystem(scpCfDestinationLoader, rfcDestination);
		String targetLogicalSystem = httpService.getTargetLogicalSystem(scpCfDestinationLoader, targetDestination);
		BinaryRelationDataModel binaryRelationDataModel = commonFunctions.setBinaryRelationDataModel(
				qualityNotificationNumber, Constants.QUALITY_NOTIFICATION_OBJECT_TYPE,
				Constants.RETURN_PURCHASE_ORDER_OBJECT_TYPE,
				sourceLogicalSystem, targetLogicalSystem);
		returnOrderRequest.setBinaryRelationDataModel(binaryRelationDataModel);
	}

	public void setReturnPurchaseOrderDetails(ResponseModel responseModel, ReturnPurchaseOrders returnPO) {
		returnPO.setIdentifier(responseModel.getResult());
	}
    /**
     * Form a request payload to create Return Purchase Order
     * @param returnPO
     * @return
     */
	public ReturnOrder createReturnPORequest(ReturnPurchaseOrders returnPO) {
		LoggerHelper.logMethodEntry(logger, RETURN_PURCHASE_ORDER_SERVICE_IMPL, "createReturnPORequest");
		ReturnOrder returnOrder = new ReturnOrder();
		String personResponsible = "";
        List<String> attributesList = new ArrayList<>();
		MasterData masterDataDetails = configurationService.getMasterDataDetails(returnPO.getMaterialId(),
				returnPO.getPlantId(), returnPO.getSupplierId(), returnPO.getPurchasingOrganizationId());
		CompanyCodes companyCodeDetails = configurationService.getCompanyCodes(returnPO.getCompanyId());

		UnitOfMeasures unitOfMeasureDetails = unitOfMeasureService.getUnitOfMeasureDetails(returnPO.getUnit());
		String isoCode = null!=unitOfMeasureDetails ? unitOfMeasureDetails.getISOCode() : "";
		
		logger.info("ISO code for unit " + returnPO.getUnit() + "is : " + isoCode);
		/*Start of extensibility*/
		Map<String, Object> returnPORequestMap = commonFunctions.convertObjectToMap(returnPO);
		returnPORequestMap.replace(ReturnPurchaseOrders.PLANT_ID, returnPO.getPlantId(), masterDataDetails.getPlants().getPlant());
		returnPORequestMap.replace(ReturnPurchaseOrders.SUPPLIER_ID, returnPO.getSupplierId(), masterDataDetails.getSupplier().getBusinessPartnerNumber());
		returnPORequestMap.replace(ReturnPurchaseOrders.MATERIAL_ID, returnPO.getMaterialId(), masterDataDetails.getMaterial().getMaterialCode());
		returnPORequestMap.replace(ReturnPurchaseOrders.PURCHASING_ORGANIZATION_ID, returnPO.getPurchasingOrganizationId(),
				masterDataDetails.getPurchaseOrg().getPurchaseOrganization());
		returnPORequestMap.replace(ReturnPurchaseOrders.COMPANY_ID, returnPO.getCompanyId(), companyCodeDetails.getCompanyCode());
		if (StringUtils.isNotBlank(returnPO.getPersonResponsibleId())) {
			BusinessPartners supplier = configurationService.getSupplier(returnPO.getPersonResponsibleId());
			personResponsible = supplier.getBusinessPartnerNumber();
		}
        returnPORequestMap.replace(ReturnPurchaseOrders.PERSON_RESPONSIBLE_ID, returnPO.getPersonResponsibleId(),
				StringUtils.isNotBlank(personResponsible)? personResponsible : "");
		returnPORequestMap.replace(ReturnPurchaseOrders.UNIT, returnPO.getUnit(),
				isoCode);
       //Add unwanted attributes and remove it from the CPI request payload
		attributesList.add(ReturnPurchaseOrders.ID);
		attributesList.add(ReturnPurchaseOrders.COMPLAINT_ID);
		attributesList.add(ReturnPurchaseOrders.IS_RETURN_ORDER_FIELD_CONTROL);
		attributesList.add(ReturnPurchaseOrders.IS_RETURN_ORDER_FIELD_CONTROL_MANDATORY);
		attributesList.add(ReturnPurchaseOrders.IS_UPDATE_RESTRICTED);
		attributesList.add(ReturnPurchaseOrders.S_NAVIGATION);
		attributesList.add(ReturnPurchaseOrders.STATUS_CODE);
		attributesList.add(ReturnPurchaseOrders.CONTACT_PERSON_ID);
		attributesList.add(ReturnPurchaseOrders.MOVEMENT_TYPE);
		attributesList.add(ReturnPurchaseOrders.GOODS_ISSUE_DATE);
		attributesList.add(ReturnPurchaseOrders.BUSINESS_OBJECT_STATUSES);
		commonFunctions.removeAttributes(attributesList, returnPORequestMap);
		returnOrder.setReturnOrders(returnPORequestMap);
		/*End of extensibility*/
		LoggerHelper.logMethodExit(logger, RETURN_PURCHASE_ORDER_SERVICE_IMPL, "createReturnPORequest");
		return returnOrder;
	}
    
	/**
	 * Get details based on Return Purchase Order id
	 */
	@Override
	public ReturnPurchaseOrders getReturnPurchaseOrderDetails(String returnPurchaseOrderId) {
		Result returnPurchaseOrderDetails = returnPurchaseOrderDao.getReturnPurchaseOrderDetails(returnPurchaseOrderId);
		return returnPurchaseOrderDetails.first().isPresent()
				? returnPurchaseOrderDetails.listOf(ReturnPurchaseOrders.class).get(0)
				: null;
	}

    	/**
	 * Get details based on Return Purchase Order id
	 */
	@Override
	public ReturnPurchaseOrders getReturnPurchaseOrderBasedOnId(String returnPurchaseOrderId) {
		Result returnPurchaseOrderDetails = returnPurchaseOrderDao.getReturnPurchaseOrderBasedOnId(returnPurchaseOrderId);
		return returnPurchaseOrderDetails.first().isPresent()
				? returnPurchaseOrderDetails.listOf(ReturnPurchaseOrders.class).get(0)
				: null;
	}
    
	/**
	 * Validate field control of Return Purchase Order attributes
	 */
	public void validateReturnPurchaseOrderFields(ReturnPurchaseOrders returnPO) {
		LoggerHelper.logMethodEntry(logger, RETURN_PURCHASE_ORDER_SERVICE_IMPL, "validateReturnPurchaseOrderFields");
		returnPurchaseOrderValidation.validateMandatoryFields(returnPO);
		if (messages.stream().noneMatch(message -> message.getSeverity() == Severity.ERROR)) {
			returnPurchaseOrderValidation.validateFieldControlReturnPO(returnPO);
		}
		LoggerHelper.logMethodExit(logger, RETURN_PURCHASE_ORDER_SERVICE_IMPL, "validateReturnPurchaseOrderFields");
	}

	/**
	 * Check if Return PO exists in DB based on Number
	 */
	@Override
	public String checkIfReturnPOExistsBasedOnNumber(String returnPoNumber) {
		LoggerHelper.logMethodEntry(logger, RETURN_PURCHASE_ORDER_SERVICE_IMPL, "checkIfReturnPOExistsBasedOnNumber");
		Result result = returnPurchaseOrderDao.checkIfReturnPOExistsBasedOnNumber(returnPoNumber);
		LoggerHelper.logMethodExit(logger, RETURN_PURCHASE_ORDER_SERVICE_IMPL, "checkIfReturnPOExistsBasedOnNumber");
		return result.first().isPresent() ? result.list().get(0).get(ReturnPurchaseOrders.ID).toString() : "";
	}

	@Override
	public ReturnPurchaseOrders getReturnPurchaseOrderDetailsBasedOnComplaintId(String complaintId) {
		LoggerHelper.logMethodEntry(logger, RETURN_PURCHASE_ORDER_SERVICE_IMPL,
				"getReturnPurchaseOrderDetailsBasedOnComplaintId");
		Result returnPurchaseOrderDetails = returnPurchaseOrderDao
				.getReturnPurchaseOrderDetailsBasedOnComplaintId(complaintId);
		LoggerHelper.logMethodExit(logger, RETURN_PURCHASE_ORDER_SERVICE_IMPL,
				"getReturnPurchaseOrderDetailsBasedOnComplaintId");
		return returnPurchaseOrderDetails.first().isPresent()
				? returnPurchaseOrderDetails.listOf(ReturnPurchaseOrders.class).get(0)
				: null;
	}

	/**
	 * Check if Return Order is created for the complaint and check if Return Order
	 * is relevant
	 */
	@Override
	public void validateIfReturnPurchaseOrderExistsForComplaint(String complaintId) {
		LoggerHelper.logMethodEntry(logger, RETURN_PURCHASE_ORDER_SERVICE_IMPL,
				"validateIfReturnPurchaseOrderExistsForComplaint");
		returnPurchaseOrderValidation.validateIfReturnPurchaseOrderExistsForComplaint(complaintId);
		if (messages.stream().noneMatch(message -> message.getSeverity() == Message.Severity.ERROR)) {
			claimValidation.validateIfQualityNotificationExists(complaintId, Constants.RETURNPO_CODE);
		}
		if (messages.stream().noneMatch(message -> message.getSeverity() == Severity.ERROR)) {
			returnPurchaseOrderValidation.validateifBOIsRelevant(complaintId, Constants.RETURNPO_CODE);
		}
		LoggerHelper.logMethodExit(logger, RETURN_PURCHASE_ORDER_SERVICE_IMPL,
				"validateIfReturnPurchaseOrderExistsForComplaint");
	}

	/**
	 * Validate if Return Purchase Order exists and validate field control
	 */
	@Override
	public void validateIfReturnPurchaseOrderExists(ReturnPurchaseOrders returnPurchaseOrders) {
		LoggerHelper.logMethodEntry(logger, RETURN_PURCHASE_ORDER_SERVICE_IMPL, "validateIfReturnPurchaseOrderExists");
		returnPurchaseOrderValidation.validateIfReturnPurchaseOrderExists(returnPurchaseOrders.getId());
		if (messages.stream().noneMatch(message -> message.getSeverity() == Severity.ERROR)) {
			returnPurchaseOrderValidation.validateFieldControlReturnPO(returnPurchaseOrders);
		}
		LoggerHelper.logMethodExit(logger, RETURN_PURCHASE_ORDER_SERVICE_IMPL, "validateIfReturnPurchaseOrderExists");
	}

	/**
	 * Check if Return PO exists in DB based on Number
	 * 
	 * @param returnPONumber
	 * @return
	 */
	@Override
	public ReturnPurchaseOrders getReturnPurchaseOrderDetailsBasedOnNumber(String returnPONumber) {
		LoggerHelper.logMethodEntry(logger, RETURN_PURCHASE_ORDER_SERVICE_IMPL,
				"getReturnPurchaseOrderDetailsBasedOnNumber");
		Result returnPurchaseOrderDetails = returnPurchaseOrderDao.checkIfReturnPOExistsBasedOnNumber(returnPONumber);
		LoggerHelper.logMethodExit(logger, RETURN_PURCHASE_ORDER_SERVICE_IMPL,
				"getReturnPurchaseOrderDetailsBasedOnNumber");
		return returnPurchaseOrderDetails.first().isPresent()
				? returnPurchaseOrderDetails.listOf(ReturnPurchaseOrders.class).get(0)
				: null;
	}

	/**
	 * Get Return Order status code and company code based on return order id
	 */
	@Override
	public ReturnPurchaseOrders getReturnOrderStatusAndCompanyCode(String returnPurchaseOrderId) {
		LoggerHelper.logMethodEntry(logger, RETURN_PURCHASE_ORDER_SERVICE_IMPL, "getReturnOrderStatusAndCompanyCode");
		Result returnPurchaseOrderDetails = returnPurchaseOrderDao
				.getReturnOrderStatusAndCompanyCode(returnPurchaseOrderId);
		LoggerHelper.logMethodExit(logger, RETURN_PURCHASE_ORDER_SERVICE_IMPL, "getReturnOrderStatusAndCompanyCode");
		return returnPurchaseOrderDetails.first().isPresent()
				? returnPurchaseOrderDetails.listOf(ReturnPurchaseOrders.class).get(0)
				: null;
	}

	@Override
	public ReturnPurchaseOrders getDraftReturnOrderByComplaintID(String complaintId) {
		Result result = returnPurchaseOrderDao.getDraftReturnOrderByComplaintID(complaintId);
		return (result != null && result.first().isPresent()) ? result.listOf(ReturnPurchaseOrders.class).get(0) : null;
	}

	@Override
	public void deleteDraftReturnOrderByID(String returnPurchaseOrderId) {
		returnPurchaseOrderDao.deleteDraftReturnOrderByID(returnPurchaseOrderId);
	}

	@Override
	public cds.gen.managereturnpurchaseorderservice.ReturnPurchaseOrders getActiveReturnPurchaseOrders(
			String returnPurchaseOrderId) {
		Result result = returnPurchaseOrderDao.getActiveReturnPurchaseOrders(returnPurchaseOrderId);
		return (result != null && result.first().isPresent()) ? result.listOf(cds.gen.managereturnpurchaseorderservice.ReturnPurchaseOrders.class).get(0) : null;
	}
	

}