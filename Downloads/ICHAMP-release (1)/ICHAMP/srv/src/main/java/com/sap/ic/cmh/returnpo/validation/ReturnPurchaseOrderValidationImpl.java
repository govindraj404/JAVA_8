package com.sap.ic.cmh.returnpo.validation;

import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.masterdataservice.PurchasingGroups_;
import cds.gen.masterdataservice.Reasons_;
import cds.gen.masterdataservice.Reasons;
import cds.gen.returnpurchaseorderservice.ReturnPurchaseOrders;
import cds.gen.returnpurchaseorderservice.ReturnPurchaseOrders_;
import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.businessobjects.service.BusinessObjectService;
import com.sap.ic.cmh.configuration.persistency.ConfigurationDao;
import com.sap.ic.cmh.configuration.validations.MasterDataValidation;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.returnpo.service.ReturnPurchaseOrderService;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.utils.SecurityValidator;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ReturnPurchaseOrderValidationImpl implements ReturnPurchaseOrderValidation {

	@Autowired
	Messages messages;
	@Autowired
	ReturnPurchaseOrderService returnPurchaseOrderService;
	@Autowired
	MasterDataValidation masterDataValidation;
	@Autowired
	PersistenceService db;
	@Autowired
	BusinessObjectService businessObjectService;
	@Autowired
	SecurityValidator securityValidator;
	@Autowired
	ConfigurationDao configurationDao;

	public static final Logger logger = LoggerFactory.getLogger(ReturnPurchaseOrderValidationImpl.class);
	private static final String RETURN_PURCHASE_ORDER_VALIDATION_IMPL = "ReturnPurchaseOrderValidationImpl";
	private static final String VALIDATE_MANDATORY_FIELDS = "validateMandatoryFields";
	private static final String VALIDATE_FIELD_CONTROL_RPO = "validateFieldControlReturnPO";

	/**
	 * validation for Return Purchase Order Attributes & raise error message for any
	 * invalid or null attribute
	 * 
	 * @param returnPO
	 */
	@Override
	public void validateMandatoryFields(ReturnPurchaseOrders returnPO) {
		LoggerHelper.logMethodEntry(logger, RETURN_PURCHASE_ORDER_VALIDATION_IMPL, VALIDATE_MANDATORY_FIELDS);
		validateType(returnPO.getReturnPurchaseType());
		validateReason(returnPO.getReasonCode());
		validateItemNumer(returnPO.getItemNumber());
		validatePurchasingGroup(returnPO.getPurchasingGroupCode());
		validatePersonResponsible(returnPO.getPersonResponsibleId());
        masterDataValidation.validateMaterial(returnPO.getMaterialId());
		masterDataValidation.validateSupplierPersonType(returnPO.getContactPersonId());
		masterDataValidation.validatePurchaseOrg(returnPO.getPurchasingOrganizationId());
		
		LoggerHelper.logMethodExit(logger, RETURN_PURCHASE_ORDER_VALIDATION_IMPL, VALIDATE_MANDATORY_FIELDS);
	}

	/**
	 * Validate and throw error message if purchasing group is blank
	 * 
	 * @param purchasingGroup
	 */
	public void validatePurchasingGroup(String purchasingGroup) {
		LoggerHelper.logMethodEntry(logger, RETURN_PURCHASE_ORDER_VALIDATION_IMPL, VALIDATE_MANDATORY_FIELDS);
		if (StringUtils.isBlank(purchasingGroup)) {
			messages.error(MessageKeys.PURCHASING_GROUP_IS_MANDATORY).target("in", ReturnPurchaseOrders_.class,
					returnPO -> returnPO.purchasingGroup_code());
		}
		validatePurchasingGroupIfExist(purchasingGroup);
		LoggerHelper.logMethodExit(logger, RETURN_PURCHASE_ORDER_VALIDATION_IMPL, VALIDATE_MANDATORY_FIELDS);
	}

		/**
	 * Validate and throw error message if purchasing group is not valid
	 * 
	 * @param purchasingGroup
	 */
	public void validatePurchasingGroupIfExist(String purchasingGroup) {
		LoggerHelper.logMethodEntry(logger, RETURN_PURCHASE_ORDER_VALIDATION_IMPL, VALIDATE_MANDATORY_FIELDS);
		if (!StringUtils.isBlank(purchasingGroup)) {
			Result pGroup = db.run(Select.from(PurchasingGroups_.class).where(rpo->rpo.code().eq(purchasingGroup)));
			if(!pGroup.first().isPresent()){
				messages.error(MessageKeys.PURCHASING_GROUP_NOT_EXIST).target("in", ReturnPurchaseOrders_.class,
					returnPO -> returnPO.purchasingGroup_code());
			}
		}
		LoggerHelper.logMethodExit(logger, RETURN_PURCHASE_ORDER_VALIDATION_IMPL, VALIDATE_MANDATORY_FIELDS);
	}

	/**
	 * Validate and throw error message if item number is blank
	 * 
	 * @param itemNumber
	 */
	public void validateItemNumer(String itemNumber) {
		LoggerHelper.logMethodEntry(logger, RETURN_PURCHASE_ORDER_VALIDATION_IMPL, VALIDATE_MANDATORY_FIELDS);
		if (StringUtils.isBlank(itemNumber)) {
			messages.error(MessageKeys.ITEM_NUMBER_NOT_MAINTAINED).target("in", ReturnPurchaseOrders_.class,
					returnPO -> returnPO.itemNumber());
		}
		LoggerHelper.logMethodExit(logger, RETURN_PURCHASE_ORDER_VALIDATION_IMPL, VALIDATE_MANDATORY_FIELDS);
	}

	/**
	 * Validate and throw error message if Reason is blank
	 * 
	 * @param reasonCode
	 */
	public void validateReason(String reasonCode) {
		LoggerHelper.logMethodEntry(logger, RETURN_PURCHASE_ORDER_VALIDATION_IMPL, VALIDATE_MANDATORY_FIELDS);
		if (StringUtils.isBlank(reasonCode)) {
			messages.error(MessageKeys.RETURN_ORDER_REASON_IS_MANDATORY).target("in", ReturnPurchaseOrders_.class,
					returnPO -> returnPO.reason_code());
		}
		validateReasonIfExist(reasonCode);
		LoggerHelper.logMethodExit(logger, RETURN_PURCHASE_ORDER_VALIDATION_IMPL, VALIDATE_MANDATORY_FIELDS);
	}

		/**
	 * Validate and throw error message if Reason is not valid
	 * 
	 * @param reasonCode
	 */
	public void validateReasonIfExist(String reasonCode) {
		LoggerHelper.logMethodEntry(logger, RETURN_PURCHASE_ORDER_VALIDATION_IMPL, VALIDATE_MANDATORY_FIELDS);
		if (!StringUtils.isBlank(reasonCode)) {
			Result reason = db.run(Select.from(Reasons_.class).columns(Reasons.CODE).where(rpo->rpo.code().eq(reasonCode)));
			if(!reason.first().isPresent()){
				messages.error(MessageKeys.RETURN_ORDER_REASON_NOT_EXIST).target("in", ReturnPurchaseOrders_.class,
					returnPO -> returnPO.reason_code());
			}
		LoggerHelper.logMethodExit(logger, RETURN_PURCHASE_ORDER_VALIDATION_IMPL, VALIDATE_MANDATORY_FIELDS);
		}
	}

	/**
	 * Validate and throw error message if return purchase type is blank
	 * 
	 * @param typeCode
	 */
	public void validateType(String typeCode) {
		LoggerHelper.logMethodEntry(logger, RETURN_PURCHASE_ORDER_VALIDATION_IMPL, VALIDATE_MANDATORY_FIELDS);
		if (StringUtils.isBlank(typeCode)) {
			messages.error(MessageKeys.RETURN_ORDER_TYPE_NOT_MAINTAINED).target("in", ReturnPurchaseOrders_.class,
					returnPO -> returnPO.returnPurchaseType());
		}
		LoggerHelper.logMethodExit(logger, RETURN_PURCHASE_ORDER_VALIDATION_IMPL, VALIDATE_MANDATORY_FIELDS);
	}
	
	/**
	 * Validate Person Responsible if exists
	 * @param personResponsibleId
	 */
	public void validatePersonResponsible(String personResponsibleId) {
		if (personResponsibleId != null) {
			Result supplierDataResult = configurationDao.getSupplierData(personResponsibleId);
		BusinessPartners supplierData = supplierDataResult.first().isPresent() ? supplierDataResult.listOf(BusinessPartners.class).get(0) : null;
			if (null == supplierData) {
				messages.error(MessageKeys.PERS_RESP_IS_NOT_VALID).target("in", ReturnPurchaseOrders_.class,
						ReturnPurchaseOrders_::personResponsible);
			}else if(Boolean.TRUE.equals(supplierData.getIsMarkedForDeletion())) {
				messages.error(MessageKeys.BUSINESS_PARTNER_MARKED_FOR_DELETION).target("in", ReturnPurchaseOrders_.class,
						ReturnPurchaseOrders_::personResponsible);
			}else if(!supplierData.getBusinessPartnerType().equalsIgnoreCase(Constants.EXCHANGE_PARTNER_TYPE_PERSON_RESPONSIBLE)) {
				messages.error(MessageKeys.WRONG_PERSON_RESP_SELECTED).target("in", ReturnPurchaseOrders_.class,
						ReturnPurchaseOrders_::personResponsible);
			}
		}
	}

	@Override
	public void validateFieldControlReturnPO(ReturnPurchaseOrders returnPO) {
		LoggerHelper.logMethodEntry(logger, RETURN_PURCHASE_ORDER_VALIDATION_IMPL, VALIDATE_FIELD_CONTROL_RPO);
		String returnPurchaseOrderStatus = returnPO.getStatusCode();
		ReturnPurchaseOrders returnOrderDetails = returnPurchaseOrderService
				.getReturnPurchaseOrderDetails(returnPO.getId());

		if (null != returnOrderDetails) {
			if (Constants.RPO_STATUS_CLOSED.equalsIgnoreCase(returnPurchaseOrderStatus)) {
				messages.error(MessageKeys.ATTRIBUTES_NOT_EDITABLE).target("in", ReturnPurchaseOrders_.class,
						rpo -> rpo.ID());
			} else if (!Constants.STATUS_NEW.equalsIgnoreCase(returnPurchaseOrderStatus)) {
                logger.info("RPO Status is other than NEW/Closed");
				validateReturnOrderProgressStatus(returnPO, returnOrderDetails);
			}
		}
		LoggerHelper.logMethodExit(logger, RETURN_PURCHASE_ORDER_VALIDATION_IMPL, VALIDATE_FIELD_CONTROL_RPO);
	}

	private void validateReturnOrderProgressStatus(ReturnPurchaseOrders returnPO,
			ReturnPurchaseOrders returnOrderDetailsInDb) {
		LoggerHelper.logMethodEntry(logger, RETURN_PURCHASE_ORDER_VALIDATION_IMPL, VALIDATE_FIELD_CONTROL_RPO);
		if (null != returnPO.getReasonCode()
				&& (!returnOrderDetailsInDb.getReasonCode().equalsIgnoreCase(returnPO.getReasonCode()))) {
			messages.error(MessageKeys.RETURN_ORDER_REASON_NOT_EDITABLE).target("in", ReturnPurchaseOrders_.class,
					rpo -> rpo.reason_code());
		} else if (null != returnPO.getPersonResponsibleId()
				&& (!returnOrderDetailsInDb.getPersonResponsibleId().equalsIgnoreCase(returnPO.getPersonResponsibleId()))) {
			messages.error(MessageKeys.PERSON_RESPONISBLE_NOT_EDITABLE).target("in", ReturnPurchaseOrders_.class,
					rpo -> rpo.personResponsible());

		}
		LoggerHelper.logMethodExit(logger, RETURN_PURCHASE_ORDER_VALIDATION_IMPL, VALIDATE_FIELD_CONTROL_RPO);
	}
    /**
     * Validate if Return order exists for the complaint
     */
	@Override
	public void validateIfReturnPurchaseOrderExistsForComplaint(String complaintId) {
		LoggerHelper.logMethodEntry(logger, RETURN_PURCHASE_ORDER_VALIDATION_IMPL, "validateIfReturnPurchaseOrderExistsForComplaint");
		ReturnPurchaseOrders returnPurchaseOrderDetailsBasedOnComplaintId = returnPurchaseOrderService.getReturnPurchaseOrderDetailsBasedOnComplaintId(complaintId);
		if(null!=returnPurchaseOrderDetailsBasedOnComplaintId) {
			messages.error(MessageKeys.RETURN_PURCHASE_ORDER_EXISTS).target("in", ReturnPurchaseOrders_.class,
					rpo -> rpo.ID());
		}
		LoggerHelper.logMethodEntry(logger, RETURN_PURCHASE_ORDER_VALIDATION_IMPL, "validateIfReturnPurchaseOrderExistsForComplaint");	
	}
    /**
     * Validate if Business Object is relevant
     */
	@Override
	public void validateifBOIsRelevant(String complaintId, String returnpoCode) {
		LoggerHelper.logMethodEntry(logger, RETURN_PURCHASE_ORDER_VALIDATION_IMPL, "validateifBOIsRelevant");
		boolean checkIfBOIsRelevant = businessObjectService.checkIfBOIsRelevant(complaintId,returnpoCode);
		if(!checkIfBOIsRelevant) {
			messages.error(MessageKeys.RETURN_PURCHASE_ORDER_NOT_RELEVANT).target("in", ReturnPurchaseOrders_.class,
					rpo -> rpo.ID());
		}
		LoggerHelper.logMethodExit(logger, RETURN_PURCHASE_ORDER_VALIDATION_IMPL, "validateifBOIsRelevant");
	}
	 /**
     * Validate if Return purchase order exists for the Return purchase order id
     */
	@Override
	public void validateIfReturnPurchaseOrderExists(String returnPurchaseOrderId) {
		LoggerHelper.logMethodEntry(logger, RETURN_PURCHASE_ORDER_VALIDATION_IMPL, "validateIfReturnPurchaseOrderExists");
		ReturnPurchaseOrders returnPurchaseOrderDetails = returnPurchaseOrderService.getReturnPurchaseOrderDetails(returnPurchaseOrderId);
		if(null==returnPurchaseOrderDetails) {
			messages.error(MessageKeys.RETURN_PURCHASE_ORDER_NOT_EXIST).target("in", ReturnPurchaseOrders_.class,
					rpo -> rpo.ID());
		}
		LoggerHelper.logMethodExit(logger, RETURN_PURCHASE_ORDER_VALIDATION_IMPL, "validateIfReturnPurchaseOrderExists");
	}

}