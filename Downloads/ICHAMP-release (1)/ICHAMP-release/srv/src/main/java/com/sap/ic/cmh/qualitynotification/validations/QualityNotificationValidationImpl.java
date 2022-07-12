package com.sap.ic.cmh.qualitynotification.validations;

import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.qualitynotificationservice.QualityNotifications;
import cds.gen.qualitynotificationservice.QualityNotifications_;
import cds.gen.qualitynotificationservice.Defects;
import cds.gen.qualitynotificationservice.Defects_;
import com.sap.cds.Result;
import java.util.Optional;
import com.sap.cds.Row;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.businessobjects.service.BusinessObjectService;
import com.sap.ic.cmh.configuration.persistency.ConfigurationDao;
import com.sap.ic.cmh.configuration.validations.MasterDataValidation;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.qualitynotification.persistency.QualityNotificationDao;
import com.sap.ic.cmh.qualitynotification.service.QualityNotificationService;
import com.sap.ic.cmh.configuration.validations.ConfigurationFieldsValidation;
import com.sap.ic.cmh.masterdata.defectcode.repository.DefectCodeRepository;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.utils.SecurityValidator;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class QualityNotificationValidationImpl implements QualityNotificationValidation {

	@Autowired
	Messages messages;

	@Autowired
	ConfigurationDao configurationDao;
	@Autowired
	ConfigurationFieldsValidation configurationFieldsValidation;
	@Autowired
	QualityNotificationService qualityNotificationService;
	@Autowired
	MasterDataValidation masterDataValidation;
	@Autowired
	BusinessObjectService businessObjectService;
	@Autowired
	SecurityValidator securityValidator;
	@Autowired
	QualityNotificationDao qualityNotificationDao;
	@Autowired
	DefectCodeRepository defectCodeRepository;

	private static final Logger logger = LoggerFactory.getLogger(QualityNotificationValidationImpl.class);
	private static final String QUALITY_NOTIFICATION_VALIDATION_IMPL = "QualityNotificationValidationImpl";
	private static final String VALIDATE_MANDATORY_FIELDS = "validateMandatoryFields";
	private static final String VALIDATE_FIELD_CONTROL = "validateFieldControlQN";
	private static final String VALIDATE_DEFECT_ATTRIBUTES = "validateDefectAttributes";

	/**
	 * Validate mandatory fields for Quality Notification
	 */
	@Override
	public void validateMandatoryFields(QualityNotifications qualityNotification) {
		LoggerHelper.logMethodEntry(logger, QUALITY_NOTIFICATION_VALIDATION_IMPL, VALIDATE_MANDATORY_FIELDS);
		validateQualityNotificationType(qualityNotification.getQnType());
		validateSupplierRole(qualityNotification.getSupplierRole());
		validateDefectAttributes(qualityNotification.getDefect());
		validatePersonResponsibleRole(qualityNotification.getPersonResponsibleRole());
		validatePersonResponsible(qualityNotification.getPersonResponsibleId());
		masterDataValidation.validateSupplier(qualityNotification.getSupplierId());
		masterDataValidation.validateSupplierPersonType(qualityNotification.getContactPersonId());
		LoggerHelper.logMethodExit(logger, QUALITY_NOTIFICATION_VALIDATION_IMPL, VALIDATE_MANDATORY_FIELDS);

	}

	@Override
	public void validateQNFreeTextFields(QualityNotifications qualityNotification) {
		validateDefectDescription(
				null != qualityNotification.getDefect() ? qualityNotification.getDefect().getDescription() : null);
		validatePurchaseOrderNumber(qualityNotification.getPurchaseOrderNumber());
		validatePurchaseOrderItemNumber(qualityNotification.getPurchaseOrderItem());
	}

	/**
	 * Validate if Quality notification type is configured
	 * 
	 * @param qnType
	 */
	public void validateQualityNotificationType(String qnType) {
		LoggerHelper.logMethodEntry(logger, QUALITY_NOTIFICATION_VALIDATION_IMPL, VALIDATE_MANDATORY_FIELDS);
		if (StringUtils.isBlank(qnType)) {
			messages.error(MessageKeys.NOTIFICATION_TYPE_NOT_CONFIGURED).target("in", QualityNotifications_.class,
					QualityNotifications_::qnType);
		}
		LoggerHelper.logMethodExit(logger, QUALITY_NOTIFICATION_VALIDATION_IMPL, VALIDATE_MANDATORY_FIELDS);
	}

	/**
	 * Validate if Supplier role is configured in the Configuration app
	 * 
	 * @param supplierRole
	 */
	public void validateSupplierRole(String supplierRole) {
		LoggerHelper.logMethodEntry(logger, QUALITY_NOTIFICATION_VALIDATION_IMPL, VALIDATE_MANDATORY_FIELDS);
		if (StringUtils.isBlank(supplierRole)) {
			messages.error(MessageKeys.SUPPLIER_ROLE_NOT_CONFIGURED).target("in", QualityNotifications_.class,
					QualityNotifications_::supplierRole);
		}
		LoggerHelper.logMethodExit(logger, QUALITY_NOTIFICATION_VALIDATION_IMPL, VALIDATE_MANDATORY_FIELDS);
	}

	/**
	 * Validate if Person responsible role is configured in the Configuration app
	 * 
	 * @param personResponsibleRole
	 */
	public void validatePersonResponsibleRole(String personResponsibleRole) {
		LoggerHelper.logMethodEntry(logger, QUALITY_NOTIFICATION_VALIDATION_IMPL, VALIDATE_MANDATORY_FIELDS);
		if (StringUtils.isBlank(personResponsibleRole)) {
			messages.error(MessageKeys.PERS_RESP_ROLE_NOT_CONFIGURED).target("in", QualityNotifications_.class,
					QualityNotifications_::personResponsibleRole);
		}
		LoggerHelper.logMethodExit(logger, QUALITY_NOTIFICATION_VALIDATION_IMPL, VALIDATE_MANDATORY_FIELDS);
	}

	/**
	 * Validate if Person responsible is not null
	 * 
	 * @param personResponsible
	 */
	public void validatePersonResponsible(String personResponsible) {
		LoggerHelper.logMethodEntry(logger, QUALITY_NOTIFICATION_VALIDATION_IMPL, VALIDATE_MANDATORY_FIELDS);
		if (StringUtils.isBlank(personResponsible)) {
			messages.error(MessageKeys.PERS_RESP_IS_MANDATORY).target("in", QualityNotifications_.class,
					QualityNotifications_::personResponsible);
		}
		validatePersonResponsibleIfExist(personResponsible);
		LoggerHelper.logMethodExit(logger, QUALITY_NOTIFICATION_VALIDATION_IMPL, VALIDATE_MANDATORY_FIELDS);
	}

	/**
	 * Validate if Person responsible is valid
	 * 
	 * @param personResponsible
	 */
	public void validatePersonResponsibleIfExist(String personResponsible) {
		LoggerHelper.logMethodEntry(logger, QUALITY_NOTIFICATION_VALIDATION_IMPL, VALIDATE_MANDATORY_FIELDS);
		if (personResponsible != null) {
			Result supplierDataResult = configurationDao.getSupplierData(personResponsible);
			BusinessPartners supplierData = supplierDataResult.first().isPresent()
					? supplierDataResult.listOf(BusinessPartners.class).get(0)
					: null;
			if (null == supplierData) {
				messages.error(MessageKeys.PERS_RESP_IS_NOT_VALID).target("in", QualityNotifications_.class,
						QualityNotifications_::personResponsible);
			} else if (Boolean.TRUE.equals(supplierData.getIsMarkedForDeletion())) {
				messages.error(MessageKeys.PERSON_RESP_MARKED_FOR_DELETION).target("in", QualityNotifications_.class,
						QualityNotifications_::personResponsible);
			} else if (!supplierData.getBusinessPartnerType()
					.equalsIgnoreCase(Constants.EXCHANGE_PARTNER_TYPE_PERSON_RESPONSIBLE)) {
				messages.error(MessageKeys.WRONG_PERSON_RESP_SELECTED).target("in", QualityNotifications_.class,
						QualityNotifications_::personResponsible);
			}
		}
		LoggerHelper.logMethodExit(logger, QUALITY_NOTIFICATION_VALIDATION_IMPL, VALIDATE_MANDATORY_FIELDS);
	}

	/**
	 * Validate Defect attributes
	 * 
	 * @param defect
	 */
	public void validateDefectAttributes(Defects defect) {
		LoggerHelper.logMethodEntry(logger, QUALITY_NOTIFICATION_VALIDATION_IMPL, VALIDATE_DEFECT_ATTRIBUTES);
		if (null == defect) {
			messages.error(MessageKeys.DEFECTS_MANDATORY).target("in", QualityNotifications_.class,
					QualityNotifications_::defect);
		} else if (null == defect.getDefectGroupCode()) {
			messages.error(MessageKeys.DEFECT_GROUP_MANDATORY).target("in", Defects_.class,
					Defects_::defectGroup_code);
		} else if (null == defect.getDefectCodeCode()) {
			messages.error(MessageKeys.DEFECT_CODE_MANDATORY).target("in", Defects_.class,
					Defects_::defectCode_code);
		} else {
			validateIfDefectAttributesExist(defect);
		}
		LoggerHelper.logMethodExit(logger, QUALITY_NOTIFICATION_VALIDATION_IMPL, VALIDATE_DEFECT_ATTRIBUTES);
	}

	/**
	 * Validate Defect attributes exists
	 * 
	 * @param defect
	 */
	public void validateIfDefectAttributesExist(Defects defect) {
		LoggerHelper.logMethodEntry(logger, QUALITY_NOTIFICATION_VALIDATION_IMPL, VALIDATE_DEFECT_ATTRIBUTES);
		Result defectCodeResult = qualityNotificationDao.getDefectCode(defect.getDefectCodeCode());
		if (!defectCodeResult.first().isPresent()) {
			messages.error(MessageKeys.DEFECT_CODE_NOT_EXIST).target("in", Defects_.class,
					Defects_::defectCode_code);
		}
		Result defectCode = defectCodeRepository.fetchDefectCode(defect.getDefectCodeCode(),
				defect.getDefectGroupCode());
		Optional<Row> defectCodeRow = defectCode.first();
		if (!defectCodeRow.isPresent()) {
			messages.error(MessageKeys.DEFECT_CODE_AND_DEFECT_GROUP_ARE_NOT_ASSOCIATED).target("in", Defects_.class,
					Defects_::defectGroup_code);
		}
		Result defectGroupCodeResult = qualityNotificationDao.getDefectGroup(defect.getDefectGroupCode());
		if (!defectGroupCodeResult.first().isPresent()) {
			messages.error(MessageKeys.DEFECT_GROUP_NOT_EXIST).target("in", Defects_.class,
					Defects_::defectGroup_code);
		}
		LoggerHelper.logMethodExit(logger, QUALITY_NOTIFICATION_VALIDATION_IMPL, VALIDATE_DEFECT_ATTRIBUTES);
	}

	/**
	 * Validate Field control and return error message if
	 * attributes are not editable
	 * 
	 * @param qn
	 */
	@Override
	public void validateFieldControlQN(QualityNotifications qn) {
		LoggerHelper.logMethodEntry(logger, QUALITY_NOTIFICATION_VALIDATION_IMPL, VALIDATE_FIELD_CONTROL);
		String qnStatus = qn.getStatusCode();
		QualityNotifications qualityNotificationDetails = qualityNotificationService
				.getQualityNotificationDetails(qn.getId());
		if (null != qualityNotificationDetails) {
			if (Constants.QN_STATUS_CLOSED.equalsIgnoreCase(qnStatus)) {
				messages.error(MessageKeys.ATTRIBUTES_NOT_EDITABLE)
						.target("in", QualityNotifications_.class, QualityNotifications_::ID);
			} else if (!Constants.STATUS_NEW.equalsIgnoreCase(qnStatus)) {
				logger.info("QN Status is other than NEW/Closed");
				validateQNProgress(qn, qualityNotificationDetails);
			}

		}
		LoggerHelper.logMethodExit(logger, QUALITY_NOTIFICATION_VALIDATION_IMPL, VALIDATE_FIELD_CONTROL);
	}

	/**
	 * Validate Field Editability if Quality notification has status
	 * other than New or closed
	 * 
	 * @param qn
	 * @param qnInDb
	 */
	public void validateQNProgress(QualityNotifications qn, QualityNotifications qnInDb) {
		LoggerHelper.logMethodEntry(logger, QUALITY_NOTIFICATION_VALIDATION_IMPL, VALIDATE_FIELD_CONTROL);
		logger.info("Defect from DB : ", qnInDb.getDefect());
		logger.info("Defect from qn request  : ", qn.getDefect());
		if (null != qn.getQnType() && null != qnInDb.getQnType()
				&& (!qnInDb.getQnType().equalsIgnoreCase(qn.getQnType()))) {
			messages.error(MessageKeys.NOTIFICATION_TYPE_NOT_EDITABLE)
					.target("in", QualityNotifications_.class, QualityNotifications_::qnType);
		} else if (null != qn.getPersonResponsibleId() && null != qnInDb.getPersonResponsibleId()
				&& (!qnInDb.getPersonResponsibleId().equalsIgnoreCase(qn.getPersonResponsibleId()))) {
			messages.error(MessageKeys.PERSON_RESPONISBLE_NOT_EDITABLE).target("in", QualityNotifications_.class,
					QualityNotifications_::personResponsible);
		} else if (null != qn.getPurchaseOrderNumber() && null != qnInDb.getPurchaseOrderNumber()
				&& (!qnInDb.getPurchaseOrderNumber().equalsIgnoreCase(qn.getPurchaseOrderNumber()))) {
			messages.error(MessageKeys.PURCHASE_ORDER_NOT_EDITABLE)
					.target("in", QualityNotifications_.class, QualityNotifications_::purchaseOrderNumber);
		} else if (null != qn.getPurchaseOrderItem() && null != qnInDb.getPurchaseOrderItem()
				&& (!qnInDb.getPurchaseOrderItem().equalsIgnoreCase(qn.getPurchaseOrderItem()))) {
			messages.error(MessageKeys.PURCHASE_ORDER_ITEM_NOT_EDITABLE)
					.target("in", QualityNotifications_.class, QualityNotifications_::purchaseOrderItem);
		} else if (null != qn.getDefect() && null != qnInDb.getDefect()) {
			validateFieldControlDefect(qn, qnInDb);
		}
		LoggerHelper.logMethodExit(logger, QUALITY_NOTIFICATION_VALIDATION_IMPL, VALIDATE_FIELD_CONTROL);
	}

	/**
	 * Validate field control for Defect attributes
	 * 
	 * @param qn
	 * @param qnInDb
	 */
	public void validateFieldControlDefect(QualityNotifications qn, QualityNotifications qnInDb) {
		LoggerHelper.logMethodEntry(logger, QUALITY_NOTIFICATION_VALIDATION_IMPL, VALIDATE_FIELD_CONTROL);
		if (null != qn.getDefect().getDefectCodeCode() && null != qnInDb.getDefect().getDefectCodeCode() &&
				!qnInDb.getDefect().getDefectCodeCode().equalsIgnoreCase(qn.getDefect().getDefectCodeCode())) {
			messages.error(MessageKeys.DEFECT_CODE_NOT_EDITABLE)
					.target("in", QualityNotifications_.class, qnotif -> qnotif.defect().defectCode_code());
		} else if (null != qn.getDefect().getDefectGroupCode() && null != qnInDb.getDefect().getDefectGroupCode() &&
				!qnInDb.getDefect().getDefectGroupCode().equalsIgnoreCase(qn.getDefect().getDefectGroupCode())) {
			messages.error(MessageKeys.DEFECT_GROUP_NOT_EDITABLE)
					.target("in", QualityNotifications_.class, qnotif -> qnotif.defect().defectGroup_code());
		} else if (null != qn.getDefect().getDescription() && null != qnInDb.getDefect().getDescription() &&
				!qnInDb.getDefect().getDescription().equalsIgnoreCase(qn.getDefect().getDescription())) {
			messages.error(MessageKeys.DEFECT_DESCRIPTION_NOT_EDITABLE)
					.target("in", QualityNotifications_.class, qnotif -> qnotif.defect().description());
		}
		LoggerHelper.logMethodExit(logger, QUALITY_NOTIFICATION_VALIDATION_IMPL, VALIDATE_FIELD_CONTROL);
	}

	/**
	 * Validate if quality notification exists for a complaint
	 */
	@Override
	public void validateIfQNExistsForComplaint(String complaintId) {
		LoggerHelper.logMethodEntry(logger, QUALITY_NOTIFICATION_VALIDATION_IMPL, "validateIfQNExistsForComplaint");
		QualityNotifications qualityNotificationDetailsByComplaintId = qualityNotificationService
				.getQualityNotificationDetailsByComplaintId(complaintId);
		if (null != qualityNotificationDetailsByComplaintId) {
			messages.error(MessageKeys.QUALITY_NOTIFICATION_EXISTS).target("in", QualityNotifications_.class,
					QualityNotifications_::ID);
		}
		LoggerHelper.logMethodExit(logger, QUALITY_NOTIFICATION_VALIDATION_IMPL, "validateIfQNExistsForComplaint");
	}

	/**
	 * Validate if quality notification exists
	 */
	@Override
	public void validateIfQNExists(String id) {
		LoggerHelper.logMethodEntry(logger, QUALITY_NOTIFICATION_VALIDATION_IMPL, "validateIfQNExists");
		QualityNotifications qualityNotificationDetails = qualityNotificationService.getQualityNotificationDetails(id);
		if (null == qualityNotificationDetails) {
			messages.error(MessageKeys.QUALITY_NOTIFICATION_NOT_EXIST).target("in", QualityNotifications_.class,
					QualityNotifications_::ID);
		}
		LoggerHelper.logMethodExit(logger, QUALITY_NOTIFICATION_VALIDATION_IMPL, "validateIfQNExists");
	}

	/**
	 * Validate if quality notification is relevant for the complaint
	 */
	@Override
	public void validateIfBOIsRelevant(String complaintId, String qualityNotificationCode) {
		boolean checkIfBOIsRelevant = businessObjectService.checkIfBOIsRelevant(complaintId, qualityNotificationCode);
		if (!checkIfBOIsRelevant) {
			messages.error(MessageKeys.QUALITY_NOTIFICATION_ALWAYS_RELEVANT).target("in", QualityNotifications_.class,
					QualityNotifications_::ID);
		}

	}

	/**
	 * Validate Defect Description
	 * 
	 * @param {@link String}
	 *
	 * @private
	 */
	private void validateDefectDescription(String defectDescription) {
		LoggerHelper.logMethodEntry(logger, QUALITY_NOTIFICATION_VALIDATION_IMPL, "validateDefectDescription");
		if (!securityValidator.isValidText(defectDescription)) {
			messages.error(MessageKeys.INVALID_DEFECT_DESCRIPTION).target("in", QualityNotifications_.class,
					qn -> qn.defect().description());
		}
		LoggerHelper.logMethodExit(logger, QUALITY_NOTIFICATION_VALIDATION_IMPL, "validateDefectDescription");
	}

	/**
	 * Validate Purchase Order Number
	 * 
	 * @param {@link String}
	 *
	 * @public
	 */
	@Override
	public void validatePurchaseOrderNumber(String purchaseOrderNumber) {
		LoggerHelper.logMethodEntry(logger, QUALITY_NOTIFICATION_VALIDATION_IMPL, "validatePurchaseOrderNumber");
		if (StringUtils.isNotBlank(purchaseOrderNumber) && !purchaseOrderNumber.matches("[0-9]+")) {
			messages.error(MessageKeys.PURCHASE_NUMBER_NOT_VALID).target("in", QualityNotifications_.class,
					qn -> qn.purchaseOrderNumber());
		}
		LoggerHelper.logMethodExit(logger, QUALITY_NOTIFICATION_VALIDATION_IMPL, "validatePurchaseOrderNumber");
	}

	/**
	 * Validate Purchase Order Item
	 * 
	 * @param {@link String}
	 *
	 * @public
	 */
	@Override
	public void validatePurchaseOrderItemNumber(String purchaseOrderItem) {
		LoggerHelper.logMethodEntry(logger, QUALITY_NOTIFICATION_VALIDATION_IMPL, "validatePurchaseOrderItem");
		if (StringUtils.isNotBlank(purchaseOrderItem) && !purchaseOrderItem.matches("[0-9]+")) {
			messages.error(MessageKeys.PURCHASE_ITEM_NOT_VALID).target("in", QualityNotifications_.class,
					qn -> qn.purchaseOrderItem());
		}
		LoggerHelper.logMethodExit(logger, QUALITY_NOTIFICATION_VALIDATION_IMPL, "validatePurchaseOrderItem");
	}
}