package com.sap.ic.cmh.supplierissueprocess.validations;

import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.qualitynotificationservice.QualityNotifications;
import cds.gen.qualitynotificationservice.QualityNotifications_;
import cds.gen.supplierissueprocessservice.Supplier8DProcesses;
import cds.gen.supplierissueprocessservice.Supplier8DProcesses_;

import com.sap.cds.Result;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.businessobjects.service.BusinessObjectService;
import com.sap.ic.cmh.configuration.persistency.ConfigurationDao;
import com.sap.ic.cmh.configuration.validations.MasterDataValidation;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.supplierissueprocess.persistency.EightDDao;
import com.sap.ic.cmh.supplierissueprocess.service.EightDService;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.SecurityValidator;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class EightDValidationImpl implements EightDValidation {
	@Autowired
	Messages messages;

	@Autowired
	SecurityValidator securityValidator;

	@Autowired
	EightDDao eightDDao;

	@Autowired
	MasterDataValidation masterDataValidation;

	@Autowired
	PersistenceService db;
	@Autowired
	EightDService eightDService;
	@Autowired
	BusinessObjectService businessObjectService;
	@Autowired
	ConfigurationDao configurationDao;

	public static final Logger logger = LoggerFactory.getLogger(EightDValidationImpl.class);

	/**
	 * validation for Claim Attributes & raise error message for any invalid or null
	 * attribute
	 *
	 * @param {@link String}
	 * @public
	 */
	@Override
	public void validateEightDFields(Supplier8DProcesses eightD) {
		validateSupplier8DProcessType(eightD.getSupplierIssueProcessesType());
		masterDataValidation.validateSupplierPersonType(eightD.getContactPersonId());
		validatePersonResponsible(eightD.getPersonResponsibleId());
		
	}

	/**
	 * Validate if Supplier8D type is not null
	 *
	 * @param supplier8DType
	 * @public
	 */
	public void validateSupplier8DProcessType(String supplier8DType) {
		if (supplier8DType == null || supplier8DType.isEmpty()) {
			messages.error(MessageKeys.SUPPLIER_TYPE_NOT_CONFIGURED).target("in", Supplier8DProcesses_.class,
					Supplier8DProcesses_::supplierIssueProcessesType);
		}
		validateSupplier8DProcessTypeIfExist(supplier8DType);
	}

	private void validateSupplier8DProcessTypeIfExist(String supplier8DType) {
		if (supplier8DType != null && !supplier8DType.isEmpty() && !securityValidator.isValidText(supplier8DType)) {
			messages.error(MessageKeys.INVALID_8D_SUPPLIER_PROCESS_TYPE).target("in", Supplier8DProcesses_.class,
					Supplier8DProcesses_::supplierIssueProcessesType);
		}
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
				messages.error(MessageKeys.PERS_RESP_IS_NOT_VALID).target("in", Supplier8DProcesses_.class,
						Supplier8DProcesses_::personResponsible);
			}else if(Boolean.TRUE.equals(supplierData.getIsMarkedForDeletion())) {
				messages.error(MessageKeys.PERSON_RESP_MARKED_FOR_DELETION).target("in", Supplier8DProcesses_.class,
						Supplier8DProcesses_::personResponsible);
			}else if(!supplierData.getBusinessPartnerType().equalsIgnoreCase(Constants.EXCHANGE_PARTNER_TYPE_PERSON_RESPONSIBLE)) {
				messages.error(MessageKeys.WRONG_PERSON_RESP_SELECTED).target("in", Supplier8DProcesses_.class,
						Supplier8DProcesses_::personResponsible);
			}
		}
	}

	/**
	 * Validate Field control and return error message if
	 *
	 * @param eightD
	 */
	@Override
	public void validateFieldControlEightD(Supplier8DProcesses eightD) {
		String supplier8DStatus = eightD.getStatusCode();
		Supplier8DProcesses eightDDetails = eightDService.getEightDDetails(eightD.getId());
		if (null!=eightDDetails) {
			if (Constants.SUPPLIER_ISSUE_PROCESS_STATUS_CLOSED.equalsIgnoreCase(supplier8DStatus)) {
				messages.error(MessageKeys.ATTRIBUTES_NOT_EDITABLE).target("in", Supplier8DProcesses_.class,
						Supplier8DProcesses_::ID);
			} else if (!Constants.STATUS_NEW.equalsIgnoreCase(supplier8DStatus)) {
                logger.info("8D Status is other than NEW/Closed");
				validateEightDProgress(eightD, eightDDetails);
			}
		}

	}

	/**
	 * Validate if Supplier 8D exists for the complaint
	 */
	@Override
	public void validateIf8DExistsForComplaint(String complaintId) {
		Supplier8DProcesses supplier8DDetailsBasedOnComplaintId = eightDService
				.getEightDDetailsBasedOnComplaintId(complaintId);
		if (null != supplier8DDetailsBasedOnComplaintId) {
			messages.error(MessageKeys.SUPPLIER_EIGHTD_EXISTS).target("in", Supplier8DProcesses_.class,
					Supplier8DProcesses_::ID);
		}

	}

	/**
	 * Validate if Supplier 8D is relevant
	 */
	@Override
	public void validateifBOIsRelevant(String complaintId, String supplierEightdCode) {
		boolean checkIfBOIsRelevant = businessObjectService.checkIfBOIsRelevant(complaintId, supplierEightdCode);
		if (!checkIfBOIsRelevant) {
			messages.error(MessageKeys.SUPPLIER_EIGHTD_NOT_RELEVANT).target("in", Supplier8DProcesses_.class,
					Supplier8DProcesses_::ID);
		}
	}

	/**
	 * Validate if Supplier 8D exists
	 */
	@Override
	public void validateIf8DExists(String supplierEightDId) {
		Supplier8DProcesses eightDDetails = eightDService.getEightDDetails(supplierEightDId);
		if (null == eightDDetails) {
			messages.error(MessageKeys.SUPPLIER_EIGHTD_NOT_EXIST).target("in", Supplier8DProcesses_.class,
					Supplier8DProcesses_::ID);
		}
	}

	/**
	 * Validate Field control for Supplier 8D
	 */
    public void validateEightDProgress(Supplier8DProcesses eightD, Supplier8DProcesses eightDInDb) {
        if (null!=eightD.getSupplierIssueProcessesType() && (!eightDInDb.getSupplierIssueProcessesType().equalsIgnoreCase(eightD.getSupplierIssueProcessesType()))) {
            messages.error(MessageKeys.NOTIFICATION_TYPE_NOT_EDITABLE)
                    .target("in", Supplier8DProcesses_.class,Supplier8DProcesses_::supplierIssueProcessesType);
        } else if (null!=eightD.getPersonResponsibleId() && (!eightDInDb.getPersonResponsibleId().equalsIgnoreCase(eightD.getPersonResponsibleId()))) {
            messages.error(MessageKeys.PERSON_RESPONISBLE_NOT_EDITABLE).target("in", Supplier8DProcesses_.class,
            		Supplier8DProcesses_::personResponsible);
        }

    }

    @Override
    public void validateQNFields(QualityNotifications qualityNotification) {
        if(StringUtils.isBlank(qualityNotification.getPurchaseOrderNumber())){
            messages.error(MessageKeys.PURCHASE_ORDER_NUMBER_MANDATORY).target("in", QualityNotifications_.class,
            		QualityNotifications_::purchaseOrderNumber);
        }else if(StringUtils.isBlank(qualityNotification.getPurchaseOrderItem())){
             messages.error(MessageKeys.PURCHASE_ORDER_ITEM_NUMBER_MANDATORY).target("in", QualityNotifications_.class,
            		QualityNotifications_::purchaseOrderItem);
        }
        
    }
        
}
