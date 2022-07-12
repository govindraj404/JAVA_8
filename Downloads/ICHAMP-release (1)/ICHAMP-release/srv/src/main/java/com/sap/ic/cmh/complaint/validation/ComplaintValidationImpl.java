package com.sap.ic.cmh.complaint.validation;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.complaint.service.ComplaintService;
import com.sap.ic.cmh.configuration.validations.MasterDataValidation;
import com.sap.ic.cmh.configuration.validations.ConfigurationFieldsValidation;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.masterdata.common.service.MasterDataService;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.utils.SecurityValidator;

import cds.gen.complaintservice.Complaints;
import cds.gen.complaintservice.Complaints_;
import cds.gen.masterdataservice.BusinessPartners;

@Component
public class ComplaintValidationImpl implements ComplaintValidation {

	@Autowired
	MasterDataService masterDataService;
	@Autowired
	ComplaintService complaintService;
	@Autowired
	MasterDataValidation masterDataValidation;
	@Autowired
	Messages messages;
	@Autowired
	SecurityValidator securityValidator;
    @Autowired
	ConfigurationFieldsValidation configurationFieldsValidation;

	public static final Logger logger = LoggerHelper.getLogger(ComplaintValidationImpl.class);
	private static final String COMPLAINT_VALIDATION_IMPL = "ComplaintValidatorImpl";
	private static final String VALIDATE_COMPLAINT_ATTRIBUTES = "validateComplaintAttributes";
	private static final String VALIDATE_MASTER_DATA_DELETION = "validateMasterDataDeletion";

	/**
	 * Validate Plant
	 */
	public void validatePlant(String plantId) {
		masterDataValidation.validatePlant(plantId);
	}

	/**
	 * Validate Complaint with MasterData Attributes
	 * 
	 * @param complaint
	 */
	@Override
	public void validateComplaintBeforeCreate(Complaints complaint) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_VALIDATION_IMPL, "validateComplaint");
		Complaints comp = complaintService.getComplaintDetails(complaint.getId());
		if (comp == null) {
			validateMasterDataDeletion(complaint);
			masterDataValidation.validateSupplierPersonType(complaint.getContactPersonId());
			validateResponsiblePerson(complaint.getPersonResponsibleId());
		} else {
			messages.error(MessageKeys.COMPLAINT_ALREADY_EXISTS).target("in", Complaints_.class,
					complaints -> complaint.getId());

		}
		validateUnitOfMeasure(complaint.getUnitCode());
        if(complaint.getQuantity()!=null && !configurationFieldsValidation.isValidateNumericValue(complaint.getQuantity())){
			messages.error(MessageKeys.NUMBER_IS_NEGATIVE).target("in", Complaints_.class,
					Complaints_::quantity);
		}
		LoggerHelper.logMethodExit(logger, COMPLAINT_VALIDATION_IMPL, "validateComplaint");
	}

	@Override
	public void validateComplaintFreeTextFields(Complaints complaint) {
		validateReferenceNumber(complaint.getReferenceNumber());
		validateDescription(complaint.getDescription());
		validateNote(complaint.getNote());
	}

	@Override
	public boolean validateResponsiblePerson(String personResponsible) {
		if (StringUtils.isNotBlank(personResponsible) && !masterDataValidation.validateBTPUser(personResponsible)) {
			messages.error(MessageKeys.INVALID_RESPONSIBLE_PERSON).target("in", Complaints_.class,
					Complaints_::personResponsible_ID);
			return false;
		}
		return true;
	}

	/**
	 * Validate master data deletion flag and Business partner type
	 * 
	 * @param complaint
	 */
	public void validateMasterDataDeletion(Complaints complaint) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_VALIDATION_IMPL, VALIDATE_MASTER_DATA_DELETION);
		if (null != complaint && null != complaint.getSupplierId()) {
			validateSupplierDetails(complaint);
		}
		LoggerHelper.logMethodEntry(logger, COMPLAINT_VALIDATION_IMPL, VALIDATE_MASTER_DATA_DELETION);
	}

	/**
	 * Validate supplier details for isMarkedForDeletion and business partner type
	 * 
	 * @param complaint
	 */
	public void validateSupplierDetails(Complaints complaint) {
		BusinessPartners supplierDetails = masterDataService.getSupplier(complaint.getSupplierId());
		if (null != supplierDetails) {
			if (Boolean.TRUE.equals(supplierDetails.getIsMarkedForDeletion())) {
				messages.error(MessageKeys.BUSINESS_PARTNER_MARKED_FOR_DELETION).target("in", Complaints_.class,
						Complaints_::supplier);
			} else if (!supplierDetails.getBusinessPartnerType()
					.equalsIgnoreCase(Constants.EXCHANGE_PARTNER_TYPE_SUPPLER)) {
				messages.error(MessageKeys.WRONG_SUPPLIER_SELECTED).target("in", Complaints_.class,
						Complaints_::supplier);
			}

		}
	}

	/**
	 * Validate Complaint description
	 * 
	 * @param {@link String}
	 *
	 * @private
	 */
	private void validateDescription(String description) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_VALIDATION_IMPL, "validateComplaintDescription");
		if (!securityValidator.isValidText(description)) {
			messages.error(MessageKeys.INVALID_COMPLAINT_DESCRIPTION).target("in", Complaints_.class,
					Complaints_::description);
		}
		LoggerHelper.logMethodExit(logger, COMPLAINT_VALIDATION_IMPL, "validateComplaintDescription");
	}

	/**
	 * Validate Complaint note
	 * 
	 * @param {@link String}
	 *
	 * @private
	 */
	private void validateNote(String note) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_VALIDATION_IMPL, "validateComplaintNote");
		if (!securityValidator.isValidText(note)) {
			messages.error(MessageKeys.INVALID_COMPLAINT_NOTE).target("in", Complaints_.class, Complaints_::note);
		}
		LoggerHelper.logMethodExit(logger, COMPLAINT_VALIDATION_IMPL, "validateComplaintNote");
	}

	/**
	 * Validate complaint editability based on complaint status
	 * 
	 */
	@Override
	public void validateComplaintAttributes(Complaints complaint) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_VALIDATION_IMPL, VALIDATE_COMPLAINT_ATTRIBUTES);
		String complaintStatusCode = complaint.getComplaintStatusCode();
		Complaints complaintFromDb = complaintService.getComplaintDetails(complaint.getId());
		if (null != complaintFromDb && StringUtils.isNotBlank(complaintStatusCode)) {
			if (Constants.COMPLAINT_IN_PROGRESS.equalsIgnoreCase(complaintStatusCode) ||
					Constants.COMPLAINT_REVISED.equalsIgnoreCase(complaintStatusCode)) {
				validateComplaintFieldControl(complaint, complaintFromDb);
			} else if (Constants.COMPLAINT_CLOSED.equalsIgnoreCase(complaintStatusCode)
					|| Constants.COMPLAINT_DISCARDED.equalsIgnoreCase(complaintStatusCode)) {
				messages.error(MessageKeys.ATTRIBUTES_NOT_EDITABLE).target("in", Complaints_.class, Complaints_::ID);
			}
		}
		LoggerHelper.logMethodExit(logger, COMPLAINT_VALIDATION_IMPL, VALIDATE_COMPLAINT_ATTRIBUTES);
	}

	// exposing company code in the payload???
	/**
	 * Validate field control when complaint status is In-Progress
	 * 
	 * @param complaint
	 * @param complaintFromDb
	 */
	public void validateComplaintFieldControl(Complaints complaint, Complaints complaintFromDb) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_VALIDATION_IMPL, VALIDATE_COMPLAINT_ATTRIBUTES);
		if (null != complaint.getSupplierId() && null != complaintFromDb.getSupplierId()
				&& (!complaintFromDb.getSupplierId().equalsIgnoreCase(complaint.getSupplierId()))) {
			messages.error(MessageKeys.SUPPLIER_NOT_EDITABLE).target("in", Complaints_.class, Complaints_::supplier_ID);
		} else if (null != complaint.getMaterialId() && null != complaintFromDb.getMaterialId()
				&& (!complaintFromDb.getMaterialId().equalsIgnoreCase(complaint.getMaterialId()))) {
			messages.error(MessageKeys.MATERIAL_NOT_EDITABLE).target("in", Complaints_.class, Complaints_::material_ID);
		} else if (null != complaint.getQuantity() && null != complaintFromDb.getQuantity()
				&& (complaintFromDb.getQuantity().compareTo(complaint.getQuantity()) != 0)) {
			messages.error(MessageKeys.QUANTITY_NOT_EDITABLE).target("in", Complaints_.class, Complaints_::quantity);
		} else if (null != complaint.getUnitCode() && null != complaintFromDb.getUnitCode()
				&& (!complaintFromDb.getUnitCode().equalsIgnoreCase(complaint.getUnitCode()))) {
			messages.error(MessageKeys.UNIT_NOT_EDITABLE).target("in", Complaints_.class, Complaints_::unit_code);
		} else if (null != complaint.getPlantId() && null != complaintFromDb.getPlantId()
				&& (!complaintFromDb.getPlantId().equalsIgnoreCase(complaint.getPlantId()))) {
			messages.error(MessageKeys.PLANT_NOT_EDITABLE).target("in", Complaints_.class, Complaints_::plant_ID);
		} else if (null != complaint.getPurchasingOrganizationId()
				&& null != complaintFromDb.getPurchasingOrganizationId() && (!complaintFromDb
						.getPurchasingOrganizationId().equalsIgnoreCase(complaint.getPurchasingOrganizationId()))) {
			messages.error(MessageKeys.PURCHASE_ORG_NOT_EDITABLE).target("in", Complaints_.class,
					Complaints_::purchasingOrganization_ID);
		} else if (null != complaint.getReferenceNumber() && null != complaintFromDb.getReferenceNumber()
				&& (!complaintFromDb.getReferenceNumber().equalsIgnoreCase(complaint.getReferenceNumber()))) {
			messages.error(MessageKeys.REF_NUMBER_NOT_EDITABLE).target("in", Complaints_.class,
					Complaints_::referenceNumber);
		}
		LoggerHelper.logMethodExit(logger, COMPLAINT_VALIDATION_IMPL, VALIDATE_COMPLAINT_ATTRIBUTES);
	}

	/**
	 * Validate Complaint reference number
	 * 
	 * @param {@link String}
	 *
	 * @private
	 */
	private void validateReferenceNumber(String referenceNumber) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_VALIDATION_IMPL, "validateReferenceNumber");
		if (!securityValidator.isValidText(referenceNumber)) {
			messages.error(MessageKeys.INVALID_COMPLAINT_REFERENCE_NUMBER).target("in", Complaints_.class,
					Complaints_::referenceNumber);
		}
		LoggerHelper.logMethodExit(logger, COMPLAINT_VALIDATION_IMPL, "validateReferenceNumber");
	}

	private void validateUnitOfMeasure(String unit) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_VALIDATION_IMPL, "validateUnitOfMeasure");
		if (unit != null && !masterDataService.getUnitOfMeasure(unit).first().isPresent()) {
			messages.error(MessageKeys.INVALID_UNIT).target("in", Complaints_.class,
					Complaints_::unit);
		}
		LoggerHelper.logMethodExit(logger, COMPLAINT_VALIDATION_IMPL, "validateUnitOfMeasure");
	}

}
