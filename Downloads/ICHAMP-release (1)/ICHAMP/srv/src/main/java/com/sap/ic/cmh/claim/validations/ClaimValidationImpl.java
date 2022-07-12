package com.sap.ic.cmh.claim.validations;

import cds.gen.claimservice.Claims;
import cds.gen.claimservice.Claims_;
import cds.gen.claimservice.ItemTypes_;
import cds.gen.masterdataservice.ActionPreconditions;
import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.qualitynotificationservice.QualityNotifications;

import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.action.persistency.ActionDao;
import com.sap.ic.cmh.businessobjects.service.BusinessObjectService;
import com.sap.ic.cmh.claim.service.ClaimService;
import com.sap.ic.cmh.configuration.persistency.ConfigurationDao;
import com.sap.ic.cmh.configuration.validations.MasterDataValidation;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.qualitynotification.service.QualityNotificationService;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.SecurityValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ClaimValidationImpl implements ClaimValidation {

	@Autowired
	Messages messages;

	@Autowired
	SecurityValidator securityValidator;

	@Autowired
	ClaimService claimService;

	@Autowired
	MasterDataValidation masterDataValidation;

	@Autowired
	PersistenceService db;
	@Autowired
	BusinessObjectService businessObjectService;
	@Autowired
    QualityNotificationService qualityNotificationService;
    @Autowired
    ActionDao actionDao;
	@Autowired
	ConfigurationDao configurationDao;

	public static final Logger logger = LoggerFactory.getLogger(ClaimValidationImpl.class);

	/**
	 * validation for Claim Attributes & raise error message for any invalid or null
	 * attribute
	 *
	 * @param {@link String}
	 *
	 * @public
	 */
	@Override
	public void validateClaimFields(Claims claim) {
		validateItemType(claim.getItemTypeCode());
		validateClaimType(claim.getClaimType());
		validateSupplierRole(claim.getSupplierRole());
		validateVersionCategory(claim.getVersionCategory());
		validatePersonResponsible(claim.getPersonResponsibleId());
		masterDataValidation.validateSupplierPersonType(claim.getContactPersonId());
	}


	/**
	 * Validate if Item type is configured
	 * 
	 * @param itemType
	 * @public
	 */
	public void validateItemType(String itemType) {
		if (itemType == null || itemType.isEmpty()) {
			messages.error(MessageKeys.ITEM_TYPE_NOT_CONFIGURED).target("in", Claims_.class,
					Claims_::itemType_code);
		}
		validateItemTypeIfExist(itemType);

	}

	private void validateItemTypeIfExist(String itemType) {
		if (itemType != null && !itemType.isEmpty()) {
			if (!securityValidator.isValidText(itemType)) {
				messages.error(MessageKeys.INVALID_ITEM_TYPE_FOR_CLAIM).target("in", Claims_.class,
						Claims_::itemType);
			} else {
				Result itemTypes = db.run(Select.from(ItemTypes_.class).where(b -> b.code().eq(itemType)));
				if (itemTypes == null) {
					messages.error(MessageKeys.ITEM_TYPE_DO_NOT_EXIST).target("in", ItemTypes_.class, ItemTypes_::code);
				}
			}
		}

	}

	/**
	 * Validate if Claim type is not null
	 * 
	 * @param claimType
	 * @public
	 */
	public void validateClaimType(String claimType) {
		if (claimType == null || claimType.isEmpty()) {
			messages.error(MessageKeys.CLAIM_TYPE_NOT_CONFIGURED).target("in", Claims_.class,
					Claims_::claimType);
		}
		validateClaimTypeIfExist(claimType);
	}

	private void validateClaimTypeIfExist(String claimType) {
		if (claimType != null && !claimType.isEmpty() && !securityValidator.isValidText(claimType)) {
			messages.error(MessageKeys.INVALID_TYPE_FOR_CLAIM).target("in", Claims_.class, Claims_::claimType);
		}
	}

	/**
	 * Validate if Supplier Role is configured and exist
	 * 
	 * @param supplierRole
	 * @public
	 */
	public void validateSupplierRole(String supplierRole) {
		if (supplierRole == null || supplierRole.isEmpty()) {
			messages.error(MessageKeys.INVALID_SUPPLIER_ROLE_FOR_CLAIM).target("in", Claims_.class,
					Claims_::supplierRole);
		}
		validateSupplierRoleIfExist(supplierRole);
	}

	private void validateSupplierRoleIfExist(String supplierRole) {
		if (supplierRole != null && !supplierRole.isEmpty() && !securityValidator.isValidText(supplierRole)) {
			messages.error(MessageKeys.INVALID_SUPPLIER_ROLE_FOR_CLAIM).target("in", Claims_.class,
					Claims_::supplierRole);
		}
	}

	/**
	 * Validate if Version Category is configured and exist
	 * 
	 * @param versionCategory
	 * @public
	 */
	public void validateVersionCategory(String versionCategory) {
		if (versionCategory == null || versionCategory.isEmpty()) {
			messages.error(MessageKeys.VERSION_CATEGORY_NOT_CONFIGURED).target("in", Claims_.class,
					Claims_::versionCategory);
		}
		validateVersionCategoryIfExist(versionCategory);
	}

	private void validateVersionCategoryIfExist(String versionCategory) {
		if (versionCategory != null && !versionCategory.isEmpty() && !securityValidator.isValidText(versionCategory)) {
			messages.error(MessageKeys.INVALID_VERSION_CATEGORY_FOR_CLAIM).target("in", Claims_.class,
					Claims_::versionCategory);
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
				messages.error(MessageKeys.PERS_RESP_IS_NOT_VALID).target("in", Claims_.class,
						Claims_::personResponsible);
			}else if(Boolean.TRUE.equals(supplierData.getIsMarkedForDeletion())) {
				messages.error(MessageKeys.PERSON_RESP_MARKED_FOR_DELETION).target("in", Claims_.class,
						Claims_::personResponsible);
			}else if(!supplierData.getBusinessPartnerType().equalsIgnoreCase(Constants.EXCHANGE_PARTNER_TYPE_PERSON_RESPONSIBLE)) {
				messages.error(MessageKeys.WRONG_PERSON_RESP_SELECTED).target("in", Claims_.class,
						Claims_::personResponsible);
			}
		}
	}

	/**
	 * Validate Field control and return error message if
	 * 
	 * @param claim
	 */
	@Override
	public void validateFieldControlClaim(Claims claim) {
		String claimStatus = claim.getStatusCode();
		Claims claimDetails = claimService.getClaimBasedOnId(claim.getId());
		if (null != claimDetails) {
			if (Constants.CLAIM_STATUS_CLOSED.equalsIgnoreCase(claimStatus)) {
				messages.error(MessageKeys.ATTRIBUTES_NOT_EDITABLE).target("in", Claims_.class, Claims_::ID);
			} else if(!Constants.STATUS_NEW.equalsIgnoreCase(claimStatus)) {
                logger.info("Claim status is other than NEW/Closed");
				validateClaimProgress(claim, claimDetails);
			}
		}

	}
    
    /**
     * Validate Field control for claim
     * when status is other than New/Closed
     */
	public void validateClaimProgress(Claims claim, Claims claimInDb) {
		if (null!=claim.getClaimType() && (!claimInDb.getClaimType().equalsIgnoreCase(claim.getClaimType()))) {
			messages.error(MessageKeys.NOTIFICATION_TYPE_NOT_EDITABLE).target("in", Claims_.class, Claims_::claimType);
		} else if (null!=claim.getPersonResponsibleId() && null!=claimInDb.getPersonResponsibleId() &&(!claimInDb.getPersonResponsibleId()
				.equalsIgnoreCase(claim.getPersonResponsibleId()))) {
			messages.error(MessageKeys.PERSON_RESPONISBLE_NOT_EDITABLE).target("in", Claims_.class,
					Claims_::personResponsible);
		} else if (null!=claim.getVersionCategory()&& (!claimInDb.getVersionCategory().equalsIgnoreCase(claim.getVersionCategory()))) {
			messages.error(MessageKeys.VERSION_CATEGORY_NOT_EDITABLE).target("in", Claims_.class,
					Claims_::versionCategory);
		} else if (null!=claim.getSupplierRole() && (!claimInDb.getSupplierRole().equalsIgnoreCase(claim.getSupplierRole()))) {
			messages.error(MessageKeys.SUPPLIER_ROLE_NOT_EDITABLE).target("in", Claims_.class, Claims_::supplierRole);
		} else if (null!=claim.getItemTypeCode() && (!claimInDb.getItemTypeCode().equalsIgnoreCase(claim.getItemTypeCode()))) {
			messages.error(MessageKeys.ITEM_TYPE_NOT_EDITABLE).target("in", Claims_.class, Claims_::itemType_code);
		}

	}

	/**
	 * Validate if Claim exists for a complaint ID
	 */
	@Override
	public void validateIfClaimExistsForComplaint(String complaintId) {
		Claims claimsBasedOnComplaintId = claimService.getClaim(complaintId);
		if (null != claimsBasedOnComplaintId) {
			messages.error(MessageKeys.CLAIM_EXISTS).target("in", Claims_.class, c -> c.ID());
		}

	}

	/**
	 * Validate if claim is relevant for the complaint
	 */
	@Override
	public void validateIfBOIsRelevant(String complaintId, String claimCode) {
		boolean checkIfBOIsRelevant = businessObjectService.checkIfBOIsRelevant(complaintId, claimCode);
		if (!checkIfBOIsRelevant) {
			messages.error(MessageKeys.CLAIM_NOT_RELEVANT).target("in", Claims_.class,
					Claims_::ID);
		}

	}

	/**
	 * Validate if Claim exists for a claim ID
	 */
	@Override
	public void validateIfClaimExists(String claimId) {
		Claims claimBasedOnId = claimService.getClaimBasedOnId(claimId);
		if(null==claimBasedOnId) {
			messages.error(MessageKeys.CLAIM_NOT_EXIST).target("in", Claims_.class, Claims_::ID);
		}
	}
    /**
     * Validate if source Business Object is created
     * and validate the action pre condition
     */
	@Override
	public void validateIfQualityNotificationExists(String complaintId,String boType) {
		QualityNotifications qualityNotificationDetailsByComplaintId = qualityNotificationService.getQualityNotificationDetailsByComplaintId(complaintId);
		if(null==qualityNotificationDetailsByComplaintId) {
			messages.error(MessageKeys.QUALITY_NOTIFICATION_TO_CREATE).target("in", Claims_.class, Claims_::ID);
		}else{
         validateActionPreCondition(qualityNotificationDetailsByComplaintId.getId(),boType);
        }
    }
    /**
     * Validate Action pre condition for the Business Objects
     */
    public void validateActionPreCondition(String qualityNotificationId,String boType){
       Result result = actionDao.getActions(boType);
          if(null!=result && result.first().isPresent()){
            String actionPreConditionCode =  result.list().get(0).get(ActionPreconditions.BUSINESS_OBJECT_STATUS_CODE).toString();
            logger.info("Action Pre condition for BO is received.");
            if(!businessObjectService.checkIfBOStatusExists(qualityNotificationId, actionPreConditionCode)){
                 messages.error(MessageKeys.ACTION_PRE_CONDITION_NOT_SATISFIED).target("in", Claims_.class, Claims_::ID);
            }
          }
    }

}
