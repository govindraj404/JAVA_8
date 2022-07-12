package com.sap.ic.cmh.manageclaim.handler;

import java.util.UUID;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import com.sap.ic.cmh.auditlog.ObjectDiff;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.services.ServiceException;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.After;
import com.sap.cds.services.handler.annotations.Before;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.ic.cmh.claim.handler.ClaimHandler;
import com.sap.ic.cmh.claim.service.ClaimService;
import com.sap.ic.cmh.complaint.persistency.ComplaintsDao;
import com.sap.ic.cmh.configuration.service.ConfigurationService;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.CommonFunctions;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.auditlog.AuditLogDifference;
import cds.gen.manageclaimservice.Claims;
import cds.gen.manageclaimservice.Claims_;
import cds.gen.masterdataservice.BusinessPartners;
import com.sap.ic.cmh.configuration.service.MessageService;



@Component
@ServiceName("ManageClaimService")
public class ManageClaimHandler implements EventHandler {

	@Autowired
	ClaimHandler claimHandler;
	@Autowired
	CommonFunctions commonFunctions;

    @Autowired
	AuditLogHelper auditLogHelper;
	@Autowired
	ComplaintsDao complaintDao;

	@Autowired
    ConfigurationService configurationService;

    @Autowired
	MessageService messageService;
    @Autowired
    ClaimService claimService;

    @Autowired
	private AuditLogDifference<cds.gen.claimservice.Claims> auditLogDifference;

    private static final String ENTITY_NAME = "MANAGE_CLAIMS";
	private static final Logger logger = LoggerFactory.getLogger(ManageClaimHandler.class);
	private static final String MANAGE_CLAIM_HANDLER = "ManageClaimHandler";
	private static final boolean IS_ACTIVE = true;
	private static volatile String claimId;

	/**
	 * This method is used to call the BEFORE-CREATE event of draft Claim Handler
	 * validate all mandatory fields to create claim
	 * @param manageClaims
	 */
	@Before(event = { CdsService.EVENT_CREATE }, entity = Claims_.CDS_NAME)
	public void beforeManageClaimCreate(Claims manageClaims) {
		LoggerHelper.logMethodEntry(logger, MANAGE_CLAIM_HANDLER, "beforeManageClaimCreate");
		setMasterDataDetails(manageClaims);
		cds.gen.claimservice.Claims claims = commonFunctions.convertManageClaimsToClaims(manageClaims);
		claimId = UUID.randomUUID().toString();
		claims.setId(claimId);
		claims.setStatusCode(Constants.STATUS_NEW);
		if (StringUtils.isNotBlank(manageClaims.getComplaintCode())) {
			Result comp = complaintDao.getComplaintBasedOnCode(manageClaims.getComplaintCode());
			if(comp.first().isPresent()){
                claims.setComplaintId(comp.first().get().get("ID").toString());
                manageClaims.setComplaintId(comp.first().get().get("ID").toString());
			}
		} else {
			throw new ServiceException(MessageKeys.COMPLAINT_ASSOCIATION_TO_CLAIM);
		}
		claimHandler.beforeCreateClaim(claims);
		manageClaims.setClaimType(claims.getClaimType());
		manageClaims.setSupplierRole(claims.getSupplierRole());
		manageClaims.setVersionCategory(claims.getVersionCategory());
		manageClaims.setItemTypeCode(claims.getItemTypeCode());
		manageClaims.setMaterialId(claims.getMaterialId());
		manageClaims.setSupplierId(claims.getSupplierId());
		manageClaims.setPlantId(claims.getPlantId());
		manageClaims.setPurchasingOrganizationId(claims.getPurchasingOrganizationId());
		manageClaims.setCompanyId(claims.getCompanyId());
		manageClaims.setQuantity(claims.getQuantity());
		manageClaims.setUnit(claims.getUnit());
		LoggerHelper.logMethodExit(logger, MANAGE_CLAIM_HANDLER, "beforeManageClaimCreate");
	}

	/**
	 * This method is used to call the ON-CREATE event of draft Claim Handler
	 * Create Claim in the configured destination
	 * Link the Business object with complaint
	 * Insert business object status
	 * @param manageClaims
	 */
	@On(event = CdsService.EVENT_CREATE, entity = Claims_.CDS_NAME)
	public void onCreateClaim(Claims manageClaims) {
		LoggerHelper.logMethodEntry(logger, MANAGE_CLAIM_HANDLER, "onCreateClaim");
		cds.gen.claimservice.Claims claims = commonFunctions.convertManageClaimsToClaims(manageClaims);
		claims.setId(claimId);
		claimHandler.onCreateClaim(claims);
		manageClaims.setIdentifier(claims.getIdentifier());
		manageClaims.setStatusCode(claims.getStatusCode());
		manageClaims.setBusinessObjectStatuses(claims.getBusinessObjectStatuses());
		manageClaims.setId(claims.getId());
		LoggerHelper.logMethodExit(logger, MANAGE_CLAIM_HANDLER, "onCreateClaim");
	}


	/**
	 * This method is used to call the AFTER-CREATE and AFTER-UPDATE event of draft Claim Handler
	 * update Stream status according to the business object status
	 * @param manageClaims
	 */
	@After(event = {CdsService.EVENT_CREATE}, entity = Claims_.CDS_NAME)
	public void afterManageClaimCreateUpdate(Claims manageClaims) {
		LoggerHelper.logMethodEntry(logger, MANAGE_CLAIM_HANDLER, "afterManageClaimCreateUpdate");
		cds.gen.claimservice.Claims claims = commonFunctions.convertManageClaimsToClaims(manageClaims);
		claimHandler.updateStreamStatus(claims, Constants.CLAIM_CODE, IS_ACTIVE);
        logUpsert(manageClaims);
		LoggerHelper.logMethodExit(logger, MANAGE_CLAIM_HANDLER, "afterManageClaimCreateUpdate");
	}

	/**
	 * This method is used to set the master data details
	 * @param manageClaims
	 */
	private void setMasterDataDetails(Claims manageClaims){
		BusinessPartners supplierPerson = configurationService.validateSupplierContactPerson(manageClaims.getContactPersonCode());
		if(supplierPerson!=null){
			manageClaims.setContactPersonId(supplierPerson.getId());
		}
		BusinessPartners personResponsible = configurationService.validatePersonResponsibleCode(manageClaims.getPersonResponsibleCode());
		if(personResponsible!=null){
			manageClaims.setPersonResponsibleId(personResponsible.getId());
		}
    }
	
	
	@Before(event = {CdsService.EVENT_UPDATE}, entity = Claims_.CDS_NAME)
	public void beforeManageClaimStatusUpdate(Claims manageClaims) {
        LoggerHelper.logMethodEntry(logger, MANAGE_CLAIM_HANDLER, "beforeManageClaimStatusUpdate");
        //create a struct and copy the payload
        Claims claimsToBeUpdated = Struct.create(Claims.class);
        claimsToBeUpdated.putAll(manageClaims);
		if(StringUtils.isNotBlank(manageClaims.getId())) {
			Claims activeClaimBasedOnId = claimService.getActiveClaimBasedOnId(manageClaims.getId());
			String statusCode = manageClaims.getStatusCode();
            //copy the DB details and override the payload
			manageClaims.putAll(activeClaimBasedOnId);
             //override the claim payload
            manageClaims.putAll(claimsToBeUpdated);
			manageClaims.setStatusCode(statusCode);
            setOldAuditData(manageClaims);
			logger.info("Status Code :: {} ", statusCode);
		}
        LoggerHelper.logMethodExit(logger, MANAGE_CLAIM_HANDLER, "beforeManageClaimStatusUpdate");
	}

    @On(event = {CdsService.EVENT_UPDATE}, entity = Claims_.CDS_NAME)
	public void onManageClaimStatusUpdate(Claims manageClaims) {
        LoggerHelper.logMethodEntry(logger, MANAGE_CLAIM_HANDLER, "onManageClaimStatusUpdate");
        logger.info("inside manage claim status update handler");
        logger.info("claim number:{}, claim status:{}",manageClaims.getIdentifier(),manageClaims.getStatusCode());
            String manageClaimId = messageService.checkIfBOExists(manageClaims.getIdentifier(),Constants.CLAIM_CODE);
            if(manageClaimId==null){
                logger.error("[ManageClaimHandler][onAutomaticReturnOrderStatusUpdate] Claim does not exist, cannot update");
                throw new ServiceException(MessageKeys.CLAIM_NOT_EXIST);
            }
                cds.gen.claimservice.Claims claims = commonFunctions.convertManageClaimsToClaims(manageClaims);
                claimHandler.setBusinessObjectStatus(claims, manageClaims.getStatusCode(), IS_ACTIVE);
                manageClaims.setStatusCode(claims.getStatusCode());
                claimHandler.updateStreamStatus(claims, Constants.CLAIM_CODE, IS_ACTIVE);
        LoggerHelper.logMethodExit(logger, MANAGE_CLAIM_HANDLER, "onManageClaimStatusUpdate");
	}

    @After(event = {CdsService.EVENT_UPDATE}, entity = Claims_.CDS_NAME)
	public void afterManageClaimsUpdate(Claims manageClaims) {
        LoggerHelper.logMethodEntry(logger, MANAGE_CLAIM_HANDLER, "afterManageClaimsUpdate");
        logUpsert(manageClaims);
        LoggerHelper.logMethodExit(logger, MANAGE_CLAIM_HANDLER, "afterManageClaimsUpdate");
    }

    public void setOldAuditData(Claims claims) {
		cds.gen.claimservice.Claims oldData = claimService.getClaimBasedOnId(claims.getId());
		auditLogDifference.setOldData(oldData);
	}

	public void logUpsert(Claims claims) {
		cds.gen.claimservice.Claims newData = claimService.getClaimBasedOnId(claims.getId());
		List<ObjectDiff> diffList = auditLogDifference.getDifference(newData);
		Map<String, String> entityInfoMap = auditLogHelper.buildEntityInfoMap(
				ENTITY_NAME,
				newData.getId());
		auditLogHelper.logUpsertAuditData(diffList, entityInfoMap);
	}
}
