package com.sap.ic.cmh.claim.handler;

import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import com.sap.cds.Result;
import com.sap.cds.services.cds.CdsReadEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.draft.DraftNewEventContext;
import com.sap.cds.services.draft.DraftService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.After;
import com.sap.cds.services.handler.annotations.Before;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.request.UserInfo;
import com.sap.cloud.sdk.cloudplatform.connectivity.ScpCfDestination;
import com.sap.cloud.sdk.cloudplatform.connectivity.ScpCfDestinationLoader;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.businessobjects.service.BusinessObjectService;
import com.sap.ic.cmh.claim.service.ClaimService;
import com.sap.ic.cmh.claim.validations.ClaimValidation;
import com.sap.ic.cmh.complaint.persistency.ComplaintsDao;
import com.sap.ic.cmh.complaint.service.ComplaintService;
import com.sap.ic.cmh.complaint.service.StreamService;
import com.sap.ic.cmh.configuration.persistency.DestinationConfigurationDao;
import com.sap.ic.cmh.configuration.service.ConfigurationService;
import com.sap.ic.cmh.costcollector.service.CostCollectorService;
import com.sap.ic.cmh.network.service.DestinationService;
import com.sap.ic.cmh.network.service.HttpService;
import com.sap.ic.cmh.utils.CommonFunctions;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.auditlog.AuditLogDifference;
import com.sap.ic.cmh.auditlog.ObjectDiff;
import cds.gen.claimservice.Claims;
import cds.gen.claimservice.Claims_;
import cds.gen.claimservice.Addresses_;
import cds.gen.claimservice.BusinessPartners_;
import cds.gen.complaintservice.Complaints;
import cds.gen.costcollectorservice.CostCollectors;

@Component
@ServiceName("ClaimService")
public class ClaimHandler implements EventHandler {

    @Autowired
    ConfigurationService configurationService;

    @Autowired
    StreamService streamService;

    @Autowired
    ClaimService claimService;

    @Autowired
    ComplaintsDao complaintDao;

    @Autowired
    BusinessObjectService businessObjectService;

    @Autowired
    ComplaintService complaintService;

    @Autowired
    HttpService httpService;

    @Autowired
    CostCollectorService costCollectorService;

    @Autowired
    private AuditLogHelper auditLogHelper;

    @Autowired
    Messages messages;

    @Autowired
    ClaimValidation claimValidation;

    @Autowired
    DestinationService destinationService;

    @Autowired
    DestinationConfigurationDao destinationConfigurationDao;

    @Autowired
    CommonFunctions commonFunctions;

    @Autowired
    private AuditLogDifference<Claims> auditLogDifference;

    private static final Logger LOG = LoggerFactory.getLogger(ClaimHandler.class);
    private static final String CLAIM_HANDLER = "ClaimHandler";

    private static final String BO_TYPE = Constants.CLAIM_CODE;

    private static final String ENTITY_NAME = "CLAIM";
    private static final boolean IS_ACTIVE = false;

    @Before(event = DraftService.EVENT_DRAFT_NEW)
    public void beforeClaimDraftNew(DraftNewEventContext context, Claims claim) {
        claim.setStatusCode(Constants.STATUS_NEW);
        if (claim.getComplaintId() != null) {
            Claims oldDraftClaims = claimService.getDraftClaimByComplaintID(claim.getComplaintId());
            if (oldDraftClaims != null) {
                claim.putAll(oldDraftClaims);
                claimService.deleteDraftClaimByID(oldDraftClaims.getId());
            }
        }
        beforeClaimDraftPatch(claim);
    }

    public void beforeClaimDraftPatch(Claims claim) {
        if (claim.getComplaintId() != null) {
            // set the master data details to the Claim from the complaint
            Complaints complaint = setMasterDataFromComplaints(claim);
            String complaintTypeCode = null != complaint ? complaint.getComplaintTypeCode() : "";
            claimService.setConfiguredValues(claim, BO_TYPE, complaintTypeCode);

            List<CostCollectors> cost = costCollectorService.selectTransferToClaimCostCollector(claim.getComplaintId());
            if (cost != null) {
                for (CostCollectors costcoll : cost) {
                    costcoll.setClaim(claim.getId());
                    costCollectorService.updateCostCollector(costcoll);
                }
            }
        }
    }

    @Before(event = { CdsService.EVENT_CREATE }, entity = Claims_.CDS_NAME)
    public void beforeCreateClaim(Claims claim) {
        LOG.info("Claims Payload");
        beforeClaimDraftPatch(claim);
        LOG.info(ENTITY_NAME, "beforeClaimCreate");
        complaintService.validateComplaintStatus(claim.getComplaintId());
        claimService.validateIfClaimExistsForComplaint(claim.getComplaintId());
        claimService.validateClaimFields(claim);
        messages.throwIfError();
    }

    @On(event = CdsService.EVENT_CREATE, entity = Claims_.CDS_NAME)
    public void onCreateClaim(Claims claim) {
        claim.setIdentifier(claimService.createClaimAtDestination(claim));

        String backendStatusCode = claim.getStatusCode();
        LOG.info("Backend status code from Target system is Recieved");
        // update business object with the Claim ID
        businessObjectService.updateBusinessObjects(claim, BO_TYPE);

        setBusinessObjectStatus(claim, backendStatusCode, IS_ACTIVE);
        LOG.info("Claim created successfully");
        // Update complaint status to INPR
        complaintService.updateComplaintStatus(claim.getComplaintId(), Constants.COMPLAINT_IN_PROGRESS);
    }

    @After(event = { CdsService.EVENT_CREATE, CdsService.EVENT_UPDATE }, entity = Claims_.CDS_NAME)
    public void afterCreateUpdateClaim(Claims claim) {
        updateStreamStatus(claim, BO_TYPE, IS_ACTIVE);
        LOG.info("Creation of Claim Business Object is completed successfully.");
        logUpsert(claim);
    }

    public void updateStreamStatus(Claims claim, String boType, boolean isActive) {
        streamService.updateStreamStatus(claim.getId(), boType, isActive);
    }

    @Before(event = { CdsService.EVENT_UPDATE }, entity = Claims_.CDS_NAME)
    public void beforeUpdateClaim(Claims claim) {
        LOG.info(ENTITY_NAME, "beforeClaimUpdate");
        claimService.validateIfClaimExists(claim);
        messages.throwIfError();
        setOldAuditData(claim);
    }

    public void setBusinessObjectStatus(Claims claim, String backendStatusCode, boolean isActive) {
        // update BO Status
        businessObjectService.setBusinessObjectStatus(BO_TYPE, backendStatusCode, claim.getId(), isActive);
        /*
         * get the current BO status and set it to the status attribute to display the
         * latest BO status in UI
         */
        String currentStatus = businessObjectService.getCurrentBOStatus(claim.getId());
        claim.setStatusCode(currentStatus);
    }

    @Before(event = CdsService.EVENT_READ, entity = Addresses_.CDS_NAME)
    public void beforeClaimReadAddress(CdsReadEventContext context) {
        LoggerHelper.logMethodEntry(LOG, CLAIM_HANDLER, "beforeClaimReadAddress");
        LOG.info("Before Read of Address in claim");
        commonFunctions.checkBeforeRead(context);
        LoggerHelper.logMethodExit(LOG, CLAIM_HANDLER, "beforeClaimReadAddress");
    }

    @Before(event = CdsService.EVENT_READ, entity = BusinessPartners_.CDS_NAME)
    public void beforeClaimReadBusinessPartner(CdsReadEventContext context) {
        LoggerHelper.logMethodEntry(LOG, CLAIM_HANDLER, "beforeClaimReadBusinessPartner");
        LOG.info("Before Read of Business Partners in claim");
        commonFunctions.checkBeforeRead(context);
        LoggerHelper.logMethodExit(LOG, CLAIM_HANDLER, "beforeClaimReadBusinessPartner");
    }

    /**
     * Hiding button based on the role
     *
     * @param data
     */
    @After(event = CdsService.EVENT_READ, entity = Claims_.CDS_NAME)
    public void updateButtonVisibility(CdsReadEventContext context, List<Claims> data) {
        UserInfo user = context.getUserInfo();
        String companyCode = "";
        boolean hasClaimUpdateRole = user.hasRole("Claim.Update");
        ScpCfDestinationLoader scpCfDestinationLoader = new ScpCfDestinationLoader();
        Iterator<ScpCfDestination> readAllDestinations = destinationService.readAllDestination(scpCfDestinationLoader);
        for (Claims claims : data) {
            claims.setNumber(claims.getIdentifier());
            claims.setIdentifier(
                    claims.getIdentifier() != null ? claims.getIdentifier().replaceFirst("^0+(?!$)", "") : "");
            claims.setIsUpdateRestricted(!hasClaimUpdateRole);
            if (null != claims.getCompanyId()) {
                companyCode = claims.getCompanyId();
            }
            Claims claimBasedOnId = claimService.getClaimBasedOnId(claims.getId());
            if (null != claimBasedOnId && StringUtils.isNotBlank(claimBasedOnId.getStatusCode())) {
                if (!claimBasedOnId.getStatusCode().equalsIgnoreCase(Constants.STATUS_NEW)) {
                    LOG.info("Claim From DB {}", claimBasedOnId.getStatusCode());
                    claims.setIsClaimFieldControl(Constants.FIELD_CONTROL_READ_ONLY);
                    LOG.info("Claim in CRTD status ::: {} ", claims.getIsClaimFieldControl());
                }
                if (claimBasedOnId.getStatusCode().equalsIgnoreCase(Constants.CLAIM_STATUS_CLOSED)
                        && Boolean.FALSE.equals(claims.getIsUpdateRestricted())) {
                    claims.setIsUpdateRestricted(hasClaimUpdateRole);
                }
                companyCode = claimBasedOnId.getCompanyId();
            }

            LOG.info("Company code is recevied");
            setNavigationURL(companyCode, readAllDestinations, claims);
        }
    }

    public void setNavigationURL(String companyCode, Iterator<ScpCfDestination> readAllDestinations, Claims claims) {
        /* START of Set Navigation URL */
        Result result = destinationConfigurationDao.getDestinationConfigBasedOnCompanyAndBOType(companyCode,
                Constants.CLAIM_CODE);
        Object destinationObj = result.first().isPresent() ? result.list().get(0).get("navigationDestination") : null;
        String destination = null != destinationObj ? destinationObj.toString() : "";
        while (readAllDestinations.hasNext()) {
            ScpCfDestination getDestination = readAllDestinations.next();
            if (null != destination && getDestination.getName().equals(destination) && getDestination.isHttp()) {
                claims.setSNavigation(getDestination.getName());
            }
        }
        /* END of Set Navigation URL */
    }

    /**
     * Set the master data details from the complaint
     * 
     * @param claim
     */
    public Complaints setMasterDataFromComplaints(Claims claim) {
        Complaints complaint = complaintService.getMasterDataFromComplaints(claim.getComplaintId());
        if (null != complaint) {
            claim.setMaterialId(complaint.getMaterialId());
            claim.setPlantId(complaint.getPlantId());
            claim.setSupplierId(complaint.getSupplierId());
            claim.setPurchasingOrganizationId(complaint.getPurchasingOrganizationId());
            claim.setQuantity(complaint.getQuantity());
            claim.setUnit(complaint.getUnitCode());
            claim.setCompanyId(complaint.getCompanyCodeId());
        }
        return complaint;
    }

    public void setOldAuditData(Claims claim) {
        Claims oldData = claimService.getClaimBasedOnId(claim.getId());
        auditLogDifference.setOldData(oldData);
    }

    public void logUpsert(Claims claim) {
        Claims newData = claimService.getClaimBasedOnId(claim.getId());
        List<ObjectDiff> diffList = auditLogDifference.getDifference(newData);
        Map<String, String> entityInfoMap = auditLogHelper.buildEntityInfoMap(
                ENTITY_NAME,
                newData.getId());
        auditLogHelper.logUpsertAuditData(diffList, entityInfoMap);
    }

}