package com.sap.ic.cmh.claim.persistency;

import org.springframework.stereotype.Repository;
import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.ql.Delete;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import cds.gen.claimservice.Claims;
import cds.gen.claimservice.Claims_;
import com.sap.cds.services.draft.DraftService;
import org.springframework.beans.factory.annotation.Qualifier;

@Repository
public class ClaimDao {

    private DraftService draftService;

    PersistenceService db;

    ClaimDao(@Qualifier("ClaimService") DraftService draftService, PersistenceService db) {
        this.draftService = draftService;
        this.db = db;
    }

    /**
     * Read claim
     * 
     * @param claimId
     * @return Result
     */
    public Result getClaimBasedOnId(String claimId) {
        CqnSelect val = Select.from(Claims_.class).where(b -> b.get(Claims.ID).eq(claimId));
        return db.run(val);
    }

    /**
     * Read claim based on Complaint ID
     * 
     * @param complaintID
     * @return Claims
     */
    public Result getClaim(String complaintID) {
        CqnSelect select = Select.from(Claims_.class).columns(Claims.ID)
                .where(b -> b.get(Claims.COMPLAINT_ID).eq(complaintID));
        return db.run(select);
    }

    /**
     * Check if Claim number exists in DB
     */
    public Result checkIfClaimExistsBasedOnNumber(String claimNumber) {
        CqnSelect select = Select.from(Claims_.class).columns(Claims.ID)
                .where(claim -> claim.identifier().eq(claimNumber));
        return db.run(select);
    }

    /**
     * Get status code and company code based on Claim ID
     */
    public Result getClaimStatusAndCompanyCode(String claimId) {
        CqnSelect val = Select.from(Claims_.class).columns(Claims.STATUS_CODE, Claims.COMPANY_ID)
                .where(b -> b.get(Claims.ID).eq(claimId));
        return db.run(val);
    }

    /**
     * Read draft claim based on Complaint ID
     * 
     * @param complaintId
     * @return Claims
     */
    public Result getDraftClaimByComplaintID(String complaintId) {
        return draftService.run(Select.from(Claims_.class)
                .where(b -> b.get(Claims.COMPLAINT_ID).eq(complaintId).and(b.IsActiveEntity().eq(false))));
    }

    /**
     * Delete draft claim based on claim ID
     * 
     * @param claimId
     */

    public void deleteDraftClaimByID(String claimId) {
        draftService.cancelDraft(
                Delete.from(Claims_.class).where(b -> b.ID().eq(claimId)));
    }
    
    /**
     * Get details of Active Claim based on ID
     * 
     * @param claimId
     * @return Result
     */
    public Result getActiveClaimBasedOnId(String claimId) {
        CqnSelect val = Select.from(cds.gen.manageclaimservice.Claims_.class).where(b -> b.get(Claims.ID).eq(claimId));
        return db.run(val);
    }
}
