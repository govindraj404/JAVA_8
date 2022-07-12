package com.sap.ic.cmh.claim.service;

import cds.gen.claimservice.Claims;

public interface ClaimService {

    Claims getClaim(String complaintID);

    Claims getDraftClaimByComplaintID(String complaintId);

    void deleteDraftClaimByID(String claimId);

    String createClaimAtDestination(Claims claim);

    void setConfiguredValues(Claims claim, String boType, String complaintTypeCode);

    Claims getClaimBasedOnId(String claimID);

    void validateClaimFields(Claims claim);

    void validateIfClaimExistsForComplaint(String complaintId);

    void validateIfClaimExists(Claims claim);

    Claims getClaimStatusAndCompanyCode(String claimID);
    
    cds.gen.manageclaimservice.Claims getActiveClaimBasedOnId(String claimId);

    String checkIfClaimExistsBasedOnNumber(String claimNumber);

}
