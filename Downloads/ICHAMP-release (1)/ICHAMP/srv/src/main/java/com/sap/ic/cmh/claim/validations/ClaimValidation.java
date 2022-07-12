package com.sap.ic.cmh.claim.validations;

import cds.gen.claimservice.Claims;

public interface ClaimValidation {

    void validateClaimFields(Claims claim);
    void validateFieldControlClaim(Claims claim);
	void validateIfClaimExistsForComplaint(String complaintId);
	void validateIfBOIsRelevant(String complaintId, String claimCode);
    void validateIfQualityNotificationExists(String complaintId,String boType);
    void validateIfClaimExists(String claimId);
}
