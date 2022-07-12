package com.sap.ic.cmh.claim.model;

import com.sap.cloud.sdk.result.ElementName;

public class ClaimDetails {
	
	@ElementName("CLAIM")
	private String claimNumber;
	@ElementName("PRO_STATE")
	private String claimStatus;
	public String getClaimNumber() {
		return claimNumber;
	}
	public void setClaimNumber(String claimNumber) {
		this.claimNumber = claimNumber;
	}
	public String getClaimStatus() {
		return claimStatus;
	}
	public void setClaimStatus(String claimStatus) {
		this.claimStatus = claimStatus;
	}
	
	
	

}
