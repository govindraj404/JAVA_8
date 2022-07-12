package com.sap.ic.cmh.claim.model;

import java.util.List;
import java.util.Map;
import com.sap.ic.cmh.claim.model.binary_relation.BinaryRelationDataModel;


public class ClaimDTO {

    private Map<String,Object> claims;
    private List<ClaimItem> claimItem = null;
    private List<ClaimPricing> claimPricing = null;
    private BinaryRelationDataModel binaryRelationDataModel;


    public List<ClaimItem> getClaimItem() {
        return claimItem;
    }

    public void setClaimItem(List<ClaimItem> claimItem) {
        this.claimItem = claimItem;
    }

    public List<ClaimPricing> getClaimPricing() {
        return claimPricing;
    }

    public void setClaimPricing(List<ClaimPricing> claimPricing) {
        this.claimPricing = claimPricing;
    }

	public BinaryRelationDataModel getBinaryRelationDataModel() {
		return binaryRelationDataModel;
	}

	public void setBinaryRelationDataModel(BinaryRelationDataModel binaryRelationDataModel) {
		this.binaryRelationDataModel = binaryRelationDataModel;
	}

    public Map<String, Object> getClaims() {
		return claims;
	}

	public void setClaims(Map<String, Object> claims) {
		this.claims = claims;
	}

}