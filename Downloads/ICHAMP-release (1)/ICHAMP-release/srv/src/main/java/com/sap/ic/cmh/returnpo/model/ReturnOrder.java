package com.sap.ic.cmh.returnpo.model;

import java.util.Map;

import com.sap.ic.cmh.claim.model.binary_relation.BinaryRelationDataModel;

public class ReturnOrder {
	private Map<String, Object> returnOrders;
	private BinaryRelationDataModel binaryRelationDataModel;

	public BinaryRelationDataModel getBinaryRelationDataModel() {
		return binaryRelationDataModel;
	}

	public void setBinaryRelationDataModel(BinaryRelationDataModel binaryRelationDataModel) {
		this.binaryRelationDataModel = binaryRelationDataModel;
	}

	public Map<String, Object> getReturnOrders() {
		return returnOrders;
	}

	public void setReturnOrders(Map<String, Object> returnOrders) {
		this.returnOrders = returnOrders;
	}

}