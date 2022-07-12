package com.sap.ic.cmh.returnpo.model;

import java.util.List;

public class ReturnPurchaseOrderRequestPayload {
    	
	private String rpoNumber;
	private String plantNumber;
	private String rpoBackendStatus;
	private List<ReturnOrderBackendStatus> rpoBackendStatusList;
	
	public String getRpoNumber() {
		return rpoNumber;
	}
	public void setRpoNumber(String rpoNumber) {
		this.rpoNumber = rpoNumber;
	}
	public String getPlantNumber() {
		return plantNumber;
	}
	public void setPlantNumber(String plantNumber) {
		this.plantNumber = plantNumber;
	}
	public String getRpoBackendStatus() {
		return rpoBackendStatus;
	}
	public void setRpoBackendStatus(String rpoBackendStatus) {
		this.rpoBackendStatus = rpoBackendStatus;
	}
	public List<ReturnOrderBackendStatus> getRpoBackendStatusList() {
		return rpoBackendStatusList;
	}
	public void setRpoBackendStatusList(List<ReturnOrderBackendStatus> rpoBackendStatusList) {
		this.rpoBackendStatusList = rpoBackendStatusList;
	}
	@Override
	public String toString() {
		return "ReturnPurchaseOrderRequestPayload [rpoNumber=" + rpoNumber + ", plantNumber=" + plantNumber
				+ ", rpoBackendStatus=" + rpoBackendStatus + ", rpoBackendStatusList=" + rpoBackendStatusList + "]";
	}
	
	
}
