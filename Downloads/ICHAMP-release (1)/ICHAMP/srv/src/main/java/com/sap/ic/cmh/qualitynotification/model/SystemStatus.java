package com.sap.ic.cmh.qualitynotification.model;

public class SystemStatus {

	private String activeIndicator;
	private String code;
    private String description;

	public String getActiveIndicator() {
		return activeIndicator;
	}

	public void setActiveIndicator(String activeIndicator) {
		this.activeIndicator = activeIndicator;
	}

	public String getCode() {
		return code;
	}

	public void setCode(String code) {
		this.code = code;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
    }
    
    @Override
	public String toString() {
		return "SystemStatus [activeIndicator=" + activeIndicator + ", code=" + code + ", description=" + description
				+ "]";
	}

}
