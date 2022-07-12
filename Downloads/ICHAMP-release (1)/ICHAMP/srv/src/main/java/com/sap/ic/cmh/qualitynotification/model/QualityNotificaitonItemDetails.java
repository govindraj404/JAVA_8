package com.sap.ic.cmh.qualitynotification.model;


public class QualityNotificaitonItemDetails {
	private String itemKey;
	private String defectGroup;
	private String defectCode;
	private String defectDescription;

	public String getItemKey() {
		return itemKey;
	}

	public void setItemKey(String itemKey) {
		this.itemKey = itemKey;
	}

	public String getDefectGroup() {
		return defectGroup;
	}

	public void setDefectGroup(String defectGroup) {
		this.defectGroup = defectGroup;
	}

	public String getDefectCode() {
		return defectCode;
	}

	public void setDefectCode(String defectCode) {
		this.defectCode = defectCode;
	}
    
    public String getDefectDescription() {
		return defectDescription;
	}

	public void setDefectDescription(String defectDescription) {
		this.defectDescription = defectDescription;
	}

	@Override
	public String toString() {
		return "QualityNotificaitonItemDetails [itemKey=" + itemKey + ", defectGroup=" + defectGroup + ", defectCode="
				+ defectCode + ", defectDescription=" + defectDescription + "]";
	}

}
