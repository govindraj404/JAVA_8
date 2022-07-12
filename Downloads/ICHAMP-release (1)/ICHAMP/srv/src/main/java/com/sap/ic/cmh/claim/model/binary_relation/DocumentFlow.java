package com.sap.ic.cmh.claim.model.binary_relation;

public class DocumentFlow {

	private String objectANumber;
	private String objectAType;
	private String objectBNumber;
	private String objectBType;
	private String sourceLogicalSystem;
	private BinaryRelationObject objectA;
	private BinaryRelationObject objectB;
	private String targetLogicalSystem;

	public String getObjectANumber() {
		return objectANumber;
	}

	public void setObjectANumber(String objectANumber) {
		this.objectANumber = objectANumber;
	}

	public String getObjectAType() {
		return objectAType;
	}

	public void setObjectAType(String objectAType) {
		this.objectAType = objectAType;
	}

	public String getObjectBNumber() {
		return objectBNumber;
	}

	public void setObjectBNumber(String objectBNumber) {
		this.objectBNumber = objectBNumber;
	}

	public String getObjectBType() {
		return objectBType;
	}

	public void setObjectBType(String objectBType) {
		this.objectBType = objectBType;
	}

	public String getSourceLogicalSystem() {
		return sourceLogicalSystem;
	}

	public void setSourceLogicalSystem(String sourceLogicalSystem) {
		this.sourceLogicalSystem = sourceLogicalSystem;
	}

	public BinaryRelationObject getObjectA() {
		return objectA;
	}

	public void setObjectA(BinaryRelationObject objectA) {
		this.objectA = objectA;
	}

	public BinaryRelationObject getObjectB() {
		return objectB;
	}

	public void setObjectB(BinaryRelationObject objectB) {
		this.objectB = objectB;
	}

	public String getTargetLogicalSystem() {
		return targetLogicalSystem;
	}

	public void setTargetLogicalSystem(String targetLogicalSystem) {
		this.targetLogicalSystem = targetLogicalSystem;
	}

}
