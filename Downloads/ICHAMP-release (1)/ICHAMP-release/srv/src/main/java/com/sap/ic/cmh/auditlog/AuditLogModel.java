package com.sap.ic.cmh.auditlog;

import java.util.Map;

public class AuditLogModel {
	private String userId;
    private String tenant;
    private String operation;
    private String roles;
    private Map<String, String> entityInfoMap;
    ObjectDiff diff;

    public Map<String, String> getEntityInfoMap() {
        return entityInfoMap;
    }

    public void setEntityInfoMap(Map<String, String> entityInfoMap) {
        this.entityInfoMap = entityInfoMap;
    }

    public ObjectDiff getDiff() {
        return diff;
    }

    public void setDiff(ObjectDiff diff) {
        this.diff = diff;
    }

    public String getUserId() {
        return userId;
    }

    public void setUserId(String userId) {
        this.userId = userId;
    }

    public String getTenant() {
        return tenant;
    }

    public void setTenant(String tenant) {
        this.tenant = tenant;
    }

    public String getOperation() {
        return operation;
    }

    public void setOperation(String operation) {
        this.operation = operation;
    }

    public String getRoles() {
        return roles;
    }

    public void setRoles(String roles) {
        this.roles = roles;
    }

    
	public AuditLogModel(String userId, String tenant, String operation, String roles, Map<String, String> entityInfoMap,ObjectDiff diff) {
		super();
		this.userId = userId;
		this.tenant = tenant;
		this.operation = operation;
		this.roles = roles;
        this.entityInfoMap = entityInfoMap;
        this.diff = diff;
	}


    
}
