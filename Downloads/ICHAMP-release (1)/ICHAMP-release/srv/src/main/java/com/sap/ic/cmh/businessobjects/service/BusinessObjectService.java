package com.sap.ic.cmh.businessobjects.service;

import cds.gen.complaintservice.BusinessObjects;

public interface BusinessObjectService{
	
	public void updateBusinessObjects(Object businessObject,String boType);

	public void setBusinessObjectStatus(String boType, String backendStatusCode, String businessObjectId, boolean isActive);
	
	public BusinessObjects getBusinessObjectsBasedOnBusinessObjectId(String businessObjectId);

	public String getCurrentBOStatus(String businessObjectIDId);

	public String fetchStreamTypeByBOType(String boType);
	
	public String getBusinessObjectStatusBasedOnType(String complaintId, String boType);

	public void insertBusinessObjectRelations(String sourceBOType, String qualityNotificationId, String targetBOType,
			String targetId);
	public String getCurrentActiveBOStatus(String businessObjectIDId);

	public boolean checkIfBOIsRelevant(String complaintId, String boType);
    
    public boolean checkIfBOStatusExists(String businessObjectId, String boStatus);
    
    public void insertBusinessObjectStatus(String boType, String businessObjectId, String statusCode,boolean isActive);
    
    public String getCurrentActiveBOStatusSupplier8D(String businessObjectIDId);

    public String getCurrentBOStatusSupplier8D(String businessObjectId);
    
    public String determineQualityNotificationStatus(String backendStatusCode);

    public String getBusinessObjectIdBasedOnTypeAndComplaint(String complaintId,
			String boType);
    
    public String determineReturnOrderStatus(String backendStatusCode);
	
}