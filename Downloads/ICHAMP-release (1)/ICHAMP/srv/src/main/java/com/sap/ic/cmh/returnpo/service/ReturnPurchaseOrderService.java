package com.sap.ic.cmh.returnpo.service;

import cds.gen.returnpurchaseorderservice.ReturnPurchaseOrders;

public interface ReturnPurchaseOrderService {

    void createReturPurchaseOrder(ReturnPurchaseOrders returnPO);

    void setConfiguredValues(ReturnPurchaseOrders returnPO, String boType, String complaintTypeCode);

    ReturnPurchaseOrders getReturnPurchaseOrderDetails(String returnPurchaseOrderId);

    void validateReturnPurchaseOrderFields(ReturnPurchaseOrders returnPO);

    String checkIfReturnPOExistsBasedOnNumber(String returnPoNumber);

    ReturnPurchaseOrders getReturnPurchaseOrderDetailsBasedOnComplaintId(String complaintId);

    void validateIfReturnPurchaseOrderExistsForComplaint(String complaintId);

    void validateIfReturnPurchaseOrderExists(ReturnPurchaseOrders returnPurchaseOrders);

    ReturnPurchaseOrders getReturnPurchaseOrderDetailsBasedOnNumber(String returnPurchaseOrderNumber);

    ReturnPurchaseOrders getReturnOrderStatusAndCompanyCode(String returnPurchaseOrderId);

    ReturnPurchaseOrders getDraftReturnOrderByComplaintID(String complaintId);

    void deleteDraftReturnOrderByID(String returnPurchaseOrderId);
    
    cds.gen.managereturnpurchaseorderservice.ReturnPurchaseOrders getActiveReturnPurchaseOrders(String returnPurchaseOrderId);
    
    ReturnPurchaseOrders getReturnPurchaseOrderBasedOnId(String returnPurchaseOrderId);
}
