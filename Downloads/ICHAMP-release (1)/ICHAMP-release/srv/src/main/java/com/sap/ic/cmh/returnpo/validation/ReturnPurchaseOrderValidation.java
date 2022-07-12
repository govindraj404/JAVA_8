package com.sap.ic.cmh.returnpo.validation;

import cds.gen.returnpurchaseorderservice.ReturnPurchaseOrders;

public interface ReturnPurchaseOrderValidation {

	void validateMandatoryFields(ReturnPurchaseOrders returnPO);

	void validateFieldControlReturnPO(ReturnPurchaseOrders returnPO);

	void validateIfReturnPurchaseOrderExistsForComplaint(String complaintId);

	void validateifBOIsRelevant(String complaintId, String returnpoCode);

	void validateIfReturnPurchaseOrderExists(String id);

}
