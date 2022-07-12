package com.sap.ic.cmh.managereturnpurchaseorder.service;

import java.util.List;

import cds.gen.managereturnpurchaseorderservice.BusinessObjectStatuses;
import cds.gen.managereturnpurchaseorderservice.ReturnPurchaseOrders;


public interface ManageReturnPurchaseOrderService {
	
	List<BusinessObjectStatuses> updateReturnPurchaseOrderStatus(ReturnPurchaseOrders returnPurchaseOrders);

}
