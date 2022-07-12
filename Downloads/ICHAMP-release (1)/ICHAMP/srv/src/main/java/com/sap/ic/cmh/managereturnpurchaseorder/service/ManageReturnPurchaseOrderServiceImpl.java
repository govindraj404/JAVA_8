package com.sap.ic.cmh.managereturnpurchaseorder.service;

import java.util.ArrayList;
import java.util.List;

import java.time.Instant;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import com.sap.cds.Result;
import com.sap.ic.cmh.businessobjects.persistency.BusinessObjectDao;
import com.sap.ic.cmh.businessobjects.service.BusinessObjectService;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.com.sap.ic.cmh.returnpurchaseorderstatusmapping.ReturnPurchaseOrderStatusMappings;
import cds.gen.managereturnpurchaseorderservice.BusinessObjectStatuses;
import cds.gen.managereturnpurchaseorderservice.ReturnPurchaseOrders;

@Service
public class ManageReturnPurchaseOrderServiceImpl implements ManageReturnPurchaseOrderService {
	
	
    @Autowired
    BusinessObjectDao businessObjectDao;
    @Autowired
    BusinessObjectService businessObjectService;
	
	 private static final String MANAGE_RETURN_PURCHASE_ORDER_SERVICE = "ManageReturnPurchaseOrderServiceImpl";
	 private static final Logger logger = LoggerFactory.getLogger(ManageReturnPurchaseOrderServiceImpl.class);
	 private static final String UPDATE_RPO_STATUS = "updateReturnPurchaseOrderStatus";

	@Override
	public List<BusinessObjectStatuses> updateReturnPurchaseOrderStatus(ReturnPurchaseOrders manageReturnPurchaseOrders) {
		LoggerHelper.logMethodEntry(logger, MANAGE_RETURN_PURCHASE_ORDER_SERVICE, UPDATE_RPO_STATUS);
		logger.info("inside update rpo handler");
        List<ReturnPurchaseOrderStatusMappings> rpoStatusMappings = businessObjectDao
                .getAllReturnPOStatusMappings();
        logger.info("rpoStatusMappings List {}: ", rpoStatusMappings.size());
        List<BusinessObjectStatuses> businessObjectStatuses = manageReturnPurchaseOrders.getBusinessObjectStatuses();
        List<BusinessObjectStatuses> boStatuses = new ArrayList<>();
        businessObjectStatuses.forEach(boStatus->{
            String rpoBackendStatus = boStatus.getBusinessObjectStatusCode();
            logger.info("rpoBackendStatus :: {} ", rpoBackendStatus);
            // Get the Pre-Defined Status Code using the Backend Status Code
            ReturnPurchaseOrderStatusMappings rpoStatusEntry = rpoStatusMappings.stream()
                    .filter(rpoStatus -> rpoBackendStatus.equalsIgnoreCase(rpoStatus.getCode())).findAny().orElse(null);
            logger.info("RPO Status Entry Mapping: {}", rpoStatusEntry);
            Result result = businessObjectDao.findBOStatusCode(manageReturnPurchaseOrders.getId(), rpoStatusEntry.getStatusCode());
            List<BusinessObjectStatuses> boStatusesExists = result.first().isPresent()
                    ? result.listOf(BusinessObjectStatuses.class)
                    : new ArrayList<>();
            logger.info("[checkIfStatusesExistsAndUpdateFromBackend] Size of Business Object status for RPO: {} ",
                    boStatusesExists.size());
            if (CollectionUtils.isEmpty(boStatusesExists)) {
            	 List<BusinessObjectStatuses> existingBOStatusList = findExistingBusinessObjectStatusList(manageReturnPurchaseOrders.getId());
      		   logger.info("Finding existing BO Status for RPO ");
      		   if(!CollectionUtils.isEmpty(existingBOStatusList)) {
      			    logger.info("Size of Existing Business Object status for RPO: {} ", existingBOStatusList.size());
      			    setBusinessObjectStatuses(boStatus,boStatus.getBusinessObjectStatusCode());
      			  boStatus.setParent(manageReturnPurchaseOrders.getId());
      			    //set the current status code
      			  manageReturnPurchaseOrders.setStatusCode(boStatus.getBusinessObjectStatusCode());
      			    logger.info("Status Code for Update is : {} ",boStatus.getBusinessObjectStatusCode());
      			boStatuses.add(boStatus);
      		    boStatuses.addAll(existingBOStatusList);
      			   }
            }else {
            	List<BusinessObjectStatuses> existingBOStatusList = findExistingBusinessObjectStatusList(manageReturnPurchaseOrders.getId());
            	 logger.info("Status already exists for update ");
            	boStatuses.addAll(!CollectionUtils.isEmpty(existingBOStatusList)?existingBOStatusList:new ArrayList<>());
            	logger.info("Status list for existing statuses {}  ",boStatuses.size());
            }
        });
        LoggerHelper.logMethodExit(logger, MANAGE_RETURN_PURCHASE_ORDER_SERVICE, UPDATE_RPO_STATUS);
		return boStatuses;



	}
	
	 /**
     * Set Business Object Status Code and Business Object Type Code
     * @param boStatus
     * @param backendStatusCode
     */
    public void setBusinessObjectStatuses(BusinessObjectStatuses boStatus, String backendStatusCode){
    	LoggerHelper.logMethodEntry(logger, MANAGE_RETURN_PURCHASE_ORDER_SERVICE, UPDATE_RPO_STATUS);
        String statusCode =getStatus(backendStatusCode);
        boStatus.setBusinessObjectStatusCode(statusCode);
        boStatus.setBusinessObjectType(Constants.RETURNPO_CODE);
        boStatus.setCreatedAt(Instant.now());
        LoggerHelper.logMethodExit(logger, MANAGE_RETURN_PURCHASE_ORDER_SERVICE, UPDATE_RPO_STATUS);
    }
    /**
     * Determine CMH Cockpit status based on the backend status
     * @param backendStatusCode
     * @return
     */
    public String getStatus(String backendStatusCode){
        return businessObjectService.determineReturnOrderStatus(backendStatusCode);
      }
	
    /**
     * Find the Existing Business Object Codes for the RPO ID
     * @param returnOrderId
     * @return
     */
    public List<BusinessObjectStatuses> findExistingBusinessObjectStatusList(String returnOrderId){
        Result result=businessObjectDao.findBOStatusBasedOnBOId(returnOrderId);
        return  result.first().isPresent()
                                ? result.listOf(BusinessObjectStatuses.class)
                                : new ArrayList<>();
        }

}
