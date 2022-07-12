package com.sap.ic.cmh.internalsupplierissueprocess.handler;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.After;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.ic.cmh.businessobjects.service.BusinessObjectService;
import com.sap.ic.cmh.supplierissueprocess.handler.EightDHandler;
import com.sap.ic.cmh.utils.CommonFunctions;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.supplierissueprocessinternalservice.Supplier8DProcesses;
import cds.gen.supplierissueprocessinternalservice.Supplier8DProcesses_;


@Component
@ServiceName("SupplierIssueProcessInternalService")
public class InternalEightDHandler implements EventHandler {
	
	@Autowired
	EightDHandler eightDHandler;
	@Autowired
	CommonFunctions commonFunctions;
	@Autowired
	BusinessObjectService businessObjectService;

	private static final Logger logger = LoggerFactory.getLogger(InternalEightDHandler.class);
	private static final String INTERNAL_SUPPLIER_ISSUE_PROCESS_HANDLER = "InternalEightDHandler";
	
	/**
	 * This method is used to call the ON-UPDATE event of draft Supplier 8D Handler
	 * Insert business object status
	 * update the 8D status with latest data received from target system
	 * @param internalSupplier8DProcesses
	 */
	@On(event = CdsService.EVENT_UPDATE, entity = Supplier8DProcesses_.CDS_NAME)
	public void onManageSupplier8DProcessesUpdate(Supplier8DProcesses internalSupplier8DProcesses) {
		LoggerHelper.logMethodEntry(logger, INTERNAL_SUPPLIER_ISSUE_PROCESS_HANDLER, "onManageSupplier8DProcessesUpdate");
		cds.gen.supplierissueprocessservice.Supplier8DProcesses supplier8DProcess = commonFunctions
				.convertInternalSupplierEightDToSupplierEightD(internalSupplier8DProcesses);
		businessObjectService.insertBusinessObjectStatus(Constants.SUPPLIER_EIGHTD_CODE, supplier8DProcess.getId(), supplier8DProcess.getStatusCode(), true);
		eightDHandler.setCurrentBOStatus(supplier8DProcess);
		internalSupplier8DProcesses.setStatusCode(supplier8DProcess.getStatusCode());
		
		LoggerHelper.logMethodEntry(logger, INTERNAL_SUPPLIER_ISSUE_PROCESS_HANDLER, "onManageSupplier8DProcessesUpdate");
	}
	
	
	/**
	 * This method is used to call the AFTER-UPDATE event of draft Supplier 8D Handler
	 * update Stream status according to the business object status
	 * @param internalSupplier8DProcesses
	 */
	@After(event = CdsService.EVENT_UPDATE, entity = Supplier8DProcesses_.CDS_NAME)
	public void afterManageSupplier8DProcessesUpdate(Supplier8DProcesses internalSupplier8DProcesses) {
		LoggerHelper.logMethodEntry(logger, INTERNAL_SUPPLIER_ISSUE_PROCESS_HANDLER, "afterManageSupplier8DProcessesUpdate");
		cds.gen.supplierissueprocessservice.Supplier8DProcesses supplier8DProcess = commonFunctions
				.convertInternalSupplierEightDToSupplierEightD(internalSupplier8DProcesses);
		
		eightDHandler.updateStreamStatus(supplier8DProcess, Constants.SUPPLIER_EIGHTD_CODE, true);
		LoggerHelper.logMethodEntry(logger, INTERNAL_SUPPLIER_ISSUE_PROCESS_HANDLER, "afterManageSupplier8DProcessesUpdate");
	}

}
