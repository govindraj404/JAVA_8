package com.sap.ic.cmh.managesupplierissueprocess.handler;

import java.util.UUID;


import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.sap.cds.Result;
import com.sap.cds.services.ServiceException;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.After;
import com.sap.cds.services.handler.annotations.Before;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import java.util.List;
import java.util.Map;
import com.sap.ic.cmh.auditlog.ObjectDiff;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.auditlog.AuditLogDifference;
import com.sap.ic.cmh.complaint.persistency.ComplaintsDao;
import com.sap.ic.cmh.configuration.service.ConfigurationService;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.supplierissueprocess.handler.EightDHandler;
import com.sap.ic.cmh.utils.CommonFunctions;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.supplierissueprocess.service.EightDService;

import cds.gen.managesupplierissueprocessservice.ManageSupplierIssueProcessService_;
import cds.gen.managesupplierissueprocessservice.Supplier8DProcesses;
import cds.gen.managesupplierissueprocessservice.Supplier8DProcesses_;
import cds.gen.masterdataservice.BusinessPartners;


@Component
@ServiceName(ManageSupplierIssueProcessService_.CDS_NAME)
public class ManageSupplierIssueProcessHandler implements EventHandler {

    @Autowired
	AuditLogHelper auditLogHelper;
	@Autowired
	EightDHandler eightDHandler;
	@Autowired
	CommonFunctions commonFunctions;

	@Autowired
	ComplaintsDao complaintDao;

	@Autowired
	ConfigurationService configurationService;

    @Autowired
	EightDService eightDService;

    @Autowired
	private AuditLogDifference<cds.gen.supplierissueprocessservice.Supplier8DProcesses> auditLogDifference;

    private static final String ENTITY_NAME = "MANAGE_SUPPLIER8D";
	private static final Logger logger = LoggerFactory.getLogger(ManageSupplierIssueProcessHandler.class);
	private static final String MANAGE_SUPPLIER_ISSUE_PROCESS_HANDLER = "ManageSupplierIssueProcessHandler";
	private static volatile String supplierIssueProcessId;

	/**
	 * This method is used to call the BEFORE-CREATE event of draft Supplier 8D Handler
	 * validate mandatory attributes to create Supplier 8D
	 * @param manageSupplier8DProcesses
	 */
	@Before(event = { CdsService.EVENT_CREATE }, entity = Supplier8DProcesses_.CDS_NAME)
	public void beforeManageSupplier8DCreate(Supplier8DProcesses manageSupplier8DProcesses) {
		LoggerHelper.logMethodEntry(logger, MANAGE_SUPPLIER_ISSUE_PROCESS_HANDLER, "beforeManageSupplier8DCreate");
		setMasterDataDetails(manageSupplier8DProcesses);
		cds.gen.supplierissueprocessservice.Supplier8DProcesses supplierIssueProcess = commonFunctions
				.convertManageSupplierEightDToSupplierEightD(manageSupplier8DProcesses);
		supplierIssueProcessId = UUID.randomUUID().toString();
		supplierIssueProcess.setId(supplierIssueProcessId);
		supplierIssueProcess.setStatusCode(Constants.STATUS_NEW);
		// Is this scenario possible?
		if (StringUtils.isNotBlank(manageSupplier8DProcesses.getComplaintCode())) {
			Result comp = complaintDao.getComplaintBasedOnCode(manageSupplier8DProcesses.getComplaintCode());
			if(comp.first().isPresent()){
				supplierIssueProcess.setComplaintId(comp.first().get().get("ID").toString());
				manageSupplier8DProcesses.setComplaintId(comp.first().get().get("ID").toString());
			}
		} else {
			throw new ServiceException(MessageKeys.COMPLAINT_ASSOCIATION_TO_EIGHTD);
		}
		eightDHandler.beforeSupplier8DCreate(supplierIssueProcess);
		manageSupplier8DProcesses.setSupplierIssueProcessesType(supplierIssueProcess.getSupplierIssueProcessesType());
		manageSupplier8DProcesses.setMaterialId(supplierIssueProcess.getMaterialId());
		manageSupplier8DProcesses.setSupplierId(supplierIssueProcess.getSupplierId());
		manageSupplier8DProcesses.setPlantId(supplierIssueProcess.getPlantId());
		manageSupplier8DProcesses.setPurchasingOrganizationId(supplierIssueProcess.getPurchasingOrganizationId());
		manageSupplier8DProcesses.setCompanyId(supplierIssueProcess.getCompanyId());
		manageSupplier8DProcesses.setQuantity(supplierIssueProcess.getQuantity());
		manageSupplier8DProcesses.setUnit(supplierIssueProcess.getUnit());
		LoggerHelper.logMethodExit(logger, MANAGE_SUPPLIER_ISSUE_PROCESS_HANDLER, "beforeManageSupplier8DCreate");
	}
	
	/**
	 * This method is used to call the ON-CREATE event of draft Supplier 8D Handler
	 * create supplier 8D in the configured destination
	 * Link the Business object with complaint
	 * Insert business object status
	 * @param manageSupplier8DProcesses
	 */
	@On(event = CdsService.EVENT_CREATE, entity = Supplier8DProcesses_.CDS_NAME)
	public void onManageSupplier8DCreate(Supplier8DProcesses manageSupplier8DProcesses,CdsCreateEventContext context) {
		LoggerHelper.logMethodEntry(logger, MANAGE_SUPPLIER_ISSUE_PROCESS_HANDLER, "onManageSupplier8DCreate");
		cds.gen.supplierissueprocessservice.Supplier8DProcesses supplierIssueProcess = commonFunctions
				.convertManageSupplierEightDToSupplierEightD(manageSupplier8DProcesses);
		supplierIssueProcess.setId(supplierIssueProcessId);
		eightDHandler.onSupplier8DCreate(supplierIssueProcess,context);
		manageSupplier8DProcesses.setIdentifier(supplierIssueProcess.getIdentifier());
		manageSupplier8DProcesses.setDefectId(supplierIssueProcess.getDefectId());
		manageSupplier8DProcesses.setStatusCode(supplierIssueProcess.getStatusCode());
		manageSupplier8DProcesses.setBusinessObjectStatuses(supplierIssueProcess.getBusinessObjectStatuses());
		manageSupplier8DProcesses.setId(supplierIssueProcess.getId());
		LoggerHelper.logMethodExit(logger, MANAGE_SUPPLIER_ISSUE_PROCESS_HANDLER, "onManageSupplier8DCreate");
		
	}
	
	/**
	 * This method is used to call the AFTER-CREATE event of draft Supplier 8D Handler
	 * update Stream status according to the business object status
	 * @param manageSupplier8DProcesses
	 */
	@After(event = CdsService.EVENT_CREATE, entity = Supplier8DProcesses_.CDS_NAME)
	public void afterManageSupplier8DCreate(Supplier8DProcesses manageSupplier8DProcesses) {
		LoggerHelper.logMethodEntry(logger, MANAGE_SUPPLIER_ISSUE_PROCESS_HANDLER, "afterManageSupplier8DCreate");
		cds.gen.supplierissueprocessservice.Supplier8DProcesses supplierIssueProcess = commonFunctions
				.convertManageSupplierEightDToSupplierEightD(manageSupplier8DProcesses);
		eightDHandler.afterSupplier8DCreate(supplierIssueProcess);
		LoggerHelper.logMethodExit(logger, MANAGE_SUPPLIER_ISSUE_PROCESS_HANDLER, "afterManageSupplier8DCreate");
        logUpsert(manageSupplier8DProcesses);
	}

	/**
	 * This method is used to set the master data details
	 * @param manageClaims
	 */
	private void setMasterDataDetails(Supplier8DProcesses manageSupplier8DProcesses){
		BusinessPartners supplierPerson = configurationService.validateSupplierContactPerson(manageSupplier8DProcesses.getContactPersonCode());
		if(supplierPerson!=null){
			manageSupplier8DProcesses.setContactPersonId(supplierPerson.getId());
		}
		BusinessPartners personResponsible = configurationService.validatePersonResponsibleCode(manageSupplier8DProcesses.getPersonResponsibleCode());
		if(personResponsible!=null){
			manageSupplier8DProcesses.setPersonResponsibleId(personResponsible.getId());
		}
    }

    public void logUpsert(Supplier8DProcesses manageSupplier8DProcesses) {
		cds.gen.supplierissueprocessservice.Supplier8DProcesses newData = eightDService.getEightDBasedOnId(manageSupplier8DProcesses.getId());
		List<ObjectDiff> diffList = auditLogDifference.getDifference(newData);
		Map<String, String> entityInfoMap = auditLogHelper.buildEntityInfoMap(
				ENTITY_NAME,
				newData.getId());
		auditLogHelper.logUpsertAuditData(diffList, entityInfoMap);
	}
}
