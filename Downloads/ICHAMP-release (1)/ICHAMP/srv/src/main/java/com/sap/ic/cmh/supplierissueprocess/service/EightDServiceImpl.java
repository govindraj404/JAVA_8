package com.sap.ic.cmh.supplierissueprocess.service;

import java.time.LocalTime;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.apache.commons.collections.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

import com.google.gson.Gson;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.ql.Update;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cloud.sdk.cloudplatform.connectivity.HttpDestination;
import com.sap.cloud.sdk.cloudplatform.connectivity.ScpCfDestinationLoader;
import com.sap.cloud.sdk.datamodel.odata.client.exception.ODataException;
import com.sap.cloud.sdk.datamodel.odata.client.exception.ODataServiceError;
import com.sap.cloud.sdk.datamodel.odata.client.exception.ODataServiceErrorDetails;
import com.sap.cloud.sdk.datamodel.odata.client.exception.ODataServiceErrorException;
import com.sap.cloud.sdk.datamodel.odata.helper.ModificationResponse;
import com.sap.ic.cmh.businessobjects.persistency.BusinessObjectDao;
import com.sap.ic.cmh.businessobjects.service.BusinessObjectService;
import com.sap.ic.cmh.claim.validations.ClaimValidation;
import com.sap.ic.cmh.configuration.model.MasterData;
import com.sap.ic.cmh.configuration.persistency.BusinessObjectConfigurationDao;
import com.sap.ic.cmh.configuration.persistency.DestinationConfigurationDao;
import com.sap.ic.cmh.configuration.service.ConfigurationService;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.network.service.DestinationService;
import com.sap.ic.cmh.network.service.HttpService;
import com.sap.ic.cmh.qualitynotification.persistency.QualityNotificationDao;
import com.sap.ic.cmh.supplierissueprocess.persistency.EightDDao;
import com.sap.ic.cmh.supplierissueprocess.validations.EightDValidation;
import com.sap.ic.cmh.utils.CommonFunctions;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.com.sap.ic.cmh.supplierissueprocessstatusmapping.SupplierIssueProcessStatusMappings;
import cds.gen.complaintservice.BusinessObjectStatuses;
import cds.gen.configurationservice.BusinessObjectConfigurations;
import cds.gen.qualitynotificationservice.Defects;
import cds.gen.qualitynotificationservice.QualityNotifications;
import cds.gen.supplierissueprocessinternalservice.Supplier8DProcesses_;
import cds.gen.supplierissueprocessservice.Supplier8DProcesses;
import vdm.namespaces.qmsipsrv.SuplrIssProcCustPartners;
import vdm.namespaces.qmsipsrv.SuplrIssProcSteps;
import vdm.namespaces.qmsipsrv.SuplrIssProcSuplrPartners;
import vdm.namespaces.qmsipsrv.SupplierIssueProcesses;
import vdm.services.APIQMSIPSRVService;
import vdm.services.DefaultAPIQMSIPSRVService;

@Service
public class EightDServiceImpl implements EightDService {

	@Autowired
	EightDDao eightDDao;

	@Autowired
	EightDValidation eightDValidator;

	@Autowired
	ConfigurationService configurationService;

	@Autowired
	DestinationConfigurationDao destinationConfigDao;

	@Autowired
	BusinessObjectConfigurationDao businessObjectConfigurationDao;

	@Autowired
	QualityNotificationDao qnDao;

	@Autowired
	DestinationService destinationService;

	@Autowired
	BusinessObjectService businessObjectService;

	@Autowired
	HttpService httpService;

	@Autowired
	Messages messages;

	@Autowired
	@Qualifier("SupplierIssueProcessInternalService")
	private CdsService cdsService;

	@Autowired
	ClaimValidation claimValidation;
	@Autowired
	CommonFunctions commonFunctions;
	@Autowired
	BusinessObjectDao businessObjectDao;

	private static final Logger logger = LoggerFactory.getLogger(EightDServiceImpl.class);
	private static final String SUPPLIER_8D_SERVICE_IMPL = "EightDServiceImpl";
	private static final String CREATE_8D = "create8D";
	private static final String MAP_EIGHTD_STATUS = "mapEightDStatus";
    private static final String GET_EIGHTD_DETAILS = "getEightDDetails";

	/**
	 * Create Supplier 8D in the target system and create document flow between QN
	 * and supplier 8D
	 */
	@Override
	public void create8D(Supplier8DProcesses eightD, String userId) {
		LoggerHelper.logMethodEntry(logger, SUPPLIER_8D_SERVICE_IMPL, CREATE_8D);
		String eightDNumber = create8DAtDestination(eightD, userId);
		String targetDestination = "";
		if (eightDNumber != null && !eightDNumber.equals("")) {
			eightD.setIdentifier(eightDNumber);
			Result eightDestinationConfig = destinationConfigDao
					.getDestinationConfigBasedOnCompanyAndBOType(eightD.getCompanyId(), Constants.SUPPLIER_EIGHTD_CODE);
			Optional<Row> eightDestinationConfigFirst = eightDestinationConfig.first();
			if (eightDestinationConfigFirst.isPresent()) {
				targetDestination = eightDestinationConfigFirst.get().get(Constants.DESTINATION).toString();
			}
			// Create document flow between QN and 8D
			Result qualityNotification = qnDao.getQualityNotificationDetailsByComplaintId(eightD.getComplaintId());
			Optional<Row> qualityNotificationFirst = qualityNotification.first();
			if (qualityNotificationFirst.isPresent()) {
				String destination = "";
				ScpCfDestinationLoader scpCfDestinationLoader = new ScpCfDestinationLoader();
				String qualityNotificationNumber = qualityNotificationFirst.get().get(QualityNotifications.IDENTIFIER)
						.toString();
				Result destinationConfigs = destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(
						eightD.getCompanyId(), Constants.QUALITYNOTIFICATION_CODE);
				Optional<Row> destinationConfigsFirst = destinationConfigs.first();
				if (destinationConfigsFirst.isPresent()) {
					destination = destinationConfigsFirst.get().get(Constants.DESTINATION).toString();
				}
				httpService.createDocumentFlowObjects(scpCfDestinationLoader, qualityNotificationNumber,
						Constants.QUALITY_NOTIFICATION_OBJECT_TYPE, eightDNumber, Constants.EIGHTD_OBJECT_TYPE,
						destination, targetDestination);
				businessObjectService.insertBusinessObjectRelations(Constants.QUALITYNOTIFICATION_CODE,
						qualityNotificationFirst.get().get(QualityNotifications.ID).toString(),
						Constants.SUPPLIER_EIGHTD_CODE,
						eightD.getId());
			}
		} else {
			messages.error(MessageKeys.ERROR_8D_CREATION_AT_DESTINATION);
		}
		LoggerHelper.logMethodExit(logger, SUPPLIER_8D_SERVICE_IMPL, CREATE_8D);
	}

	/**
	 * Form the request for supplier 8D and create in the configured system
	 * 
	 * @param eightD
	 * @param userId
	 * @return
	 */
	private String create8DAtDestination(Supplier8DProcesses eightD, String userId) {
		LoggerHelper.logMethodEntry(logger, SUPPLIER_8D_SERVICE_IMPL, CREATE_8D);
		APIQMSIPSRVService service = new DefaultAPIQMSIPSRVService();
		String eightDNumber = "";
        String errorMsg = "";
		Result eightDestinationConfig = destinationConfigDao
				.getDestinationConfigBasedOnCompanyAndBOType(eightD.getCompanyId(), Constants.SUPPLIER_EIGHTD_CODE);
		Optional<Row> eightDestinationConfigFirst = eightDestinationConfig.first();
		if (eightDestinationConfigFirst.isPresent()) {
			try {
				ScpCfDestinationLoader scpCfDestinationLoader = new ScpCfDestinationLoader();
				HttpDestination destination = destinationService.getHttpDestination(scpCfDestinationLoader,
						eightDestinationConfigFirst.get().get(Constants.DESTINATION).toString());
				SupplierIssueProcesses supplierIssueProcesses = new SupplierIssueProcesses();
				supplierIssueProcesses.setProcessTypeId(eightD.getSupplierIssueProcessesType());

				// set Master Data details
				setMasterData(eightD, supplierIssueProcesses);
				// Set details from Quality Notification
				setDetailsFromQualityNotification(eightD, supplierIssueProcesses);

				// Store the Date
				if (eightD.getRequestStartDate() != null) {
					supplierIssueProcesses.setRequestedStartDate(eightD.getRequestStartDate().atTime(LocalTime.now()));
				}
				if (eightD.getRequestEndDate() != null) {
					supplierIssueProcesses.setRequestedEndDate(eightD.getRequestEndDate().atTime(LocalTime.now()));
				}

				SuplrIssProcSteps suplrIssProcSteps1 = new SuplrIssProcSteps();
				suplrIssProcSteps1.setStepTypeId("D1");
				suplrIssProcSteps1.setProcessTypeId("8D");

				// Set customer partners
				setCustomerPartners(suplrIssProcSteps1, userId);
				// set supplier partners
				setSupplierPartners(suplrIssProcSteps1);

				SuplrIssProcSteps suplrIssProcSteps2 = new SuplrIssProcSteps();
				suplrIssProcSteps2.setStepTypeId("D2");
				suplrIssProcSteps2.setProcessTypeId("8D");
				SuplrIssProcSteps suplrIssProcSteps3 = new SuplrIssProcSteps();
				suplrIssProcSteps3.setStepTypeId("D3");
				suplrIssProcSteps3.setProcessTypeId("8D");
				SuplrIssProcSteps suplrIssProcSteps4 = new SuplrIssProcSteps();
				suplrIssProcSteps4.setStepTypeId("D4");
				suplrIssProcSteps4.setProcessTypeId("8D");
				SuplrIssProcSteps suplrIssProcSteps5 = new SuplrIssProcSteps();
				suplrIssProcSteps5.setStepTypeId("D5");
				suplrIssProcSteps5.setProcessTypeId("8D");
				SuplrIssProcSteps suplrIssProcSteps6 = new SuplrIssProcSteps();
				suplrIssProcSteps6.setStepTypeId("D6");
				suplrIssProcSteps6.setProcessTypeId("8D");
				SuplrIssProcSteps suplrIssProcSteps7 = new SuplrIssProcSteps();
				suplrIssProcSteps7.setStepTypeId("D7");
				suplrIssProcSteps7.setProcessTypeId("8D");
				SuplrIssProcSteps suplrIssProcSteps8 = new SuplrIssProcSteps();
				suplrIssProcSteps8.setStepTypeId("D8");
				suplrIssProcSteps8.setProcessTypeId("8D");

				List<SuplrIssProcSteps> suplrIssProcStepsList = new ArrayList<>();
				suplrIssProcStepsList.add(suplrIssProcSteps1);
				suplrIssProcStepsList.add(suplrIssProcSteps2);
				suplrIssProcStepsList.add(suplrIssProcSteps3);
				suplrIssProcStepsList.add(suplrIssProcSteps4);
				suplrIssProcStepsList.add(suplrIssProcSteps5);
				suplrIssProcStepsList.add(suplrIssProcSteps6);
				suplrIssProcStepsList.add(suplrIssProcSteps7);
				suplrIssProcStepsList.add(suplrIssProcSteps8);

				supplierIssueProcesses.setSteps(suplrIssProcStepsList);
				logger.info("Created Object:");

				Gson gsonObj = new Gson();
				String binaryRelationDataModelJsonStr = gsonObj.toJson(supplierIssueProcesses);
				logger.info(binaryRelationDataModelJsonStr);
				ModificationResponse<SupplierIssueProcesses> createdSupplierIssueProcesses = service
						.createSupplierIssueProcesses(supplierIssueProcesses).executeRequest(destination);
				SupplierIssueProcesses modifiedSupplierIssueProcesses = createdSupplierIssueProcesses
						.getModifiedEntity();

				logger.info("Fetched Supplier In JSON:");
				String modifiedSupplierIssueProcessesJsonStr = gsonObj.toJson(modifiedSupplierIssueProcesses);
				logger.info(modifiedSupplierIssueProcessesJsonStr);

				logger.info("Fetched EightD Number:");
				Map<String, Object> modifiedSupplierIssueProcessesMap = gsonObj
						.fromJson(modifiedSupplierIssueProcessesJsonStr, Map.class);
				eightDNumber = modifiedSupplierIssueProcessesMap.get("suplrIssProcId").toString();
				String processStatusId = modifiedSupplierIssueProcessesMap.get("processStatusId").toString();
				logger.info("Process Status id for created 8D : ", processStatusId);
				String confirmationStatus = modifiedSupplierIssueProcessesMap.get("confirmationStatusId").toString();
				logger.info("confirmation Status id for created 8D : ", confirmationStatus);
				eightD.setStatusCode(confirmationStatus);
				logger.info(eightDNumber);
			} catch (ODataException e) {
				logger.info("OData Exception");

				if (e instanceof ODataServiceErrorException) {
					logger.info("OData Service Error Exception");
					ODataServiceError oDataServiceError = ((ODataServiceErrorException) e).getOdataError();
					logger.info(oDataServiceError.getODataCode());
					logger.info(oDataServiceError.getODataMessage());
					logger.info(oDataServiceError.getInnerError().toString());
					logger.info("OData Service Error Details");
					for (ODataServiceErrorDetails oDataServiceErrorDetails : oDataServiceError.getDetails()) {
						logger.info(oDataServiceErrorDetails.getODataCode());
						logger.info(oDataServiceErrorDetails.getODataMessage());
                        errorMsg = oDataServiceErrorDetails.getODataMessage();
					}
				}
				messages.error(errorMsg);
				messages.throwIfError();
			}
		}
		LoggerHelper.logMethodExit(logger, SUPPLIER_8D_SERVICE_IMPL, CREATE_8D);
		return eightDNumber;
	}

	/**
	 * Set Supplier partners
	 * 
	 * @param suplrIssProcSteps1
	 */
	public void setSupplierPartners(SuplrIssProcSteps suplrIssProcSteps1) {
		List<SuplrIssProcSuplrPartners> suplrIssProcSuplrPartnersList = new ArrayList<>();
		SuplrIssProcSuplrPartners suplrIssProcSuplrPartners = new SuplrIssProcSuplrPartners();
		suplrIssProcSuplrPartnersList.add(suplrIssProcSuplrPartners);
		suplrIssProcSteps1.setSupplierPartners(suplrIssProcSuplrPartnersList);
	}

	/**
	 * Set Customer partners
	 * 
	 * @param suplrIssProcSteps1
	 */
	public void setCustomerPartners(SuplrIssProcSteps suplrIssProcSteps1, String userId) {
		logger.info("User ID of the logged in user before setting the value : ", userId);
		SuplrIssProcCustPartners suplrIssProcCustPartners = new SuplrIssProcCustPartners();
		suplrIssProcCustPartners.setBusinessPartnerId("");
		suplrIssProcCustPartners.setBusinessPartnerFullName("");
		suplrIssProcCustPartners.setBusinessPartnerEmail(userId);

		List<SuplrIssProcCustPartners> suplrIssProcCustPartnersList = new ArrayList<>();
		suplrIssProcCustPartnersList.add(suplrIssProcCustPartners);
		suplrIssProcSteps1.setCustomerPartners(suplrIssProcCustPartnersList);
	}

	/**
	 * set Master Data details
	 * 
	 * @param eightD
	 * @param supplierIssueProcesses
	 * @return
	 */
	public MasterData setMasterData(Supplier8DProcesses eightD, SupplierIssueProcesses supplierIssueProcesses) {
		// Read and Set Material, Plant and Supplier from Supplier8D
		MasterData masterDataDetails = configurationService.getMasterDataDetails(eightD.getMaterialId(),
				eightD.getPlantId(), eightD.getSupplierId(), eightD.getPurchasingOrganizationId());
		supplierIssueProcesses.setProduct(masterDataDetails.getMaterial().getMaterialCode());
		supplierIssueProcesses.setPlant(masterDataDetails.getPlants().getPlant());
		supplierIssueProcesses.setSupplier(masterDataDetails.getSupplier().getBusinessPartnerNumber());
		return masterDataDetails;
	}

	/**
	 * Set details from Quality notification
	 * 
	 * @param eightD
	 * @param supplierIssueProcesses
	 */
	public void setDetailsFromQualityNotification(Supplier8DProcesses eightD,
			SupplierIssueProcesses supplierIssueProcesses) {

		QualityNotifications qualityNotification = qnDao.fetchQNForSupplier8d(eightD.getComplaintId());

		if (qualityNotification != null) {
			eightDValidator.validateQNFields(qualityNotification);
			messages.throwIfError();
			supplierIssueProcesses.setNotification(qualityNotification.getIdentifier());
			supplierIssueProcesses.setPurchaseOrder(qualityNotification.getPurchaseOrderNumber());
			supplierIssueProcesses.setPurchaseOrderItem(qualityNotification.getPurchaseOrderItem());
			logger.info("Quality Notification Number : {} ", qualityNotification.getIdentifier());
			Defects defect = qualityNotification.getDefect();
			supplierIssueProcesses.setDefect(defect.getIdentifier());
			logger.info("Defect Number : {} ", defect.getIdentifier());
			supplierIssueProcesses.setDefectCodeGroup(defect.getDefectGroupCode());
			supplierIssueProcesses.setDefectCode(defect.getDefectCodeCode());
		}
	}

	/**
	 * get the configured destination based on company code id and business object
	 * type get the configured business object attributes based on complaint type
	 * code,destination and business object type
	 */
	@Override
	public void setConfiguredValues(Supplier8DProcesses eightD, String boType, String complaintTypeCode) {
		LoggerHelper.logMethodEntry(logger, SUPPLIER_8D_SERVICE_IMPL, "setConfiguredValues");
		String destination = "";
		Result result = destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(eightD.getCompanyId(),
				Constants.SUPPLIER_EIGHTD_CODE);

		destination = result.first().isPresent() ? result.list().get(0).get(Constants.DESTINATION).toString() : "";

		logger.info(destination);

		Result businessObjectConfigurationsResult = businessObjectConfigurationDao
				.getBusinessObjectConfigBasedOnDestinationAndBOAndDest(complaintTypeCode, boType, destination);

		List<BusinessObjectConfigurations> businessObjectConfigurations = businessObjectConfigurationsResult.first()
				.isPresent() ? businessObjectConfigurationsResult.listOf(BusinessObjectConfigurations.class) : null;
		if (null != businessObjectConfigurations && !CollectionUtils.isEmpty(businessObjectConfigurations)) {
			for (int i = 0; i < businessObjectConfigurations.size(); i++) {
				if (businessObjectConfigurations.get(i).getBusinessObjectAttributeCode()
						.equalsIgnoreCase(Supplier8DProcesses.SUPPLIER_ISSUE_PROCESSES_TYPE)) {

					eightD.setSupplierIssueProcessesType(businessObjectConfigurations.get(i).getBusinessObjectValue());
				}
			}
		}
		LoggerHelper.logMethodExit(logger, SUPPLIER_8D_SERVICE_IMPL, "setConfiguredValues");
	}

	/**
	 * Validate mandatory fields for supplier 8D and validate field editability
	 */
	@Override
	public void validate8DDetails(Supplier8DProcesses eightD) {
		LoggerHelper.logMethodEntry(logger, SUPPLIER_8D_SERVICE_IMPL, "validate8DDetails");
		eightDValidator.validateEightDFields(eightD);
		if (messages.stream().noneMatch(message -> message.getSeverity() == Message.Severity.ERROR)) {
			eightDValidator.validateFieldControlEightD(eightD);
		}
		LoggerHelper.logMethodExit(logger, SUPPLIER_8D_SERVICE_IMPL, "validate8DDetails");
	}

	/**
	 * Get Supplier 8D details based on its ID
	 */
	@Override
	public Supplier8DProcesses getEightDDetails(String eightDId) {
		LoggerHelper.logMethodEntry(logger, SUPPLIER_8D_SERVICE_IMPL, GET_EIGHTD_DETAILS);
		Result eightDDetails = eightDDao.getEightDBasedOnId(eightDId);
		LoggerHelper.logMethodExit(logger, SUPPLIER_8D_SERVICE_IMPL, GET_EIGHTD_DETAILS);
		return eightDDetails.first().isPresent() ? eightDDetails.single(Supplier8DProcesses.class) : null;
	}

    /**
	 * Get Supplier 8D based on its ID
	 */
	@Override
	public Supplier8DProcesses getEightDBasedOnId(String eightDId) {
		LoggerHelper.logMethodEntry(logger, SUPPLIER_8D_SERVICE_IMPL, GET_EIGHTD_DETAILS);
		Result eightDDetails = eightDDao.getEightD(eightDId);
		LoggerHelper.logMethodExit(logger, SUPPLIER_8D_SERVICE_IMPL, GET_EIGHTD_DETAILS);
		return eightDDetails.first().isPresent() ? eightDDetails.single(Supplier8DProcesses.class) : null;
	}

	/**
	 * Fetch Supplier 8D based on complaint ID
	 */
	@Override
	public Supplier8DProcesses getEightDDetailsBasedOnComplaintId(String complaintId) {
		LoggerHelper.logMethodEntry(logger, SUPPLIER_8D_SERVICE_IMPL, "getEightDDetailsBasedOnComplaintId");
		Result supplier8DDetailsBasedOnComplaintId = eightDDao.getEightDDetailsBasedOnComplaintId(complaintId);
		LoggerHelper.logMethodExit(logger, SUPPLIER_8D_SERVICE_IMPL, "getEightDDetailsBasedOnComplaintId");
		return supplier8DDetailsBasedOnComplaintId.first().isPresent()
				? supplier8DDetailsBasedOnComplaintId.single(Supplier8DProcesses.class)
				: null;
	}

	/**
	 * Check if Supplier 8D is created for the complaint and check if Supplier 8D is
	 * relevant
	 */
	@Override
	public void validateIf8DExistsForComplaint(String complaintId) {
		LoggerHelper.logMethodEntry(logger, SUPPLIER_8D_SERVICE_IMPL, "validateIf8DExistsForComplaint");
		eightDValidator.validateIf8DExistsForComplaint(complaintId);
		if (messages.stream().noneMatch(message -> message.getSeverity() == Message.Severity.ERROR)) {
			claimValidation.validateIfQualityNotificationExists(complaintId, Constants.SUPPLIER_EIGHTD_CODE);
		}
		if (messages.stream().noneMatch(message -> message.getSeverity() == Message.Severity.ERROR)) {
			eightDValidator.validateifBOIsRelevant(complaintId, Constants.SUPPLIER_EIGHTD_CODE);
		}
		LoggerHelper.logMethodExit(logger, SUPPLIER_8D_SERVICE_IMPL, "validateIf8DExistsForComplaint");
	}

	/**
	 * Check if Supplier 8D exists and validate field editability
	 */
	@Override
	public void validateIf8DExists(Supplier8DProcesses eightD) {
		LoggerHelper.logMethodEntry(logger, SUPPLIER_8D_SERVICE_IMPL, "validateIf8DExists");
		eightDValidator.validateIf8DExists(eightD.getId());
		if (messages.stream().noneMatch(message -> message.getSeverity() == Message.Severity.ERROR)) {
			eightDValidator.validateFieldControlEightD(eightD);
		}
		LoggerHelper.logMethodExit(logger, SUPPLIER_8D_SERVICE_IMPL, "validateIf8DExists");
	}

	/**
	 * Sync the 8D status with the backend 8D status
	 */
	@Override
	public void mapEightDStatus() {
		LoggerHelper.logMethodEntry(logger, SUPPLIER_8D_SERVICE_IMPL, MAP_EIGHTD_STATUS);
		logger.info("*********Inside mapEightDStatus*********");
		APIQMSIPSRVService service = new DefaultAPIQMSIPSRVService();
		List<cds.gen.supplierissueprocessinternalservice.Supplier8DProcesses> supplier8DProcessesList = eightDDao
				.getActiveEightD();
		List<SupplierIssueProcessStatusMappings> allSupplierIssueProcessMappings = businessObjectDao
				.getAllSupplierIssueProcessMappings();
		logger.info("SupplierIssueProcessStatusMappings Size :: {}  ", allSupplierIssueProcessMappings.size());

		for (cds.gen.supplierissueprocessinternalservice.Supplier8DProcesses supplier8DProcess : supplier8DProcessesList) {
			try {
				logger.info("EightD Identifier: {} ", supplier8DProcess.getIdentifier());
				Result eightDestinationConfig = destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(
						supplier8DProcess.getCompanyId(), Constants.SUPPLIER_EIGHTD_CODE);

				if (eightDestinationConfig.first().isPresent()) {
					ScpCfDestinationLoader scpCfDestinationLoader = new ScpCfDestinationLoader();
					HttpDestination destination = destinationService.getHttpDestination(scpCfDestinationLoader,
							eightDestinationConfig.first().get().get(Constants.DESTINATION).toString());

					SupplierIssueProcesses supplierIssueProcesses = service
							.getSupplierIssueProcessesByKey(supplier8DProcess.getIdentifier())
							.executeRequest(destination);
					String processStatus = supplierIssueProcesses.getProcessStatusId(); // lifecycle-F1
					String confirmationStatus = supplierIssueProcesses.getConfirmationStatusId();// confirmation-F2
					Map<String, String> statusMap = new LinkedHashMap<>();
					statusMap.put(Constants.CONFIRMATION_STATUS_CODE, confirmationStatus);
					statusMap.put(Constants.LIFECYCLE_STATUS_CODE, processStatus);
					logger.info("EightD Identifier ( {} ",
							supplier8DProcess.getIdentifier() + ") Lifecycle status: {} " , processStatus);
					logger.info("EightD Identifier ( {} ",
							supplier8DProcess.getIdentifier() + ") Confirmation status: {} " , confirmationStatus);
					processEighDStatusesAndUpdate(allSupplierIssueProcessMappings, supplier8DProcess, statusMap);
				}
			} catch (Exception e) {
				// Exception
				logger.info("8D could not be updated");
				LoggerHelper.logExceptionWithMessage(logger, "Exception while fetching Supplier 8D details ", e);
				continue;
			}
		}
		LoggerHelper.logMethodExit(logger, SUPPLIER_8D_SERVICE_IMPL, MAP_EIGHTD_STATUS);
	}

	/**
	 * Process each status(Lifecycle/Confirmation status) and insert into BO status
	 * if status does not exist
	 * 
	 * @param allSupplierIssueProcessMappings
	 * @param supplier8DProcess
	 * @param statusMap
	 */
	public void processEighDStatusesAndUpdate(List<SupplierIssueProcessStatusMappings> allSupplierIssueProcessMappings,
			cds.gen.supplierissueprocessinternalservice.Supplier8DProcesses supplier8DProcess,
			Map<String, String> statusMap) {
		LoggerHelper.logMethodEntry(logger, SUPPLIER_8D_SERVICE_IMPL, MAP_EIGHTD_STATUS);
		statusMap.forEach((key, value) -> {
			SupplierIssueProcessStatusMappings supplierIssueEntry = allSupplierIssueProcessMappings.stream()
					.filter(supplierStatus -> value.equalsIgnoreCase(supplierStatus.getCode())
							&& key.equalsIgnoreCase(supplierStatus.getFieldName()))
					.findAny().orElse(null);
			if (null != supplierIssueEntry) {
				Result result = businessObjectDao.findBOStatusCode(supplier8DProcess.getId(),
						supplierIssueEntry.getStatusCode());
				List<BusinessObjectStatuses> boStatuses = result.first().isPresent()
						? result.listOf(BusinessObjectStatuses.class)
						: new ArrayList<>();
				logger.info("Size of Business Object status for RPO: ", boStatuses.size());
				if (CollectionUtils.isEmpty(boStatuses)) {
					logger.info("Field name : " + key + "and value is :" + value);
					Result statusCodeResult = businessObjectDao.findEightDStatusMappingbyCode(value, key);
					String statusCode = statusCodeResult.first().isPresent()
							? statusCodeResult.list().get(0).get(SupplierIssueProcessStatusMappings.STATUS_CODE)
									.toString()
							: "";
					supplier8DProcess.setStatusCode(statusCode);
					CqnUpdate update = Update.entity(Supplier8DProcesses_.class).data(supplier8DProcess);
					cdsService.run(update)
							.listOf(cds.gen.supplierissueprocessinternalservice.Supplier8DProcesses.class);
				}
			}

		});
		LoggerHelper.logMethodEntry(logger, SUPPLIER_8D_SERVICE_IMPL, MAP_EIGHTD_STATUS);
	}

	@Override
	public Supplier8DProcesses getDraftEightDByComplaintID(String complaintId) {
		Result result = eightDDao.getDraftEightDByComplaintID(complaintId);
		return (result != null && result.first().isPresent()) ? result.listOf(Supplier8DProcesses.class).get(0) : null;
	}

	@Override
	public void deleteDraftEightDByID(String eightDId) {
		eightDDao.deleteDraftEightDByID(eightDId);
	}

}