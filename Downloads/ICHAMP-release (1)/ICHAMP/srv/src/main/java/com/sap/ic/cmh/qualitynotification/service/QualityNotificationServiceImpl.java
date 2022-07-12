package com.sap.ic.cmh.qualitynotification.service;

import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import com.sap.ic.cmh.utils.QnValidation;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.sap.cds.Result;
import com.sap.cds.services.messages.Message.Severity;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.claim.model.ResponseModel;
import com.sap.ic.cmh.complaint.service.ComplaintService;
import com.sap.ic.cmh.configuration.model.MasterData;
import com.sap.ic.cmh.configuration.persistency.BusinessObjectConfigurationDao;
import com.sap.ic.cmh.configuration.persistency.DestinationConfigurationDao;
import com.sap.ic.cmh.configuration.service.ConfigurationService;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.masterdata.unitofmeasure.service.UnitOfMeasureService;
import com.sap.ic.cmh.network.service.HttpService;
import com.sap.ic.cmh.qualitynotification.model.QualityNotificationDTO;
import com.sap.ic.cmh.qualitynotification.model.SupplierPersonDetails;
import com.sap.ic.cmh.qualitynotification.persistency.QualityNotificationDao;
import com.sap.ic.cmh.qualitynotification.validations.QualityNotificationValidation;
import com.sap.ic.cmh.utils.CommonFunctions;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import cds.gen.configurationservice.BusinessObjectConfigurations;
import cds.gen.complaintservice.Complaints;
import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.masterdataservice.UnitOfMeasures;
import cds.gen.qualitynotificationservice.Defects;
import cds.gen.qualitynotificationservice.QualityNotifications;

@Service
public class QualityNotificationServiceImpl implements QualityNotificationService {

	@Autowired
	QualityNotificationDao qualityNotificationDao;

	@Autowired
	HttpService httpService;
	@Autowired
	ConfigurationService configService;
	@Autowired
	CommonFunctions commonFunctions;
	@Autowired
	DestinationConfigurationDao destinationConfigDao;
	@Autowired
	BusinessObjectConfigurationDao businessObjectConfigurationDao;
	@Autowired
	QualityNotificationValidation qualityNotificationValidation;
	@Autowired
	Messages messages;
	@Autowired
	UnitOfMeasureService unitOfMeasureService;

	@Autowired
	QnValidation qnValidation;
	@Autowired
	ComplaintService complaintService;

	private static final Logger logger = LoggerFactory.getLogger(QualityNotificationServiceImpl.class);
	private static final String QUALITY_NOTIF_SERVICE_IMPL = "QualityNotificationServiceImpl";

	/**
	 * Get the values from configuration and set it to the Quality Notification
	 * object
	 */
	@Override
	public void setConfiguredValues(QualityNotifications qn, String boType, String complaintTypeCode) {
		LoggerHelper.logMethodEntry(logger, QUALITY_NOTIF_SERVICE_IMPL, "setConfiguredValues");
		String destination = "";
		// get the configured destination based on company code id and business object
		// type
		Result result = destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(qn.getCompanyId(),
				Constants.QUALITYNOTIFICATION_CODE);

		destination = result.first().isPresent() ? result.list().get(0).get(Constants.DESTINATION).toString() : "";

		logger.info("Targeted destination for QN Creation is ", destination);
		// get the configured business object attributes based on complaint type
		// code,destination and business object type
		Result businessObjectConfigurationsResult = businessObjectConfigurationDao
				.getBusinessObjectConfigBasedOnDestinationAndBOAndDest(complaintTypeCode, boType, destination);

		List<BusinessObjectConfigurations> businessObjectConfigurations = businessObjectConfigurationsResult.first()
				.isPresent() ? businessObjectConfigurationsResult.listOf(BusinessObjectConfigurations.class) : null;
		if (null != businessObjectConfigurations && !CollectionUtils.isEmpty(businessObjectConfigurations)) {
			for (int i = 0; i < businessObjectConfigurations.size(); i++) {
				if (businessObjectConfigurations.get(i).getBusinessObjectAttributeCode()
						.equalsIgnoreCase(QualityNotifications.QN_TYPE)) {
					qn.setQnType(businessObjectConfigurations.get(i).getBusinessObjectValue());
				} else if (businessObjectConfigurations.get(i).getBusinessObjectAttributeCode()
						.equalsIgnoreCase(QualityNotifications.SUPPLIER_ROLE)) {
					qn.setSupplierRole(businessObjectConfigurations.get(i).getBusinessObjectValue());
				} else if (businessObjectConfigurations.get(i).getBusinessObjectAttributeCode()
						.equalsIgnoreCase(QualityNotifications.PERSON_RESPONSIBLE_ROLE)) {
					qn.setPersonResponsibleRole(businessObjectConfigurations.get(i).getBusinessObjectValue());
				}
			}
		}
		LoggerHelper.logMethodExit(logger, QUALITY_NOTIF_SERVICE_IMPL, "setConfiguredValues");
	}

	/**
	 * Create Quality Notification in the configured destination
	 */
	@Override
	public void createQualityNotification(QualityNotifications qn) {
		LoggerHelper.logMethodEntry(logger, QUALITY_NOTIF_SERVICE_IMPL, "createQualityNotification");
		String destination = "";
		String endPoint = Constants.QN_APPENDED_URL;
		ResponseModel responseModel = null;
		logger.info("Inside Create of QN method");
		QualityNotificationDTO qnRequest = createRequest(qn);
		Map<String, Object> qnRequestMap = commonFunctions.convertObjectToMap(qnRequest);
		Result result = destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(qn.getCompanyId(),
				Constants.QUALITYNOTIFICATION_CODE);
		destination = result.first().isPresent() ? result.list().get(0).get(Constants.DESTINATION).toString() : "";

		try {
			responseModel = httpService.callCpiFlow(endPoint, qnRequestMap, destination);

		} catch (IOException e) {
			messages.error(MessageKeys.ERROR_IN_CPI_FLOW.concat(e.toString()));
			messages.throwIfError();
		}
		if (null != responseModel) {
			setQualityDefectObjects(responseModel, qn);
			logger.info("[QualityNotificationServiceImpl][createQualityNotification] qn.getIdentifier {} ",
					qn.getIdentifier());
		} else {
			messages.throwIfError();
		}

		LoggerHelper.logMethodExit(logger, QUALITY_NOTIF_SERVICE_IMPL, "createQualityNotification");
	}

	/**
	 * Set the Quality Notification number and defect details
	 * 
	 * @param responseModel
	 * @param qn
	 */
	public void setQualityDefectObjects(ResponseModel responseModel, QualityNotifications qn) {
		LoggerHelper.logMethodEntry(logger, QUALITY_NOTIF_SERVICE_IMPL, "setQualityDefectObjects");
		qn.setIdentifier(String.format("%012d", Integer.parseInt(responseModel.getResult())));
		qn.setStatusCode(null != responseModel.getStatusCode() ? responseModel.getStatusCode() : "");
		// Set Defect BO details
		Defects defect = qn.getDefect();
		// Hardcoding the value of defect as 0001.Used for creation of Supplier 8D
		defect.setIdentifier(Constants.DEFECT_IDENTIFIER);
		LoggerHelper.logMethodExit(logger, QUALITY_NOTIF_SERVICE_IMPL, "setQualityDefectObjects");
	}

	/**
	 * Create Quality notification Request
	 * 
	 * @param qn
	 * @return
	 */
	public QualityNotificationDTO createRequest(QualityNotifications qn) {
		LoggerHelper.logMethodEntry(logger, QUALITY_NOTIF_SERVICE_IMPL, "createRequest");
		QualityNotificationDTO request = new QualityNotificationDTO();
		String isoCode = "";
		List<String> attributesList = new ArrayList<>();
        Map<String,Object> qualityNotificationMap = commonFunctions.convertObjectToMap(qn);
		MasterData masterData = configService.getMasterDataDetails(qn.getMaterialId(), qn.getPlantId(),
				qn.getSupplierId(), qn.getPurchasingOrganizationId());
		if (StringUtils.isNotBlank(qn.getUnit())) {
			UnitOfMeasures unitOfMeasureDetails = unitOfMeasureService.getUnitOfMeasureDetails(qn.getUnit());
			isoCode = null!=unitOfMeasureDetails ? unitOfMeasureDetails.getISOCode() : "";
			logger.info("ISO code for unit " + qn.getUnit() + "is : " + isoCode);
		}
		SupplierPersonDetails supplierDetails = new SupplierPersonDetails();
		supplierDetails.setCode(masterData.getSupplier().getBusinessPartnerNumber());
		supplierDetails.setRole(qn.getSupplierRole());
		request.setSupplierDetails(supplierDetails);
		BusinessPartners supplier = configService.getSupplier(qn.getPersonResponsibleId());
		String personResponsible = supplier.getBusinessPartnerNumber();
		SupplierPersonDetails personDetails = new SupplierPersonDetails();
		personDetails.setCode(personResponsible);
		personDetails.setRole(qn.getPersonResponsibleRole());
		request.setPersonDetails(personDetails);
		qualityNotificationMap.replace(QualityNotifications.MATERIAL_ID, qn.getMaterialId(), null != masterData.getMaterial() ? masterData.getMaterial().getMaterialCode() : "");
		qualityNotificationMap.replace(QualityNotifications.PLANT_ID, qn.getPlantId(), masterData.getPlants().getPlant());
		qualityNotificationMap.replace(QualityNotifications.QUANTITY, qn.getQuantity(), null != qn.getQuantity() ? qn.getQuantity() : BigDecimal.ZERO);
		qualityNotificationMap.replace(QualityNotifications.UNIT, qn.getUnit(), isoCode);

        addAttributesToBeRemoved(attributesList);
		commonFunctions.removeAttributes(attributesList, qualityNotificationMap);

		request.setQualityNotifications(qualityNotificationMap);
		LoggerHelper.logMethodExit(logger, QUALITY_NOTIF_SERVICE_IMPL, "createRequest");
		return request;

	}
    
    /**
     * Add Attributes to a list to be removed while sending 
     * to CPI
     */
    public void addAttributesToBeRemoved(List<String> attributesList) {
		attributesList.add(QualityNotifications.ID);
		attributesList.add(QualityNotifications.SUPPLIER_ID);
		attributesList.add(QualityNotifications.SUPPLIER_ROLE);
		attributesList.add(QualityNotifications.PERSON_RESPONSIBLE_ID);
		attributesList.add(QualityNotifications.PERSON_RESPONSIBLE_ROLE);
		attributesList.add(QualityNotifications.PURCHASING_ORGANIZATION_ID);
		attributesList.add(QualityNotifications.COMPANY_ID);
		attributesList.add(QualityNotifications.CONTACT_PERSON_ID);
		attributesList.add(QualityNotifications.INSPECTION_RESULT);
		attributesList.add(QualityNotifications.COMPLAINT_ID);
		attributesList.add(QualityNotifications.STATUS_CODE);
		attributesList.add(QualityNotifications.BUSINESS_OBJECT_STATUSES);
		attributesList.add(QualityNotifications.IS_QUALITY_NOTIFICATION_FIELD_CONTROL_MANDATORY);
		attributesList.add(QualityNotifications.IS_QUALITY_NOTIFICATION_FIELD_CONTROL);
		attributesList.add(QualityNotifications.IS_UPDATE_RESTRICTED);
		attributesList.add(QualityNotifications.S_NAVIGATION);
		attributesList.add(Defects.PERSON_RESPONSIBLE);
		attributesList.add(Defects.BUSINESS_OBJECT_STATUSES);
		attributesList.add(Defects.PARENT_ID);
	}

	/**
	 * @param qn
	 * @return update QualityNotification
	 */
	@Override
	public void updateQualityNotification(QualityNotifications qn) {
		LoggerHelper.logMethodEntry(logger, QUALITY_NOTIF_SERVICE_IMPL, "updateQualityNotification");
		logger.info("Update Quality Notification");
		qualityNotificationDao.updateQualityNotification(qn);
		LoggerHelper.logMethodExit(logger, QUALITY_NOTIF_SERVICE_IMPL, "updateQualityNotification");
	}

	/**
	 * Get QN details for a QN ID
	 */
	@Override
	public QualityNotifications getQualityNotificationDetails(String qnId) {
		Result qualityNotificationDetails = qualityNotificationDao.getQualityNotificationDetails(qnId);
		return qualityNotificationDetails.first().isPresent()
				? qualityNotificationDetails.listOf(QualityNotifications.class).get(0)
				: null;
	}

	/**
	 * Get Quality Notification details by a complaint ID
	 */
	@Override
	public QualityNotifications getQualityNotificationDetailsByComplaintId(String complaintId) {
		Result qualityNotificationDetailsByComplaintId = qualityNotificationDao
				.getQualityNotificationDetailsByComplaintId(complaintId);
		return qualityNotificationDetailsByComplaintId.first().isPresent()
				? qualityNotificationDetailsByComplaintId.listOf(QualityNotifications.class).get(0)
				: null;
	}

	/**
	 * Validate mandatory fields and validate field editability
	 */
	public void validateQNDetails(QualityNotifications qn) {
		LoggerHelper.logMethodEntry(logger, QUALITY_NOTIF_SERVICE_IMPL, "validateQNDetails");
		qualityNotificationValidation.validateMandatoryFields(qn);
		if (messages.stream().noneMatch(message -> message.getSeverity() == Severity.ERROR)) {
			qualityNotificationValidation.validateFieldControlQN(qn);
		}
		LoggerHelper.logMethodExit(logger, QUALITY_NOTIF_SERVICE_IMPL, "validateQNDetails");
	}

	/**
	 * Validate if quality notification exists for the complaint
	 */
	@Override
	public void validateIfQNExistsForComplaint(String complaintId) {
		LoggerHelper.logMethodEntry(logger, QUALITY_NOTIF_SERVICE_IMPL, "validateIfQNExists");
		qualityNotificationValidation.validateIfQNExistsForComplaint(complaintId);
		if (messages.stream().noneMatch(message -> message.getSeverity() == Severity.ERROR)) {
			qualityNotificationValidation.validateIfBOIsRelevant(complaintId, Constants.QUALITYNOTIFICATION_CODE);
		}

		LoggerHelper.logMethodExit(logger, QUALITY_NOTIF_SERVICE_IMPL, "validateIfQNExists");
	}

	/**
	 * Validate if quality notification exists
	 * and validate field control
	 */
	@Override
	public void validateQNExistsAndFieldControl(QualityNotifications qn) {
		qualityNotificationValidation.validateIfQNExists(qn.getId());
		if (messages.stream().noneMatch(message -> message.getSeverity() == Severity.ERROR)) {
			qualityNotificationValidation.validateFieldControlQN(qn);
		}

	}

	/**
	 * Get Defect for a QN ID
	 */
	@Override
	public cds.gen.qualitynotificationservice.Defects getDefectBasedOnQN(String qnId) {
		Result defectDetails = qualityNotificationDao.getDefectBasedOnQN(qnId);
		return defectDetails.first().isPresent()
				? defectDetails.listOf(cds.gen.qualitynotificationservice.Defects.class).get(0)
				: null;
	}

	/**
	 * Check if duplicate QN exists
	 * Delete the QN and complaint and associated BOs that got created through event
	 * mesh
	 * 
	 * @param qnNumber
	 */
	@Override
	public void checkIfDuplicateQNExistsAndDelete(String qnNumber) {
		LoggerHelper.logMethodEntry(logger, QUALITY_NOTIF_SERVICE_IMPL, "checkIfDuplicateQNExistsAndDelete");
		Result checkIfQNExistsBasedOnNumber = qualityNotificationDao.checkIfQNExistsBasedOnNumber(qnNumber);
		if (checkIfQNExistsBasedOnNumber.first().isPresent()) {
			List<QualityNotifications> listOf = checkIfQNExistsBasedOnNumber.listOf(QualityNotifications.class);
			if (null != listOf && listOf.size() > 1) {
				logger.info("Duplicate QN exists ");
				listOf.stream().forEach(qn -> {
					Complaints complaintDetails = complaintService
							.getComplaintCreationTypeAndCompanyCode(qn.getComplaintId());
					if (null != complaintDetails && StringUtils.isNotBlank(complaintDetails.getCreationType()) &&
							complaintDetails.getCreationType().equalsIgnoreCase(Constants.COMPLAINT_AUTOMATIC)) {
						logger.info("Delete Quality Notification");
						qualityNotificationDao.deleteAutomaticQN(qn.getComplaintId());
						logger.info("Delete Automatic Complaint");
						complaintService.deleteComplaints(qn.getComplaintId());
					}
				});
			}
		}
		LoggerHelper.logMethodExit(logger, QUALITY_NOTIF_SERVICE_IMPL, "checkIfDuplicateQNExistsAndDelete");
	}

	@Override
	public QualityNotifications getStatusAndCompanyCode(String qualityNotificationId) {
		Result qualityNotificationDetailsResult = qualityNotificationDao.getStatusAndCompanyCode(qualityNotificationId);
		return qualityNotificationDetailsResult.first().isPresent()
				? qualityNotificationDetailsResult.listOf(QualityNotifications.class).get(0)
				: null;
	}

	@Override
	public QualityNotifications getDraftQualityNotificationByComplaintID(String complaintId) {
		Result result = qualityNotificationDao.getDraftQNByComplaintID(complaintId);
		return (result != null && result.first().isPresent()) ? result.listOf(QualityNotifications.class).get(0) : null;
	}

	@Override
	public Defects getDraftDefectByQualityNotificationID(String qualityNotificationId) {
		Result result = qualityNotificationDao.getDraftDefectByQualityNotificationID(qualityNotificationId);
		return (result != null && result.first().isPresent()) ? result.listOf(Defects.class).get(0) : null;
	}

	@Override
	public void deleteDraftQualityNotificationByID(String qnId) {
		qualityNotificationDao.deleteDraftQNByID(qnId);
	}

	/**
	 * Check if QN Exists in DB based on number
	 */
	@Override
	public String checkIfQNExistsBasedOnNumber(String qnNumber) {
		Result checkIfQNExistsBasedOnNumber = qualityNotificationDao.checkIfQNExistsBasedOnNumber(qnNumber);
		return checkIfQNExistsBasedOnNumber.first().isPresent()
				? checkIfQNExistsBasedOnNumber.list().get(0).get(QualityNotifications.ID).toString()
				: "";
	}
}