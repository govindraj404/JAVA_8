package com.sap.ic.cmh.drm.service;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.ql.CQL;
import com.sap.cds.ql.Select;
import com.sap.cds.ql.Update;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.complaint.persistency.ComplaintsDao;
import com.sap.ic.cmh.configuration.persistency.ConfigurationDao;
import com.sap.ic.cmh.masterdata.businesspartner.repository.BusinessPartnerRepository;
import com.sap.ic.cmh.masterdata.businesspartner.service.BusinessPartnerService;
import com.sap.ic.cmh.masterdata.companycode.repository.CompanyCodeRepository;
import com.sap.ic.cmh.drm.helper.RetentionManagerHelper;
import com.sap.ic.cmh.drm.model.DataSubjectLastRetentionStartDatesRequest;
import com.sap.ic.cmh.drm.model.DataSubjectLastRetentionStartDatesResponse;
import com.sap.ic.cmh.drm.model.DataSubjectRequest;
import com.sap.ic.cmh.drm.model.DataSubjectResidence;
import com.sap.ic.cmh.drm.model.DataSubjectResponse;
import com.sap.ic.cmh.drm.model.DataSubjectsEndofResidenceRequest;
import com.sap.ic.cmh.drm.model.DataSubjectsEndofResidenceResponse;
import com.sap.ic.cmh.drm.model.LegalGroundResidenceRules;
import com.sap.ic.cmh.drm.model.ResidenceRuleConditionSet;
import com.sap.ic.cmh.drm.model.ValueHelpResponse;
import com.sap.ic.cmh.utils.GenericUtils;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import cds.gen.complaintservice.CommonBusinessObjects_;
import cds.gen.complaintservice.CompanyCodes_;
import cds.gen.claimservice.Claims_;
import cds.gen.complaintservice.CommonBusinessObjects;
import cds.gen.complaintservice.Complaints;
import cds.gen.complaintservice.Complaints_;
import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.masterdataservice.BusinessPartners_;
import cds.gen.qualitynotificationservice.QualityNotifications;
import cds.gen.qualitynotificationservice.QualityNotifications_;
import cds.gen.returnpurchaseorderservice.ReturnPurchaseOrders_;
import cds.gen.supplierissueprocessservice.SupplierIssueProcessStatuses_;
import org.slf4j.Logger;

@Service
public class RetentionManagerServiceImpl implements RetentionManagerService {

	public static final String DATA_SUBJECT_ROLE = "BTP Users";
	public static final String BUSINESS_PARTNER_DATA_SUBJECT_ROLE = "Business Partner";
	public static final String LEGAL_ENTITY_DESCRIPTION = "Company Code";
	private static final String DATE_FORMATER2 = "yyyy-MM-dd";
	private static final String COMPLAINT_LEGAL_GROUND="Complaints";
	private static final String COMPLAINT_BO_LEGAL_GROUND = "Complaints and Business Objects";
	private static final String MASK_VALUE = "XXXX";

	@Autowired
	private PersistenceService persistenceService;
	@Autowired
	ConfigurationDao configDao;
	@Autowired
	BusinessPartnerService businessPartnerService;
	@Autowired
    ComplaintsDao complaintsDao;
    @Autowired
    CompanyCodeRepository companyCodeRepository;
    @Autowired
    BusinessPartnerRepository businessPartnerRepository;

	public static final Logger log = LoggerHelper.getLogger(RetentionManagerServiceImpl.class);
	private static final String RETENTION_SERVICE_IMPL = "RetentionManagerServiceImpl";

	@Override
	public List<ValueHelpResponse> getLegalEntities(String dataSubjectRole) {
		LoggerHelper.logMethodEntry(log, RETENTION_SERVICE_IMPL, "getLegalEntities");
        log.info("getLegalEntities request ");
		List<ValueHelpResponse> valueHelpResponses = new ArrayList<>();
		// validations
		if (!isValidDataSubjectRole(dataSubjectRole)) {
			return Collections.emptyList();
		}
		ValueHelpResponse valueHelpResponse;
		// Based on data subject role get the legal entities
		List<String> legalEntitiesResponseList;
		if (dataSubjectRole.equalsIgnoreCase(DATA_SUBJECT_ROLE)) {
            log.info("Getting BTP Users ");
			legalEntitiesResponseList = readAllLegalEntitiesBTPUsers();
		} else {
            log.info("Getting Business partners ");
			legalEntitiesResponseList = readAllLegalEntitiesBusinessPartners();
		}
 
		for (String legalEntity : legalEntitiesResponseList) {
			valueHelpResponse = new ValueHelpResponse();
			valueHelpResponse.setValue(legalEntity);
			valueHelpResponse
					.setValueDesc(LEGAL_ENTITY_DESCRIPTION);
            log.info("Description is : {} ",valueHelpResponse.getValueDesc());
			valueHelpResponses.add(valueHelpResponse);
		}
		log.info("Legal Entity size:{}  ", valueHelpResponses.size());
		LoggerHelper.logMethodExit(log, RETENTION_SERVICE_IMPL, "getLegalEntities");
		return valueHelpResponses;
	}

	/**
	 * Read all non masked BTP Users in complaints
	 * 
	 * @return
	 */
	private List<String> readAllLegalEntitiesBTPUsers() {
		LoggerHelper.logMethodEntry(log, RETENTION_SERVICE_IMPL, "readAllLegalEntitiesBTPUsers");
		List<String> companyCodesList = new ArrayList<>();
        Result btpUsersResult = persistenceService.run(Select.from(Complaints_.class)
        .columns(c->c.companyCode().expand(CompanyCodes_::companyCode)).distinct()
        .where(CQL.get(Complaints.PERSON_RESPONSIBLE_ID).ne(MASK_VALUE)));
        List<Complaints> complaintList = btpUsersResult.first().isPresent() ? btpUsersResult.listOf(Complaints.class) : new ArrayList<>();
        if(!CollectionUtils.isEmpty(complaintList)){
          companyCodesList =  complaintList.stream().filter(c->null!=c.getCompanyCode()).map(c->c.getCompanyCode().getCompanyCode())
        .collect(Collectors.toList());
        }
		LoggerHelper.logMethodExit(log, RETENTION_SERVICE_IMPL, "readAllLegalEntitiesBTPUsers");
		return companyCodesList;
	}

	/**
	 * Read all non masked business partner attributes and get its respective
	 * business partner numbers
	 */
	private List<String> readAllLegalEntitiesBusinessPartners() {
        LoggerHelper.logMethodEntry(log, RETENTION_SERVICE_IMPL, "readAllLegalEntitiesBusinessPartners");
        log.info("Inside readAllLegalEntitiesBusinessPartners");
		Result bpResultQN = persistenceService.run(Select.from(CommonBusinessObjects_.CDS_NAME)
				.columns(CommonBusinessObjects.SUPPLIER_ID, CommonBusinessObjects.PERSON_RESPONSIBLE_ID,
						CommonBusinessObjects.CONTACT_PERSON_ID)
				.distinct()
				.where(CQL.get(CommonBusinessObjects.SUPPLIER_ID).ne(MASK_VALUE)
						.or(CQL.get(CommonBusinessObjects.PERSON_RESPONSIBLE_ID).ne(MASK_VALUE))
                        .or(CQL.get(CommonBusinessObjects.CONTACT_PERSON_ID).ne(MASK_VALUE))));
        Result complaintResult = persistenceService.run(Select.from(Complaints_.CDS_NAME)
				.columns(Complaints.SUPPLIER_ID,
						Complaints.CONTACT_PERSON_ID)
				.distinct()
				.where(CQL.get(Complaints.SUPPLIER_ID).ne(MASK_VALUE)
                        .or(CQL.get(Complaints.CONTACT_PERSON_ID).ne(MASK_VALUE))));
		LoggerHelper.logMethodExit(log, RETENTION_SERVICE_IMPL, "readAllLegalEntitiesBusinessPartners");
          List<String> filteredBusinessPartnerList = getBusinessPartnersUsedInBOs(bpResultQN,complaintResult);
        return getBusinessPartnerCompanyCodes(filteredBusinessPartnerList);
	}

	/**
	 * Validate if valid data subject role
	 */
	private boolean isValidDataSubjectRole(String dataSubjectRole) {
		return DATA_SUBJECT_ROLE.equalsIgnoreCase(dataSubjectRole) ? DATA_SUBJECT_ROLE.equalsIgnoreCase(dataSubjectRole)
				: BUSINESS_PARTNER_DATA_SUBJECT_ROLE.equalsIgnoreCase(dataSubjectRole);
	}

	@Override
	public DataSubjectsEndofResidenceResponse getDataSubjectEndOfResidence(
			DataSubjectsEndofResidenceRequest dataSubjectsEndofResidenceRequest) {
		LoggerHelper.logMethodEntry(log, RETENTION_SERVICE_IMPL, "getDataSubjectEndOfResidence");
		DataSubjectsEndofResidenceResponse dataSubjectsEndofResidenceResponse = new DataSubjectsEndofResidenceResponse();
        log.info("Inside getDataSubjectEndOfResidence!!");
		Set<DataSubjectResponse> dataSubjectResponseExpiredList = new HashSet<>();
		Set<DataSubjectResponse> dataSubjectResponseNotExpiredList = new HashSet<>();
		for (LegalGroundResidenceRules legalGroundResidenceRules : dataSubjectsEndofResidenceRequest
				.getLegalGroundResidenceRules()) {
			if (legalGroundResidenceRules != null) {
				for (ResidenceRuleConditionSet residenceRuleConditionSet : legalGroundResidenceRules
						.getRuleConditionSet()) {
                   log.info("Before getEndofResidenceDataSubjectsList!!"); 
					getEndofResidenceDataSubjectsList(dataSubjectResponseExpiredList, dataSubjectResponseNotExpiredList,
							legalGroundResidenceRules.getLegalEntity(), residenceRuleConditionSet,
                            dataSubjectsEndofResidenceRequest.getLegalGround());
                    log.info("After getEndofResidenceDataSubjectsList!!"); 
                    dataSubjectsEndofResidenceResponse.setExpiredDataSubjects(dataSubjectResponseExpiredList);
                    dataSubjectsEndofResidenceResponse.setDataSubjectBadRequests(dataSubjectResponseNotExpiredList);
				}
			} else {
				log.info("legalGroundResidenceRules is null");
			}
		}
		LoggerHelper.logMethodExit(log, RETENTION_SERVICE_IMPL, "getDataSubjectEndOfResidence");
		return dataSubjectsEndofResidenceResponse;
	}

	public void getEndofResidenceDataSubjectsList(Set<DataSubjectResponse> dataSubjectsExpired,
			Set<DataSubjectResponse> dataSubjectsNotExpired, String legalEntity,
			ResidenceRuleConditionSet residenceRuleCondition, String legalGround) {
		LoggerHelper.logMethodEntry(log, RETENTION_SERVICE_IMPL, "getEndofResidenceDataSubjectsList");
		DataSubjectResponse dataSubjectID = null;
		Set<DataSubjectResidence> allDataSubjects = readAllDataSubjects(legalEntity, legalGround);
		for (DataSubjectResidence dataSubjectResidence : allDataSubjects) {
			dataSubjectID = new DataSubjectResponse();
			if (dataSubjectResidence.getDataSubjectReferenceDate() != null) {
				Boolean residencyCheck = RetentionManagerHelper.residencyCheck(
						residenceRuleCondition.getResidenceDate(), dataSubjectResidence.getDataSubjectReferenceDate());
                        if (Boolean.TRUE.equals(residencyCheck)) {
                    log.info("Data Subject Expired");
					dataSubjectID.setDataSubjectID(dataSubjectResidence.getDataSubjectId());
					dataSubjectsExpired.add(dataSubjectID);
				} else {
                    log.info("Data Subject Not Expired");
					dataSubjectID.setDataSubjectID(dataSubjectResidence.getDataSubjectId());
					dataSubjectsNotExpired.add(dataSubjectID);
				}
			}
		}
		LoggerHelper.logMethodExit(log, RETENTION_SERVICE_IMPL, "getEndofResidenceDataSubjectsList");
	}

	/**
	 * Read All Data Subjects
	 */
	private Set<DataSubjectResidence> readAllDataSubjects(String legalEntity, String legalGround) {
		LoggerHelper.logMethodEntry(log, RETENTION_SERVICE_IMPL, "readAllDataSubjects");
		Set<DataSubjectResidence> dataSubjectResidences = new HashSet<>();
        DataSubjectResidence dataSubjectResidence = new DataSubjectResidence();
		if (legalGround.equalsIgnoreCase(COMPLAINT_LEGAL_GROUND)) {
			readAllDataSubjectsComplaints(legalEntity, dataSubjectResidences, dataSubjectResidence);
		} else if (legalGround.equalsIgnoreCase(COMPLAINT_BO_LEGAL_GROUND)) {
			readAllBusinessPartners(legalEntity,dataSubjectResidences, dataSubjectResidence);
		} else {
			log.info("Legalground does not match  ");
			GenericUtils.sanitize(legalGround);
		}
		LoggerHelper.logMethodExit(log, RETENTION_SERVICE_IMPL, "readAllDataSubjects");
		return dataSubjectResidences;
	}

	private void readAllBusinessPartners(String legalEntity,Set<DataSubjectResidence> dataSubjectResidences,
			DataSubjectResidence dataSubjectResidence) {
		LoggerHelper.logMethodEntry(log, RETENTION_SERVICE_IMPL, "readAllBusinessPartners");
		Result commonBusinessObjectsResult = persistenceService.run(Select.from(CommonBusinessObjects_.CDS_NAME)
				.columns(CommonBusinessObjects.PERSON_RESPONSIBLE_ID, CommonBusinessObjects.SUPPLIER_ID,
						CommonBusinessObjects.CONTACT_PERSON_ID));
        	Result complaintsResult = persistenceService.run(Select.from(Complaints_.CDS_NAME)
				.columns(Complaints.SUPPLIER_ID,
						Complaints.CONTACT_PERSON_ID, Complaints.MODIFIED_AT));
		if (commonBusinessObjectsResult.first().isPresent()) {
			List<String> filteredBusinessPartnerList = getBusinessPartnersUsedInBOs(commonBusinessObjectsResult,complaintsResult);
            List<String> businessPartnerList = getBusinessPartnerNumbers(filteredBusinessPartnerList,legalEntity);
            List<Complaints> listOf = complaintsResult.listOf(Complaints.class);
			for (int i = 0; i < listOf.size(); i++) {
				for (int j = 0; j < businessPartnerList.size(); j++) {
                    log.info("Reading All Business Partners");
					dataSubjectResidence.setDataSubjectId(businessPartnerList.get(j));
					dataSubjectResidence
							.setDataSubjectReferenceDate(listOf.get(i).get(Complaints.MODIFIED_AT).toString());
					dataSubjectResidences.add(dataSubjectResidence);
				}
			}

		}

		log.info("dataSubjectResidences size for readAllBusinessPartners :{} ", dataSubjectResidences.size());
		LoggerHelper.logMethodExit(log, RETENTION_SERVICE_IMPL, "readAllBusinessPartners");
	}

	private void readAllDataSubjectsComplaints(String legalEntity, Set<DataSubjectResidence> dataSubjectResidences,
			DataSubjectResidence dataSubjectResidence) {
		LoggerHelper.logMethodEntry(log, RETENTION_SERVICE_IMPL, "readAllDataSubjectsComplaints");

        Map<String, String> matcher = new HashMap<>();
        String companyCodeId = companyCodeRepository.getCompanyCodeIdBasedOnCode(legalEntity);
		matcher.put(Complaints.COMPANY_CODE_ID, companyCodeId);
		Result btpUserResult = persistenceService.run(Select.from(Complaints_.CDS_NAME).matching(matcher)
				.columns(Complaints.PERSON_RESPONSIBLE_ID, Complaints.MODIFIED_AT));

		if (btpUserResult.first().isPresent()) {
			List<Complaints> complaintsList = btpUserResult.listOf(Complaints.class);
			for (int i = 0; i < complaintsList.size(); i++) {
				dataSubjectResidence
						.setDataSubjectId((String) complaintsList.get(i).get(Complaints.PERSON_RESPONSIBLE_ID));
				dataSubjectResidence
						.setDataSubjectReferenceDate(complaintsList.get(i).get(Complaints.MODIFIED_AT).toString());
				dataSubjectResidences.add(dataSubjectResidence);
			}
			log.info("dataSubjectResidences size for readAllDataSubjectsComplaints :{} ", dataSubjectResidences.size());
		}

		LoggerHelper.logMethodExit(log, RETENTION_SERVICE_IMPL, "readAllDataSubjectsComplaints");
	}

	/**
	 * Get the Business partners that are used in all business objects
	 * 
	 * @param commonBusinessObjectResult
	 * @return
	 */
	public List<String> getBusinessPartnersUsedInBOs(Result commonBusinessObjectResult,Result complaintsResult) {
		LoggerHelper.logMethodEntry(log, RETENTION_SERVICE_IMPL, "getBusinessPartnersUsedInBOs");
		List<String> businessPartnerIdList = new ArrayList<>();
		if (commonBusinessObjectResult.first().isPresent()) {
			List<CommonBusinessObjects> businessObjectRows = commonBusinessObjectResult.listOf(CommonBusinessObjects.class);
			if (!CollectionUtils.isEmpty(businessObjectRows)) {
				List<String> supContactList = businessObjectRows.stream().filter(c->null!=c.getContactPersonId()).map(CommonBusinessObjects::getContactPersonId)
						.collect(Collectors.toList());
				List<String> suppliertList = businessObjectRows.stream().filter(c->null!=c.getSupplierId()).map(CommonBusinessObjects::getSupplierId)
						.collect(Collectors.toList());
				List<String> personResponsibleList = businessObjectRows.stream().filter(c->null!=c.getPersonResponsibleId())
						.map(CommonBusinessObjects::getPersonResponsibleId).collect(Collectors.toList());
				businessPartnerIdList.addAll(supContactList);
				businessPartnerIdList.addAll(suppliertList);
                businessPartnerIdList.addAll(personResponsibleList);
                log.info("Business partner list size :: {} ",businessPartnerIdList.size());
			}
        }
        
        if(complaintsResult.first().isPresent()){
          List<Complaints> complaintsList = complaintsResult.listOf(Complaints.class);
          if (!CollectionUtils.isEmpty(complaintsList)) {
				List<String> supContactList = complaintsList.stream().filter(c->null!=c.getContactPersonId()).map(Complaints::getContactPersonId)
                        .collect(Collectors.toList());
                log.info("supContactList is :: {} ",supContactList.size());
				List<String> suppliertList = complaintsList.stream().filter(c->null!=c.getSupplierId()).map(Complaints::getSupplierId)
                        .collect(Collectors.toList());
                log.info("suppliertList is :: {} ",suppliertList.size());
				businessPartnerIdList.addAll(supContactList);
                businessPartnerIdList.addAll(suppliertList);
                log.info("Complaints businessPartnerIdList size :: {} ",businessPartnerIdList.size());
			}
        }
        log.info("businessPartnerIdList size :: {} ",businessPartnerIdList.size());
         List<String> filteredBusinessPartnerList = new ArrayList<>(
      new HashSet<>(businessPartnerIdList));
        log.info("filteredBusinessPartnerList size :: {} ",filteredBusinessPartnerList.size());
		LoggerHelper.logMethodExit(log, RETENTION_SERVICE_IMPL, "getBusinessPartnersUsedInBOs");
		return filteredBusinessPartnerList;
	}

	/**
	 * Based on the Business partner ids, get the Business partner numbers
	 */
	public List<String> getBusinessPartnerNumbers(List<String> filteredBusinessPartnerList,String legalEntity) {
        
        String companyCodeId = companyCodeRepository.getCompanyCodeIdBasedOnCode(legalEntity);
		Result businessPartnerResult = persistenceService
				.run(Select.from(BusinessPartners_.CDS_NAME).columns(BusinessPartners.BUSINESS_PARTNER_NUMBER)
                        .where(b -> b.get(BusinessPartners.ID).in(filteredBusinessPartnerList)
                        .and(b.get(BusinessPartners.COMPANY_CODE_ID_ID).eq(companyCodeId))));
        
		return null!=businessPartnerResult && businessPartnerResult.first().isPresent()
				? businessPartnerResult.listOf(BusinessPartners.class).stream()
						.map(BusinessPartners::getBusinessPartnerNumber).collect(Collectors.toList())
				: new ArrayList<>();
    }
    
    /**
	 * Based on the Business partner ids, get the Company code ids
	 */
	public List<String> getBusinessPartnerCompanyCodes(List<String> filteredBusinessPartnerList) {
		Result businessPartnerResult = persistenceService
                .run(Select.from(BusinessPartners_.class)
                .columns(bp->bp.companyCodeID().expand(cds.gen.masterdataservice.CompanyCodes_::companyCode))
                .distinct()
                .where(bp -> bp.get(BusinessPartners.ID).in(filteredBusinessPartnerList)));
        
		return null!=businessPartnerResult && businessPartnerResult.first().isPresent()
				? businessPartnerResult.listOf(BusinessPartners.class).stream().filter(bp->null!=bp.getCompanyCodeID())
						.map(bp->bp.getCompanyCodeID().getCompanyCode()).collect(Collectors.toList())
				: new ArrayList<>();
	}

	@Override
	public List<DataSubjectLastRetentionStartDatesResponse> getDataSubjectLastRetentionStartDates(
			DataSubjectLastRetentionStartDatesRequest dataSubjectLastRetentionStartDatesRequest) {
		LoggerHelper.logMethodEntry(log, RETENTION_SERVICE_IMPL, "getDataSubjectLastRetentionStartDates");
		List<DataSubjectLastRetentionStartDatesResponse> dataSubjectRetentionStartDates = new ArrayList<>();
		String dataSubjectID = dataSubjectLastRetentionStartDatesRequest.getDataSubjectID();
		String retentionID = dataSubjectLastRetentionStartDatesRequest.getRulesConditionSet().get(0).getRetentionID();
		if (dataSubjectID == null || dataSubjectID.isEmpty()) {
			log.info("DataSubjectId is null");
			return dataSubjectRetentionStartDates;
		}
		try {
			Map<String, String> matcher = new HashMap<>();
			matcher.put(Complaints.PERSON_RESPONSIBLE_ID, GenericUtils.sanitize(dataSubjectID));
			Result btpUserResult = persistenceService
					.run(Select.from(Complaints_.CDS_NAME).matching(matcher).columns(Complaints.MODIFIED_AT));
            List<Row> btpUserRows = btpUserResult.list();
			if (!CollectionUtils.isEmpty(btpUserRows)) {
                log.info("btpUserRows size for getDataSubjectLastRetentionStartDates  :: {} ",btpUserRows.size());
                String createdAt = btpUserRows.get(0).get(Complaints.MODIFIED_AT).toString();
				String retentionStartDate = convertLocalDate(createdAt);
				dataSubjectRetentionStartDates.add(new DataSubjectLastRetentionStartDatesResponse(
						GenericUtils.sanitize(retentionID), retentionStartDate));
            }else{
            Result supplierResult = configDao.getSupplierDataBasedOnNumber(GenericUtils.sanitize(dataSubjectID));
			BusinessPartners supplierDataBasedOnNumber = supplierResult.first().isPresent()
					? supplierResult.listOf(BusinessPartners.class).get(0)
                    : null;
            if(null!=supplierDataBasedOnNumber){
                Result complainResult = persistenceService
                    .run(Select.from(Complaints_.CDS_NAME).columns(Complaints.MODIFIED_AT)
                    .where(b -> b.get(Complaints.SUPPLIER_ID).eq(supplierDataBasedOnNumber.getId())
                    .or(b.get(Complaints.CONTACT_PERSON_ID).eq(supplierDataBasedOnNumber.getId()))));
                List<Row> complaintRows = complainResult.list();
                if (!CollectionUtils.isEmpty(complaintRows)) {
                 log.info("complaintRows size for getDataSubjectLastRetentionStartDates  :: {} ",complaintRows.size());
				String createdAt = complaintRows.get(0).get(Complaints.MODIFIED_AT).toString();
                String retentionStartDate = convertLocalDate(createdAt);
				dataSubjectRetentionStartDates.add(new DataSubjectLastRetentionStartDatesResponse(
						GenericUtils.sanitize(retentionID), retentionStartDate));
               }
        }
            }
		} catch (ParseException e) {
			log.error("ParseException occured ");
		}
		LoggerHelper.logMethodExit(log, RETENTION_SERVICE_IMPL, "getDataSubjectLastRetentionStartDates");
		return dataSubjectRetentionStartDates;

	}

	private String convertLocalDate(String dataSubDate) throws ParseException {
		SimpleDateFormat formater = new SimpleDateFormat(DATE_FORMATER2);
		DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME;
		LocalDate localDate = new java.sql.Date(formater.parse(dataSubDate).getTime()).toLocalDate();
		LocalDateTime retentionStartDate = localDate.atTime(00, 00, 00);
		return dateTimeFormatter.format(retentionStartDate);
	}

	/**
	 * Mask the data subject id
	 */
	@Override
	public void deleteDataSubject(DataSubjectRequest dataSubjectRequest) throws ParseException {
		LoggerHelper.logMethodEntry(log, RETENTION_SERVICE_IMPL, "deleteDataSubject");
		if (dataSubjectRequest.getDataSubjectID() != null
				&& RetentionManagerHelper.isDeleteOrBlock(dataSubjectRequest.getMaxDeletionDate())) {
			log.info("Records with the given subject id are expired ");
			Result supplierResult = configDao.getSupplierDataBasedOnNumber(dataSubjectRequest.getDataSubjectID());
			BusinessPartners supplierDataBasedOnNumber = supplierResult.first().isPresent()
					? supplierResult.listOf(BusinessPartners.class).get(0)
					: null;
			if (null != supplierDataBasedOnNumber
					&& !businessPartnerService.businessPartnerUsedByAnyComplaint(supplierDataBasedOnNumber.getId())
					&& !businessPartnerService
							.businessPartnerUsedByAnyBusinessObject(supplierDataBasedOnNumber.getId())) {
				log.info("Data subject mask Business Partner");
				Map<String, String> matcher = new HashMap<>();

				updateEntities(supplierDataBasedOnNumber, matcher, QualityNotifications_.CDS_NAME);
				updateEntities(supplierDataBasedOnNumber, matcher, Claims_.CDS_NAME);
				updateEntities(supplierDataBasedOnNumber, matcher, SupplierIssueProcessStatuses_.CDS_NAME);
				updateEntities(supplierDataBasedOnNumber, matcher, ReturnPurchaseOrders_.CDS_NAME);
				if (!supplierDataBasedOnNumber.getBusinessPartnerType()
						.equalsIgnoreCase(Constants.EXCHANGE_PARTNER_TYPE_PERSON_RESPONSIBLE)) {
					updateEntities(supplierDataBasedOnNumber, matcher, Complaints_.CDS_NAME);
				}
			} else {
				checkForBTPUsers(dataSubjectRequest);
			}
		} else {
			log.info("Records with the given subject id are not expired ");
		}
		LoggerHelper.logMethodExit(log, RETENTION_SERVICE_IMPL, "deleteDataSubject");
	}

	// based on the business partner type of the Data subject,mask the
	// respective attribute
	private void updateEntities(BusinessPartners supplierDataBasedOnNumber, Map<String, String> matcher,
			String cdsName) {
		LoggerHelper.logMethodEntry(log, RETENTION_SERVICE_IMPL, "updateEntities");
		String businessPartnerType = supplierDataBasedOnNumber.getBusinessPartnerType();
		String attributeToBeMasked = getAttributeToBeMasked(businessPartnerType);
		matcher.put(attributeToBeMasked, supplierDataBasedOnNumber.getId());
		Result businessPartnerResult = persistenceService.run(Select.from(cdsName).matching(matcher).columns("ID"));
		List<Row> businessPartnerRows = businessPartnerResult.list();
		if (!CollectionUtils.isEmpty(businessPartnerRows)) {
			Map<String, String> updatedEntry = new HashMap<>();
			updatedEntry.put(attributeToBeMasked, MASK_VALUE);
			for (int i = 0; i < businessPartnerRows.size(); i++) {
				persistenceService.run(Update.entity(cdsName).data(updatedEntry).matching(matcher));
				log.info("Business Partners Records updated");
            }
            List<String> businessPartnerList = new ArrayList<>();
            businessPartnerList.add(supplierDataBasedOnNumber.getId());
            businessPartnerRepository.deleteBusinessPartnerList(businessPartnerList);
		}
		LoggerHelper.logMethodExit(log, RETENTION_SERVICE_IMPL, "updateEntities");
	}

	// get the attribute to be masked
	private String getAttributeToBeMasked(String businessPartnerType) {
		LoggerHelper.logMethodEntry(log, RETENTION_SERVICE_IMPL, "getAttributeToBeMasked");
		String attributeToBeMasked = "";
		switch (businessPartnerType) {
		case Constants.EXCHANGE_PARTNER_TYPE_SUPPLER:
			attributeToBeMasked = QualityNotifications.SUPPLIER_ID;
			break;
		case Constants.EXCHANGE_PARTNER_TYPE_SUPPLER_CONTACT:
			attributeToBeMasked = QualityNotifications.CONTACT_PERSON_ID;
			break;
		case Constants.EXCHANGE_PARTNER_TYPE_PERSON_RESPONSIBLE:
			attributeToBeMasked = QualityNotifications.PERSON_RESPONSIBLE_ID;
			break;
		default:
			break;
		}
		LoggerHelper.logMethodExit(log, RETENTION_SERVICE_IMPL, "getAttributeToBeMasked");
		return attributeToBeMasked;
	}

	// If the Data Subject is BTP user, mask the BTP user attribute in complaint
	// entity
	private void checkForBTPUsers(DataSubjectRequest dataSubjectRequest) {
		LoggerHelper.logMethodEntry(log, RETENTION_SERVICE_IMPL, "checkForBTPUsers");
		log.info("Data subject mask BTP User ");
		Result isBTPUserExistsActiveComplaint = complaintsDao
				.getActiveComplaintsForPersonResponsible(dataSubjectRequest.getDataSubjectID());
		if (!isBTPUserExistsActiveComplaint.first().isPresent()) {
			Map<String, String> matcher = new HashMap<>();
			matcher.put(Complaints.PERSON_RESPONSIBLE_ID, dataSubjectRequest.getDataSubjectID());
			Result btpUserResult = persistenceService
					.run(Select.from(Complaints_.CDS_NAME).matching(matcher).columns(Complaints.ID));
			List<Row> btpUserRows = btpUserResult.list();
			if (!CollectionUtils.isEmpty(btpUserRows)) {
				Map<String, String> updatedEntry = new HashMap<>();
				updatedEntry.put(Complaints.PERSON_RESPONSIBLE_ID, MASK_VALUE);
				for (int i = 0; i < btpUserRows.size(); i++) {
					persistenceService.run(Update.entity(Complaints_.CDS_NAME).data(updatedEntry).matching(matcher));
					log.info("BTP user records updated");
				}
			}
			LoggerHelper.logMethodExit(log, RETENTION_SERVICE_IMPL, "checkForBTPUsers");
		}
	}

}
