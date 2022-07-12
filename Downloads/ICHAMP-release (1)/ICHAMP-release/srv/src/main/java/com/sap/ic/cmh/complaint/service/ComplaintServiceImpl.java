package com.sap.ic.cmh.complaint.service;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import cds.gen.complaintservice.*;
import com.sap.cloud.sdk.cloudplatform.connectivity.DestinationAccessor;
import com.sap.cloud.sdk.cloudplatform.connectivity.HttpDestination;
import com.sap.ic.cmh.businessobjects.persistency.BusinessObjectDao;
import com.sap.ic.cmh.masterdata.businesspartner.repository.BusinessPartnerRepository;
import com.sap.ic.cmh.masterdata.businesspartner.service.BusinessPartnerService;
import org.apache.commons.collections.CollectionUtils;
import org.slf4j.Logger;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.Constants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.ic.cmh.complaint.persistency.ComplaintsDao;
import com.sap.ic.cmh.complaint.validation.ComplaintValidation;
import com.sap.ic.cmh.configuration.service.ConfigurationService;
import com.sap.ic.cmh.costcollector.service.CostCollectorService;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.cds.services.messages.Messages;
import cds.gen.masterdataservice.BusinessPartners;
import sample.api.ScimUsersShadowUsersApi;
import sample.model.ScimUser;
import sample.model.ScimUsers;

@Service
public class ComplaintServiceImpl implements ComplaintService {

	@Autowired
	ComplaintsDao complaintDao;

	@Autowired
	CostCollectorService costCollectorService;

	@Autowired
	ConfigurationService configurationService;
	@Autowired
	BusinessPartnerRepository businessPartnerRepository;
	@Autowired
	ComplaintValidation complaintValidation;

	@Autowired
	BusinessPartnerService businessPartnerService;

	@Autowired
	BusinessObjectDao businessObjectDao;

	@Autowired
	Messages messages;

	public static final Logger logger = LoggerHelper.getLogger(ComplaintServiceImpl.class);
	private static final String COMPLAINT_SERVICE_IMPL = "ComplaintServiceImpl";

	/**
	 * Logic to calculate Total cost
	 *
	 * @param comp
	 */
	@Override
	public void updateComplaintWithCost(String comp, BigDecimal cost) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_SERVICE_IMPL, "updateComplaintWithCost");
		BigDecimal totalCost;
		if (cost == null) {
			updateComplaint(comp, null, null);
		}
		Complaints complaint = complaintDao.getComplaintDetails(comp).single(Complaints.class);
		if (complaint != null) {
			logger.info("***********The complaint is ::: {}", complaint);
			totalCost = complaint.getTotalSubLetCost();
			if (totalCost == null) {
				totalCost = new BigDecimal("0");
			}
			totalCost = totalCost.add(cost);
			logger.info("***********The total cost of complaint after adding is ::: {}", totalCost);
			if (totalCost.compareTo(BigDecimal.ZERO) < 0 || totalCost.compareTo(BigDecimal.ZERO) == 0)
				totalCost = new BigDecimal("0");
			updateComplaint(comp, null, totalCost);
			LoggerHelper.logMethodExit(logger, COMPLAINT_SERVICE_IMPL, "updateComplaintWithCost");
		}

	}

	/**
	 * Logic to calculate Total Labor hour
	 *
	 * @param comp
	 */
	@Override
	public void updateComplaintWithQuantity(String comp, BigDecimal quantity) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_SERVICE_IMPL, "updateComplaintWithQuantity");
		BigDecimal totalHour;

		if (quantity == null) {
			updateComplaint(comp, null, null);
		}

		Complaints complaint = complaintDao.getComplaintDetails(comp).single(Complaints.class);
		if (complaint != null) {
			totalHour = complaint.getTotalLaborHour();
			if (totalHour == null) {
				totalHour = new BigDecimal(0);
			}
			totalHour = totalHour.add(quantity);
			logger.info("***********The total quantity of complaint after adding is ::: {}", totalHour);
			if (totalHour.compareTo(BigDecimal.ZERO) < 0 || totalHour.compareTo(BigDecimal.ZERO) == 0)
				totalHour = new BigDecimal("0");
			updateComplaint(comp, totalHour, null);
			LoggerHelper.logMethodExit(logger, COMPLAINT_SERVICE_IMPL, "updateComplaintWithQuantity");
		}
	}

	/**
	 * Update complaint's Total cost and Total Labor hour
	 *
	 * @param comp
	 * @param totalHour
	 * @param totalCost
	 */
	private void updateComplaint(String comp, BigDecimal totalHour, BigDecimal totalCost) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_SERVICE_IMPL, "updateComplaint");
		Complaints complaint = getComplaintDetails(comp);
		if (complaint != null) {
			if (totalHour != null) {
				complaint.setTotalLaborHour(totalHour);
			}
			if (totalCost != null) {
				complaint.setTotalSubLetCost(totalCost);
			}
		}
		complaintDao.updateComplaint(complaint);
		LoggerHelper.logMethodExit(logger, COMPLAINT_SERVICE_IMPL, "updateComplaint");
	}

	/**
	 * Get Default Type
	 *
	 * @return ComplaintCategories
	 */

	@Override
	public ComplaintCategories getDefaultType() {
		Result defaultComplaintTypeResult = complaintDao.getDefaultType();
		return defaultComplaintTypeResult.first().isPresent()
				? defaultComplaintTypeResult.listOf(ComplaintCategories.class).get(0)
				: null;
	}

	/**
	 * Get Default Status
	 *
	 * @return ComplaintStatuses
	 */
	@Override
	public ComplaintStatuses getDefaultStatus() {
		Result defaultComplaintStatusResult = complaintDao.getDefaultStatus();
		return defaultComplaintStatusResult.first().isPresent()
				? defaultComplaintStatusResult.listOf(ComplaintStatuses.class).get(0)
				: null;
	}

	/**
	 * Update Complaint status
	 */
	@Override
	public void updateComplaintStatus(String id, String status) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_SERVICE_IMPL, "updateComplaintStatus");
		Complaints complaint = Struct.create(Complaints.class);
		complaint.setId(id);
		complaint.setComplaintStatusCode(status);
		complaintDao.updateComplaint(complaint);
		LoggerHelper.logMethodExit(logger, COMPLAINT_SERVICE_IMPL, "updateComplaintStatus");
	}

	/**
	 * Get complaint details based on ID
	 */
	@Override
	public Complaints getComplaintDetails(String id) {
		Result complaintDetailsResult = complaintDao.getComplaintDetails(id);
		return complaintDetailsResult.first().isPresent() ? complaintDetailsResult.listOf(Complaints.class).get(0)
				: null;
	}

	/**
	 * Get total complaints available in DB
	 */
	@Override
	public Result getAllComplaints() {
		return complaintDao.getAllComplaints();
	}

	/**
	 * Get the deletion flag for Business partner and check if Business partner to
	 * be deleted is associated with active complaints and delete the data in case
	 * of no active complaints
	 */
	@Override
	public void checkActiveComplaintsForDeleteMasterData(Complaints complaint) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_SERVICE_IMPL, "checkActiveComplaintsForDeleteMasterData");
		BusinessPartners supplierData = null != complaint.getSupplierId()
				? configurationService.getSupplier(complaint.getSupplierId())
				: null;
		BusinessPartners supplierContactPersonData = null != complaint.getContactPersonId()
				? configurationService.getSupplier(complaint.getContactPersonId())
				: null;
		List<String> businessPartnersToBeDeleted = new ArrayList<>();
		if (null != supplierData && supplierData.getIsMarkedForDeletion()) {
			businessPartnersToBeDeleted = getActiveComplaintsForBusinessPartner(complaint, supplierData,
					businessPartnersToBeDeleted);
		}
		if (null != supplierContactPersonData && supplierContactPersonData.getIsMarkedForDeletion()) {
			businessPartnersToBeDeleted = getActiveComplaintsForBusinessPartner(complaint, supplierContactPersonData,
					businessPartnersToBeDeleted);
		}
		if (!CollectionUtils.isEmpty(businessPartnersToBeDeleted)) {
			businessPartnerRepository.deleteBusinessPartnerList(businessPartnersToBeDeleted);
		}
		LoggerHelper.logMethodExit(logger, COMPLAINT_SERVICE_IMPL, "checkActiveComplaintsForDeleteMasterData");
	}

	/**
	 * Get Active complaints for business partner
	 * 
	 * @param complaint
	 * @param supplierData
	 * @param businessPartnerList
	 * @return
	 */
	public List<String> getActiveComplaintsForBusinessPartner(Complaints complaint, BusinessPartners supplierData,
			List<String> businessPartnerList) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_SERVICE_IMPL, "getActiveComplaintsForBusinessPartner");
		List<Complaints> activeComplaintsList;
		Result activeComplaintsResult;
		activeComplaintsResult = complaintDao.getActiveComplaintsForSupplier(complaint.getSupplierId());
		activeComplaintsList = null != activeComplaintsResult && activeComplaintsResult.first().isPresent()
				? activeComplaintsResult.listOf(Complaints.class)
				: null;
		if (CollectionUtils.isEmpty(activeComplaintsList)) {
			businessPartnerList.add(supplierData.getId());
		}
		LoggerHelper.logMethodExit(logger, COMPLAINT_SERVICE_IMPL, "getActiveComplaintsForBusinessPartner");
		return businessPartnerList;
	}

	/**
	 * Validate Complaint attributes and field control
	 */
	@Override
	public void validateComplaintAttributes(Complaints complaint) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_SERVICE_IMPL, "validateComplaintAttributes");
		complaintValidation.validateComplaintAttributes(complaint);
		LoggerHelper.logMethodEntry(logger, COMPLAINT_SERVICE_IMPL, "validateComplaintAttributes");

	}

	@Override
	public List<BTPUsers> getAllResponsiblePerson() {
		logger.info("Inside getAllResponsiblePerson");
		List<BTPUsers> btpUsers = new ArrayList<>();
		try {
			HttpDestination httpDestination = DestinationAccessor.getDestination("BTP-USERS").asHttp();
			ScimUsers scimUsers = new ScimUsersShadowUsersApi(httpDestination).getAllUsersUsingGET();
			List<ScimUser> sU = scimUsers.getResources();
			for (ScimUser user : sU) {
				BTPUsers b = Struct.create(BTPUsers.class);
				b.setPersonResponsibleNumber(user.getExternalId());
				b.setPersonResponsibleId(user.getUserName());
				btpUsers.add(b);
			}
			return btpUsers;
		} catch (Exception e) {
			logger.info("Destination not found!!! ");
			LoggerHelper.logExceptionWithMessage(logger, "BTP User Destination not found ", e);
			return btpUsers;
		}

	}

	@Override
	public void deleteBusinessPartnerWhenComplaintClosed(Complaints complaint) {
		List<String> businessPartnerList = new ArrayList<>();
		String supplierID = complaint.getSupplierId();
		if (businessPartnerService.checkIsMarkedForDeletion(supplierID)
				&& !businessPartnerService.businessPartnerUsedByAnyComplaint(supplierID)
				&& !businessPartnerService.businessPartnerUsedByAnyBusinessObject(supplierID)) {
			logger.info("******Deletable supplierID {} ::: ", supplierID);
			businessPartnerList.add(supplierID);
		}

		String contactPersonID = complaint.getContactPersonId();
		if (businessPartnerService.checkIsMarkedForDeletion(contactPersonID)
				&& !businessPartnerService.businessPartnerUsedByAnyComplaint(contactPersonID)
				&& !businessPartnerService.businessPartnerUsedByAnyBusinessObject(contactPersonID)) {
			logger.info("******Deletable contactPersonID {} ::: ", contactPersonID);
			businessPartnerList.add(contactPersonID);
		}

		List<CommonBusinessObjects> businessObjects = businessObjectDao
				.getCommonBusinessObjectsBasedOnComplaint(complaint.getId());
		for (CommonBusinessObjects bo : businessObjects) {
			if (businessPartnerService.checkIsMarkedForDeletion(bo.getSupplierId())
					&& !businessPartnerService.businessPartnerUsedByAnyComplaint(bo.getSupplierId())
					&& !businessPartnerService.businessPartnerUsedByAnyBusinessObject(bo.getSupplierId())) {
				logger.info("******Deletable SupplierID {} ::: ", bo.getSupplierId());
				businessPartnerList.add(bo.getSupplierId());
			}
			if (businessPartnerService.checkIsMarkedForDeletion(bo.getContactPersonId())
					&& !businessPartnerService.businessPartnerUsedByAnyComplaint(bo.getContactPersonId())
					&& !businessPartnerService.businessPartnerUsedByAnyBusinessObject(bo.getContactPersonId())) {
				logger.info("******Deletable ContactPersonID {} ::: ", bo.getContactPersonId());
				businessPartnerList.add(bo.getContactPersonId());
			}
			if (businessPartnerService.checkIsMarkedForDeletion(bo.getPersonResponsibleId())
					&& !businessPartnerService.businessPartnerUsedByAnyComplaint(bo.getPersonResponsibleId())
					&& !businessPartnerService.businessPartnerUsedByAnyBusinessObject(bo.getPersonResponsibleId())) {
				logger.info("******Deletable PersonResponsibleID {} ::: ", bo.getPersonResponsibleId());
				businessPartnerList.add(bo.getPersonResponsibleId());
			}
		}
		businessPartnerRepository.deleteBusinessPartnerList(businessPartnerList);
	}

	public Boolean getIsComplaintStatusClosedBasedOnBusinessPartner(String businessPartnerId) {
		Boolean isComplaintStatusClosedBasedOnSupplier = complaintDao
				.getIsComplaintStatusClosedBasedOnSupplier(businessPartnerId);
		logger.info("***isComplaintStatusClosedBasedOnSupplier {} ::: ", isComplaintStatusClosedBasedOnSupplier);
		Boolean isComplaintStatusClosedBasedOnContactPerson = complaintDao
				.getIsComplaintStatusClosedBasedOnContactPerson(businessPartnerId);
		logger.info("***isComplaintStatusClosedBasedOnContactPerson {} ::: ",
				isComplaintStatusClosedBasedOnContactPerson);
		return (isComplaintStatusClosedBasedOnSupplier && isComplaintStatusClosedBasedOnContactPerson);
	}

	/**
	 * Delete complaint with duplicate Quality notification number
	 */
	@Override
	public void deleteComplaints(String complaintId) {
		complaintDao.deleteAutomaticComplaint(complaintId);

	}

	/**
	 * Fetch Master Data Attributes of complaint based on ID
	 */
	@Override
	public Complaints getMasterDataFromComplaints(String complaintId) {
		Result result = complaintDao.getMasterDataFromComplaints(complaintId);
		return result.first().isPresent() ? result.listOf(Complaints.class).get(0) : null;
	}

	/**
	 * Fetch Creation type of complaint based on ID
	 */
	@Override
	public Complaints getComplaintCreationTypeAndCompanyCode(String complaintId) {
		Result result = complaintDao.getComplaintCreationTypeAndCompanyCode(complaintId);
		return result.first().isPresent() ? result.listOf(Complaints.class).get(0) : null;
	}

	public void validateComplaintStatus(String complaintId) {
		Complaints complaint = getComplaintDetails(complaintId);
		if (Constants.COMPLAINT_CLOSED.equalsIgnoreCase(complaint.getComplaintStatusCode())
				|| Constants.COMPLAINT_DISCARDED.equalsIgnoreCase(complaint.getComplaintStatusCode())) {
			messages.error(MessageKeys.BO_NOT_CREATED_FOR_DISCARDED_CLOSED_COMPLAINT);
		}
		messages.throwIfError();
	}

}