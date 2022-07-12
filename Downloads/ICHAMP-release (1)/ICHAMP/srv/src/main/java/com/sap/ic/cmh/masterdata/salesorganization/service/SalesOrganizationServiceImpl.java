package com.sap.ic.cmh.masterdata.salesorganization.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import com.sap.cds.Result;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.masterdata.salesorganization.model.SalesOrganizationRequest;
import com.sap.ic.cmh.masterdata.salesorganization.model.SalesOrganizationResponse;
import com.sap.ic.cmh.masterdata.salesorganization.persistency.SalesOrganizationRepository;
import com.sap.ic.cmh.utils.LocaleMessageHelper;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.complaintservice.Complaints;
import cds.gen.masterdataservice.SalesOrganizations;

/**
 * This class used to receive sales organization details, validate and process
 * data for delete
 */
@Service
public class SalesOrganizationServiceImpl implements SalesOrganizationService {

	public static final Logger logger = LoggerHelper.getLogger(SalesOrganizationServiceImpl.class);
	@Autowired
	LocaleMessageHelper messageHelper;
	@Autowired
	private SalesOrganizationRepository salesOrganizationRepository;

	@Override
	public List<SalesOrganizationResponse> deleteSalesOrganizationList(
			List<SalesOrganizationRequest> salesOrganizationRequest) {

		LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "deleteSalesOrganizationList");
		List<SalesOrganizationResponse> salesOrganizationResponseList = new ArrayList<>();
		AtomicInteger index = new AtomicInteger();
		Map<String, String> salesOrganizationsWithRecordNoMap = new HashMap<>();
		salesOrganizationRequest.forEach(salesOrg -> salesOrganizationsWithRecordNoMap
				.put(salesOrg.getSalesOrganization(), String.valueOf(index.incrementAndGet())));

		// Get Sales Organization Id by converting map into List<String
		List<String> salesOrganizationList = new ArrayList<>(salesOrganizationsWithRecordNoMap.keySet());
		Result result = salesOrganizationRepository
				.getSalesOrganizationMap(salesOrganizationList);
		List<SalesOrganizations> salesOrgList = result.first().isPresent() ? result.listOf(SalesOrganizations.class) : new ArrayList<>();
		Map<String, String> salesOrganizationMapWithId = !CollectionUtils.isEmpty(salesOrgList) ? 
				salesOrgList.stream().collect(Collectors.toMap(SalesOrganizations::getId, SalesOrganizations::getSalesOrganization)) : new HashMap<>();
		if(!CollectionUtils.isEmpty(salesOrganizationMapWithId)) {
			salesOrganizationsWithRecordNoMap.keySet().forEach(salesOrganization -> {
				if (!salesOrganizationMapWithId.containsValue(salesOrganization)) {
					logger.info("Sales Organization not Found in DB :: {} ", salesOrganization);
					SalesOrganizationResponse salesOrganizationResponse = new SalesOrganizationResponse();
					salesOrganizationResponse.setSalesOrganization(salesOrganization);
					salesOrganizationResponse
							.setMessage(messageHelper.getMessage(MessageKeys.SALES_ORGANIZATION_DOES_NOT_EXIST));
					salesOrganizationResponse.setStatus(messageHelper.getMessage(MessageKeys.ERROR));
					salesOrganizationResponse.setRecordNo(salesOrganizationsWithRecordNoMap.get(salesOrganization));
					salesOrganizationResponseList.add(salesOrganizationResponse);
				}
			});
		}
	

		checkActiveComplaints(salesOrganizationResponseList, salesOrganizationsWithRecordNoMap,
				salesOrganizationMapWithId);

		return salesOrganizationResponseList;
	}

	public void checkActiveComplaints(List<SalesOrganizationResponse> salesOrganizationResponseList,
			Map<String, String> salesOrganizationsWithRecordNoMap, Map<String, String> salesOrganizationMapWithId) {
		// Check for active complaint records
		if (!salesOrganizationMapWithId.isEmpty()) {
			List<String> recordsToBeDeleted = new ArrayList<>();
			List<String> salesOrganizationIdList = new ArrayList<>(salesOrganizationMapWithId.keySet());
			Result activeComplaintsResult = salesOrganizationRepository.getActiveComplaintsInSalesOrganizations(salesOrganizationIdList);
			List<Complaints> complaintsList = activeComplaintsResult.first().isPresent() ? activeComplaintsResult.listOf(Complaints.class) : new ArrayList<>();
            final List<String> activeComplaintsSalesOrgIdList = !CollectionUtils.isEmpty(complaintsList)? complaintsList.stream().map(Complaints::getCompanyCodeId).collect(Collectors.toList())
            		: new ArrayList<>();
           for (String salesOrganizationId : salesOrganizationIdList) {
               if (!activeComplaintsSalesOrgIdList.contains(salesOrganizationId)) {
                   //Collect records to be deleted
                   recordsToBeDeleted.add(salesOrganizationId);
                   SalesOrganizationResponse salesOrganizationResponse = new SalesOrganizationResponse();
                   salesOrganizationResponse.setSalesOrganization(salesOrganizationMapWithId.get(salesOrganizationId));
                   salesOrganizationResponse.setMessage(messageHelper.getMessage(MessageKeys.SALES_ORGANIZATION_SUCCESSFULLY_DELETED));
                   salesOrganizationResponse.setStatus(messageHelper.getMessage(MessageKeys.SUCCESS));
                   salesOrganizationResponse.setRecordNo(salesOrganizationsWithRecordNoMap.get(salesOrganizationResponse.getSalesOrganization()));
                   salesOrganizationResponseList.add(salesOrganizationResponse);
               } else {

                   //Collect the error records to be sent to the response
                   SalesOrganizationResponse salesOrganizationResponse = new SalesOrganizationResponse();
                   salesOrganizationResponse.setSalesOrganization(salesOrganizationMapWithId.get(salesOrganizationId));
                   salesOrganizationResponse.setMessage(messageHelper.getMessage(MessageKeys.SALES_ORGANIZATION_ASSOCIATION_TO_COMPLAINT));
                   salesOrganizationResponse.setStatus(messageHelper.getMessage(MessageKeys.ERROR));
                   salesOrganizationResponse.setRecordNo(salesOrganizationsWithRecordNoMap.get(salesOrganizationResponse.getSalesOrganization()));
                   salesOrganizationResponseList.add(salesOrganizationResponse);
               }
           }
			if (!CollectionUtils.isEmpty(recordsToBeDeleted)) {
				salesOrganizationRepository.deleteInactiveSalesOrganization(recordsToBeDeleted);
				logger.info("Records Deleted {} ", recordsToBeDeleted.size());
			}
		}
	}
    
	/**
	 * Fetch Sales Organization details based on Sales Org code
	 */
	@Override
	public SalesOrganizations getSalesOrganizationDetailsBasedOnSalesOrgCode(String salesOrganization) {
		Result result = salesOrganizationRepository.getSalesOrganizationDetailsBasedOnSalesOrgCode(salesOrganization);
		return result.first().isPresent() ? result.listOf(SalesOrganizations.class).get(0) : null;
	}
	/**
	 * Fetch Sales Organization details based on Sales Org id
	 */
	@Override
	public List<SalesOrganizations> getSalesOrganizationDetailsBasedOnCodeList(List<String> salesOrganization) {
		Result result = salesOrganizationRepository.getSalesOrganizationDetailsBasedOnCodeList(salesOrganization);
		return result.first().isPresent() ? result.listOf(SalesOrganizations.class) : new ArrayList<>();
	}
}
