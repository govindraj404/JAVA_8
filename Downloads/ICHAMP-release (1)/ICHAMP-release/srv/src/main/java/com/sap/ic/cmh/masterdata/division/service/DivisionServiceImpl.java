package com.sap.ic.cmh.masterdata.division.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import com.sap.cds.Result;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.masterdata.division.model.DivisionRequest;
import com.sap.ic.cmh.masterdata.division.model.DivisionResponse;
import com.sap.ic.cmh.masterdata.division.persistency.DivisionRepository;
import com.sap.ic.cmh.masterdata.salesorganization.service.SalesOrganizationService;
import com.sap.ic.cmh.utils.LocaleMessageHelper;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.complaintservice.Complaints;
import cds.gen.masterdataservice.Divisions;
import cds.gen.masterdataservice.SalesOrganizations;

/**
 * This class used to receive division details, validate and process data for delete
 */

@Service
public class DivisionServiceImpl implements DivisionService {

    public static final Logger logger = LoggerHelper.getLogger(DivisionServiceImpl.class);
    @Autowired
    LocaleMessageHelper messageHelper;
    @Autowired
    private DivisionRepository divisionRepository;
    @Autowired
	SalesOrganizationService salesOrganizationService;

    /**
     * This will delete the sales division inactive records by validating with the returnables account and send the records which are not
     * deleted back to the response
     *
     * @param divisionsRequest Request containing sales division and sales organization
     * @return Sales division Records which are not deleted
     */
    @Override
    public List<DivisionResponse> deleteDivisionList(List<DivisionRequest> divisionsRequest) {
        LoggerHelper.logMethodEntry(logger, "DivisionServiceImpl", "deleteDivisionList");
        List<DivisionResponse> divisionResponseList = new ArrayList<>();

        AtomicInteger integerAtomic = new AtomicInteger();
        List<String> salesOrgList = new ArrayList<>();
        List<String> divisionList = new ArrayList<>();
        divisionsRequest.forEach(division -> {
            salesOrgList.add(division.getSalesOrganization());
            divisionList.add(division.getDivision());
            division.setRecordNo(String.valueOf(integerAtomic.incrementAndGet()));
        });
        //Get Division records based on sales Org and Division channel
        List<SalesOrganizations> salesOrganizationDetailsBasedOnCode = salesOrganizationService.getSalesOrganizationDetailsBasedOnCodeList(salesOrgList);
		List<String> salesOrgIdList=!CollectionUtils.isEmpty(salesOrganizationDetailsBasedOnCode) ? salesOrganizationDetailsBasedOnCode.stream().map(SalesOrganizations::getId).collect(Collectors.toList())
				:new ArrayList<>();
        Result divisionResult = divisionRepository.getDivisionMap(divisionList, salesOrgIdList);
        List<Divisions> divisionDetailsFromDb = divisionResult.first().isPresent() ? divisionResult.listOf(Divisions.class) : new ArrayList<>();
        //Collect the db records into Division request list
        List<DivisionRequest> divisionsRequestDb = new ArrayList<>();

        Map<String, Divisions> divisionsDbMap = new HashMap<>();
        if(!CollectionUtils.isEmpty(divisionDetailsFromDb)) {
            divisionDetailsFromDb.forEach(division -> {
                DivisionRequest divisionRequest = new DivisionRequest();
                divisionRequest.setDivision(division.getSalesDivision());
                Optional<SalesOrganizations> findFirst = salesOrganizationDetailsBasedOnCode.stream().filter(s->s.getId().equalsIgnoreCase(division.getSalesOrganizationIDId())).findAny();
                String salesOrg = findFirst.isPresent() ? findFirst.get().getSalesOrganization() : "";
				division.setSalesOrganization(salesOrg);
                divisionRequest.setSalesOrganization(salesOrg);
                divisionsRequestDb.add(divisionRequest);
                divisionsDbMap.put(division.getId(),division);
                
            });
        }

        //Records which does not exist will be sent to the response
        divisionsRequest.stream().filter(divisionRequest ->
                !divisionsRequestDb.contains(divisionRequest))
                .forEach(divRequest -> {
                    DivisionResponse response = new DivisionResponse();
                    response.setMessage(messageHelper.getMessage(MessageKeys.SALES_DIVISION_DOES_NOT_EXIST));
                    response.setStatus(messageHelper.getMessage(MessageKeys.ERROR));
                    response.setRecordNo(divRequest.getRecordNo());
                    response.setSalesOrganization(divRequest.getSalesOrganization());
                    response.setSalesDivision(divRequest.getDivision());
                    divisionResponseList.add(response);
                });
      //Check for Active complaint records
		 checkActiveComplaints(divisionsRequest, divisionResponseList, divisionsDbMap);
        LoggerHelper.logMethodExit(logger, "DivisionServiceImpl", "deleteDivisionList");
        return divisionResponseList;
    }
    
    /**
     * 
     * @param divisionsRequest
     * @param divisionResponseList
     * @param divisionsDbMap
     */
    private void checkActiveComplaints(List<DivisionRequest> divisionsRequest,
			List<DivisionResponse> divisionResponseList, Map<String, Divisions> divisionsDbMap) {
		
        if (!CollectionUtils.isEmpty(divisionsDbMap)) {
            List<String> recordsToBeDeleted = new ArrayList<>();
            List<String> divisionIdList = new ArrayList<>(divisionsDbMap.keySet());
            Result activeComplaintsResult = divisionRepository.getActiveComplaintsInSalesDivisions(divisionIdList);
            List<Complaints> complaintsList = activeComplaintsResult.first().isPresent() ? activeComplaintsResult.listOf(Complaints.class) : new ArrayList<>();
            final List<String> activeComplaintsDistributionChannelIdList = !CollectionUtils.isEmpty(complaintsList)? complaintsList.stream().map(Complaints::getCompanyCodeId).collect(Collectors.toList())
            		: new ArrayList<>();
            for (String distributionChannelId : divisionIdList) {
            	//create the payload and delete inactive divisions
                createResponseAndDeleteDivisions(divisionsRequest, divisionResponseList,
                		divisionsDbMap, recordsToBeDeleted, activeComplaintsDistributionChannelIdList,
						distributionChannelId);
                if (!CollectionUtils.isEmpty(recordsToBeDeleted)) {
                	divisionRepository.deleteSalesDivision(recordsToBeDeleted);
                    logger.info("DistributionChannels deleted for : {}  " , recordsToBeDeleted.toString());
                }
            }
        }
	}

	private void createResponseAndDeleteDivisions(List<DivisionRequest> divisionsRequest,
			List<DivisionResponse> divisionResponseList, Map<String, Divisions> divisionsDbMap,
			List<String> recordsToBeDeleted, List<String> activeComplaintsDistributionChannelIdList,
			String divisionId) {
		if (!activeComplaintsDistributionChannelIdList.contains(divisionId)) {
			divisionsRequest.stream()
		            .filter(e -> e.getDivision().equals(divisionsDbMap.get(divisionId).getSalesDivision())
		                    && e.getSalesOrganization().equals(divisionsDbMap.get(divisionId).getSalesOrganization())
		            ).findFirst().ifPresent(salesDivision -> {
		        //Collect records to be deleted
		        recordsToBeDeleted.add(divisionId);
		        DivisionResponse divisionResponse = new DivisionResponse();
		        divisionResponse.setMessage(messageHelper.getMessage(MessageKeys.SALES_DIVISION_SUCCESSFULLY_DELETED));
		        divisionResponse.setStatus(messageHelper.getMessage(MessageKeys.SUCCESS));
		        divisionResponse.setRecordNo(salesDivision.getRecordNo());
		        divisionResponse.setSalesOrganization(salesDivision.getSalesOrganization());
		        divisionResponse.setSalesDivision(salesDivision.getDivision());
		        divisionResponseList.add(divisionResponse);
		    });
		} else {
			divisionsRequest.stream()
		            .filter(e -> e.getDivision().equals(divisionsDbMap.get(divisionId).getSalesDivision())
		                    && e.getSalesOrganization().equals(divisionsDbMap.get(divisionId).getSalesOrganization())
		            ).findFirst().ifPresent(division -> {
		        DivisionResponse divisionResponse = new DivisionResponse();
		        divisionResponse.setMessage(messageHelper.getMessage(MessageKeys.SALES_DIVISION_ASSOCIATION_TO_COMPLAINT));
		        divisionResponse.setStatus(messageHelper.getMessage(MessageKeys.ERROR));
		        divisionResponse.setRecordNo(division.getRecordNo());
		        divisionResponse.setSalesOrganization(division.getSalesOrganization());
		        divisionResponse.setSalesDivision(division.getDivision());
		        divisionResponseList.add(divisionResponse);
		    });
		}
	}

	/**
     * Fetch Division details based on Division and Sales Organization
     */
	@Override
	public Divisions getDivisionDetailsBasedOnDivisionAndSalesOrg(String division, String salesOrganization) {
		SalesOrganizations salesOrganizationDetailsBasedOnSalesOrgCode = salesOrganizationService.getSalesOrganizationDetailsBasedOnSalesOrgCode(salesOrganization);
		String salesOrgId = null!=salesOrganizationDetailsBasedOnSalesOrgCode?salesOrganizationDetailsBasedOnSalesOrgCode.getId() : "";
		Result divisionDetailsBasedOnDivisionAndSalesOrgResult = divisionRepository
				.getDivisionDetailsBasedOnDivisionAndSalesOrg(division, salesOrgId);
		return divisionDetailsBasedOnDivisionAndSalesOrgResult.first().isPresent()
				? divisionDetailsBasedOnDivisionAndSalesOrgResult.listOf(Divisions.class).get(0)
				: null;
	}
}
