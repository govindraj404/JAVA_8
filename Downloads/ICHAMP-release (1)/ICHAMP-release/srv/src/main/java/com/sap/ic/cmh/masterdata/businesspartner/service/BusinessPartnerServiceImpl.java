package com.sap.ic.cmh.masterdata.businesspartner.service;


import cds.gen.complaintservice.CommonBusinessObjects;
import cds.gen.complaintservice.Complaints;
import com.sap.ic.cmh.businessobjects.persistency.BusinessObjectDao;
import com.sap.ic.cmh.complaint.service.ComplaintService;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.masterdata.businesspartner.model.BusinessPartnerRequest;
import com.sap.ic.cmh.masterdata.businesspartner.model.BusinessPartnerResponse;
import com.sap.ic.cmh.masterdata.businesspartner.repository.BusinessPartnerRepository;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LocaleMessageHelper;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import com.sap.cds.Struct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.*;

import com.sap.cds.Result;

import java.util.concurrent.atomic.AtomicInteger;
import cds.gen.masterdataservice.BusinessPartners;

@Service
public class BusinessPartnerServiceImpl implements BusinessPartnerService {
    public static final Logger logger = LoggerHelper.getLogger(BusinessPartnerServiceImpl.class);
    private static final String BUSINESS_PARTNER_SERVICE_IMPL = "BusinessPartnerServiceImpl";
    @Autowired
    LocaleMessageHelper messageHelper;
    @Autowired
    private BusinessPartnerRepository businessPartnerRepository;



    @Autowired
    ComplaintService complaintService;

    @Autowired
    BusinessObjectDao businessObjectDao;

    /**
     * Delete Business Partners from the list by validating with the complaints and send the records which
     * are not deleted
     *
     * @param businessPartnersRequest List of business partners
     * @return Business partner records which are not deleted
     */
    @Override
    public List<BusinessPartnerResponse> deleteBusinessPartners(List<BusinessPartnerRequest> businessPartnersRequest) {
        LoggerHelper.logMethodEntry(logger, BUSINESS_PARTNER_SERVICE_IMPL, "deleteBusinessPartners");
        List<BusinessPartnerResponse> businessPartnersResponse = new ArrayList<>();

        AtomicInteger integerAtomic = new AtomicInteger();
        Map<String, String> businessPartnerMasterMap = new HashMap<>();
        businessPartnersRequest.forEach(businessPartner ->
                businessPartnerMasterMap.put(businessPartner.getBusinessPartner(), String.valueOf(integerAtomic.incrementAndGet())));
        List<String> businessPartnersList = new ArrayList<>(businessPartnerMasterMap.keySet());
        //Get Business Partner Id from business Partner Number
        final Map<String, String> businessPartnersDbMap = businessPartnerRepository.getBusinessPartnersMap(businessPartnersList);
        //Collect all the records which are not present
        businessPartnerMasterMap.keySet().stream().forEach(businessPartner -> {
            if (!businessPartnersDbMap.containsValue(businessPartner)) {
                BusinessPartnerResponse businessPartnerResponse = new BusinessPartnerResponse();
                businessPartnerResponse.setBusinessPartner(businessPartner);
                businessPartnerResponse.setMessage(messageHelper.getMessage(MessageKeys.BUSINESS_PARTNER_DOES_NOT_EXIST));
                businessPartnerResponse.setStatus(messageHelper.getMessage(MessageKeys.ERROR));
                businessPartnerResponse.setRecordNo(businessPartnerMasterMap.get(businessPartner));
                businessPartnersResponse.add(businessPartnerResponse);
            }
        });
        //Check for active complaints for business partner
        if (!businessPartnersDbMap.isEmpty()) {
            List<String> businessPartnersId = new ArrayList<>(businessPartnersDbMap.keySet());
            List<String> recordsToBeDeleted = new ArrayList<>();
            List<String> complaintBusinessPartnerDbList = businessPartnerRepository.getActiveComplaintsInBusinessPartner(businessPartnersId);
            for (String businessPartnerId : businessPartnersId) {
                BusinessPartnerResponse businessPartnerResponse = new BusinessPartnerResponse();
                businessPartnerResponse.setBusinessPartner(businessPartnersDbMap.get(businessPartnerId));
                businessPartnerResponse.setRecordNo(businessPartnerMasterMap.get(businessPartnerResponse.getBusinessPartner()));
                if (!complaintBusinessPartnerDbList.contains(businessPartnerId)) {
                    businessPartnerResponse.setMessage(messageHelper.getMessage(MessageKeys.BUSINESS_PARTNER_SUCCESSFULLY_DELETED));
                    businessPartnerResponse.setStatus(messageHelper.getMessage(MessageKeys.SUCCESS));
                    recordsToBeDeleted.add(businessPartnerId);
                    
                    
                } else {
                    updateBusinessPrtnerMarkedForDeletion(businessPartnerId);
                    businessPartnerResponse.setMessage(messageHelper.getMessage(MessageKeys.BUSINESS_PARTNER_ASSOCIATION_TO_COMPLAINT));
                    businessPartnerResponse.setStatus(messageHelper.getMessage(MessageKeys.ERROR));
                }
                businessPartnersResponse.add(businessPartnerResponse);
            }
            if (!CollectionUtils.isEmpty(recordsToBeDeleted)) {
                businessPartnerRepository.deleteBusinessPartnerList(recordsToBeDeleted);
                logger.info("BusinessPartners deleted for : ", recordsToBeDeleted.toString());
                
            }
        }
        LoggerHelper.logMethodExit(logger, BUSINESS_PARTNER_SERVICE_IMPL, "deleteBusinessPartners");
        return businessPartnersResponse;
    }
    /**
     * Update the BusinessPartner's field setIsMarkedForDeletion to true,
     * indicating a delete request is being raised for BusinessPartner.
     *
     * @param businessPartnerId BusinessPartnerID
     *
     */

    public void updateBusinessPrtnerMarkedForDeletion(String businessPartnerId) {
        LoggerHelper.logMethodEntry(logger, BUSINESS_PARTNER_SERVICE_IMPL, "UpdateBusinessPrtnerMarkedForDeletion");
        BusinessPartners updateIsMarkedForDeletion=Struct.create(BusinessPartners.class);
        updateIsMarkedForDeletion.setId(businessPartnerId);
        updateIsMarkedForDeletion.setIsMarkedForDeletion(true);
        businessPartnerRepository.updateBusinessPartner(updateIsMarkedForDeletion);
        LoggerHelper.logMethodExit(logger, BUSINESS_PARTNER_SERVICE_IMPL, "UpdateBusinessPrtnerMarkedForDeletion");
    }



    public boolean businessPartnerUsedByAnyComplaint(String businessPartnerID){
        return (!complaintService.getIsComplaintStatusClosedBasedOnBusinessPartner(businessPartnerID));
    }

    public boolean businessPartnerUsedByAnyBusinessObject(String businessPartnerID){
        List<CommonBusinessObjects> commonBusinessObjectList = businessObjectDao.getCommonBusinessObjectsBasedOnBusinessPartner(businessPartnerID);
        Set<Complaints> complaintSet = new HashSet<>();
        for(CommonBusinessObjects cbo : commonBusinessObjectList){
            logger.info("***CBO ::: {} ***",cbo);
            logger.info("***CBO Complaint ::: {} ***",cbo.getComplaint());
            complaintSet.add(cbo.getComplaint());
        }
        logger.info("***Complaint List ::: {} ***",complaintSet);

        for(Complaints complaint : complaintSet){
            if(!complaint.getComplaintStatusCode().equals(Constants.COMPLAINT_CLOSED)){
                return true;
            }
        }
        return false;
    }

    public boolean checkIsMarkedForDeletion(String businessPartnerID){
        Result businessPartner = businessPartnerRepository.getBusinessPartners(businessPartnerID);
        if(businessPartner.first().isPresent()){
            logger.info("***Business Partner ::: {} ***",businessPartner.first().get().get("isMarkedForDeletion").toString());
            return Boolean.parseBoolean(businessPartner.first().get().get("isMarkedForDeletion").toString());
        }
        return false;
    }
	@Override
	public BusinessPartners getBusinessPartnersBasedOnNumber(String businessPartnerNumber) {
		Result businessPartnersBasedOnNumberResult = businessPartnerRepository.getBusinessPartnersBasedOnNumber(businessPartnerNumber);
		return businessPartnersBasedOnNumberResult.first().isPresent() ? businessPartnersBasedOnNumberResult
				.listOf(BusinessPartners.class).get(0) : null;
	}
	@Override
	public BusinessPartners checkIfCustomerCodeExists(String customerCode) {
		Result customerCodeExistsResult = businessPartnerRepository.checkIfCustomerCodeExists(customerCode);
		return customerCodeExistsResult.first().isPresent() ? customerCodeExistsResult
				.listOf(BusinessPartners.class).get(0) : null;
	}
	@Override
	public BusinessPartners checkIfVendorCodeExists(String vendorCode) {
		Result vendorCodeExistsResult = businessPartnerRepository.checkIfVendorCodeExists(vendorCode);
		return vendorCodeExistsResult.first().isPresent() ? vendorCodeExistsResult
				.listOf(BusinessPartners.class).get(0) : null;
	}
}