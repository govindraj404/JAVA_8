package com.sap.ic.cmh.masterdata.businesspartner.repository;

import java.util.List;
import java.util.Map;
import cds.gen.masterdataservice.BusinessPartners;
import com.sap.cds.Result;


public interface BusinessPartnerRepository {
    Map<String, String> getBusinessPartnersMap(List<String> businessPartnerRequestList);

    List<String> getActiveComplaintsInBusinessPartner(List<String> businessPartnersId);

    void deleteBusinessPartnerList(List<String> recordsToBeDeleted);

    void updateBusinessPartner(BusinessPartners updateIsMarkedForDeletion);

    Result getBusinessPartners(String businessPartnerId);
    
    Result getBusinessPartnersBasedOnNumber(String businessPartnerNumber);
    
    Result checkIfCustomerCodeExists(String customerCode);
    
    Result checkIfVendorCodeExists(String vendorCode);
}
