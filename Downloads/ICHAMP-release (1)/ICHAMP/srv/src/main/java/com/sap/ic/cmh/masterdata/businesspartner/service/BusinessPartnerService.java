package com.sap.ic.cmh.masterdata.businesspartner.service;

import java.util.List;
import com.sap.ic.cmh.masterdata.businesspartner.model.BusinessPartnerRequest;
import com.sap.ic.cmh.masterdata.businesspartner.model.BusinessPartnerResponse;
import cds.gen.masterdataservice.BusinessPartners;

public interface BusinessPartnerService {
    List<BusinessPartnerResponse> deleteBusinessPartners(List<BusinessPartnerRequest> businessPartnersRequest);

    public boolean businessPartnerUsedByAnyComplaint(String businessPartnerID);

    public boolean businessPartnerUsedByAnyBusinessObject(String businessPartnerID);

    public boolean checkIsMarkedForDeletion(String businessPartnerID);

   BusinessPartners getBusinessPartnersBasedOnNumber(String businessPartnerNumber);

   BusinessPartners checkIfCustomerCodeExists(String customerCode);

   BusinessPartners checkIfVendorCodeExists(String vendorCode);
}
