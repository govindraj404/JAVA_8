package com.sap.ic.cmh.masterdata.purchaseorganization.service;

import java.util.List;
import com.sap.ic.cmh.masterdata.purchaseorganization.model.PurchaseOrganizationRequest;
import com.sap.ic.cmh.masterdata.purchaseorganization.model.PurchaseOrganizationResponse;
import cds.gen.masterdataservice.PurchaseOrganizations;

public interface PurchaseOrganizationService {
    List<PurchaseOrganizationResponse> deletePurchaseOrganizationList(List<PurchaseOrganizationRequest> purchaseOrganizationRequest);

    PurchaseOrganizations fetchPurchaseOrganization(String purchaseOrganizations);
}
