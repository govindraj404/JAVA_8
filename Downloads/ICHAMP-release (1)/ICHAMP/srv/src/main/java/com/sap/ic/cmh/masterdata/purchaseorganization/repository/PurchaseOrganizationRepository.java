package com.sap.ic.cmh.masterdata.purchaseorganization.repository;

import java.util.List;
import java.util.Map;

import com.sap.cds.Result;

public interface PurchaseOrganizationRepository {
    Map<String, String> getPurchaseOrganizationMap(List<String> purchaseOrganizationList);

    List<String> getActiveComplaintsInPurchaseOrganization(List<String> purchaseOrganizationsId);

    void deletePurchaseOrganizationList(List<String> purchaseOrganizationList);
    
    Result fetchPurchaseOrganization(String purchaseOrganizations);
}
