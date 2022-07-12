package com.sap.ic.cmh.masterdata.salesorganization.persistency;

import java.util.List;

import com.sap.cds.Result;

public interface SalesOrganizationRepository {

    void deleteInactiveSalesOrganization(List<String> recordsToBeDeleted);

    Result getSalesOrganizationMap(List<String> salesOrganizations);

	Result getSalesOrganizationDetailsBasedOnSalesOrgCode(String salesOrganization);

	Result getActiveComplaintsInSalesOrganizations(List<String> salesOrganizationIdList);

	Result getSalesOrganizationDetailsBasedOnCodeList(List<String> salesOrganizationId);

    public Result getSalesOrganizationById(String salesOrganizationId);

}