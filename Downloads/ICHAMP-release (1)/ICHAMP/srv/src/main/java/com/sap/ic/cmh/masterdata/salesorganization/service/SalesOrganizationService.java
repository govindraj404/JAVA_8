package com.sap.ic.cmh.masterdata.salesorganization.service;

import java.util.List;

import com.sap.ic.cmh.masterdata.salesorganization.model.SalesOrganizationRequest;
import com.sap.ic.cmh.masterdata.salesorganization.model.SalesOrganizationResponse;

import cds.gen.masterdataservice.SalesOrganizations;

public interface SalesOrganizationService {
	List<SalesOrganizationResponse> deleteSalesOrganizationList(List<SalesOrganizationRequest> salesOrganizationRequest);

	SalesOrganizations getSalesOrganizationDetailsBasedOnSalesOrgCode(String salesOrganization);
	
	List<SalesOrganizations> getSalesOrganizationDetailsBasedOnCodeList(List<String> salesOrganization);

}