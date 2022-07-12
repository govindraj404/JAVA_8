package com.sap.ic.cmh.masterdata.division.service;

import java.util.List;

import com.sap.ic.cmh.masterdata.division.model.DivisionRequest;
import com.sap.ic.cmh.masterdata.division.model.DivisionResponse;

import cds.gen.masterdataservice.Divisions;

public interface DivisionService {

    List<DivisionResponse> deleteDivisionList(List<DivisionRequest> divisionRequest);
    
    Divisions getDivisionDetailsBasedOnDivisionAndSalesOrg(String division, String salesOrganization);

}