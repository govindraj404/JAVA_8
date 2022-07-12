package com.sap.ic.cmh.masterdata.division.persistency;

import java.util.List;
import com.sap.cds.Result;

public interface DivisionRepository {
	void deleteSalesDivision(List<String> recordsToBeDeleted);

	Result getDivisionMap(List<String> divisions, List<String> salesOrgList);

	Result getDivisionDetailsBasedOnDivisionAndSalesOrg(String division, String salesOrgIdList);

	Result getActiveComplaintsInSalesDivisions(List<String> divisionsId);

    public Result getDivisionIDandSalesIDById(String divisionId);

}