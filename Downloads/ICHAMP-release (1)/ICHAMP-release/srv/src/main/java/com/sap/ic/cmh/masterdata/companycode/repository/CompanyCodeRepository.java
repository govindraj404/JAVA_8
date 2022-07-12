package com.sap.ic.cmh.masterdata.companycode.repository;

import java.util.List;
import java.util.Map;
import java.util.function.Function;
import com.sap.cds.Result;
import com.sap.cds.ql.StructuredType;

public interface CompanyCodeRepository {

    Map<String, String> getCompanyCodeMap(List<String> companyCodes);

    List<String> getActiveComplaintsInCompanyCode(List<String> companyCodesId);

    void deleteCompanyCodeList(List<String> recordsToBeDeleted);

    String getCompanyCodeIdBasedOnCode(String companyCode);

    public <E extends StructuredType<E>> Result fetchCompanyCode(String companyCode,
            String message, Class<E> targetClass, Function<E, Object> targetClassAttribute);

    public Result fetchCompanyCodesBasedOnCode(String companyCode);

 

}
