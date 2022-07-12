package com.sap.ic.cmh.masterdata.companycode.service;


import com.sap.cds.ql.StructuredType;
import com.sap.ic.cmh.masterdata.companycode.model.CompanyCodeRequest;
import com.sap.ic.cmh.masterdata.companycode.model.CompanyCodeResponse;
import cds.gen.masterdataservice.CompanyCodes;
import java.util.List;
import java.util.function.Function;

public interface CompanyCodeService {
    List<CompanyCodeResponse> deleteCompanyCodeList(List<CompanyCodeRequest> companyCodeLists);
    public <E extends StructuredType<E>> CompanyCodes fetchCompanyCode(String companyCode, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute);
    CompanyCodes fetchCompanyCodesBasedOnCode(String companyCode);
}