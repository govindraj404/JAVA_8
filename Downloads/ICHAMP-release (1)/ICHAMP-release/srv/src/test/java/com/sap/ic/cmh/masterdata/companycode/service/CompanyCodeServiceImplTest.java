package com.sap.ic.cmh.masterdata.companycode.service;

import cds.gen.masterdataservice.CompanyCodes;
import cds.gen.masterdataservice.CompanyCodes_;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.ic.cmh.masterdata.companycode.model.CompanyCodeRequest;
import com.sap.ic.cmh.masterdata.companycode.model.CompanyCodeResponse;
import com.sap.ic.cmh.masterdata.companycode.repository.CompanyCodeRepository;
import com.sap.ic.cmh.utils.LocaleMessageHelper;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.*;
import java.util.stream.Collectors;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;


public class CompanyCodeServiceImplTest {

    @InjectMocks
    private CompanyCodeServiceImpl services;

    @Mock
    private CompanyCodeRepository repository;

    @Mock
    LocaleMessageHelper messageHelper;

    @Mock
    Result result;

    private Row row;
    private Optional opt;
    private List<CompanyCodeRequest> companyCodeRequestList  = new ArrayList<>();
    private List<String> companyCodeList = new ArrayList<>();
    private CompanyCodes companyCodes;
    private List<CompanyCodes> companyCodeLists = new ArrayList<>();
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        CompanyCodeRequest request = new CompanyCodeRequest();
        request.setCompanyCode("01");
        request.setRecordNo("33");

        companyCodes = Struct.create(CompanyCodes.class);
        companyCodes.setId("100");
        companyCodes.setCompanyCode("F001");
        companyCodeRequestList.add(request);
        companyCodeList = companyCodeRequestList.stream().map(CompanyCodeRequest::getCompanyCode).collect(Collectors.toList());

    }

    @Test
    public void testDeleteCompanyCodeList_RecordsDeleted() {
        final List<CompanyCodeResponse> companyCodeResponses =services.deleteCompanyCodeList(companyCodeRequestList);
        Assert.assertEquals(1, companyCodeResponses.size());
    }
    @Test
    public void testDeleteCompanyCodeList() {
        Map<String, String> companyCodeWithIdMap = new LinkedHashMap<>();
        companyCodeWithIdMap.put("01", "4052c63c-6351-4ea5-8192-4af6686bb526");
        companyCodeWithIdMap.put("21", "4052c63c-6351-4ea5-8192-4af6686bb527");
        when(repository.getCompanyCodeMap(companyCodeList)).thenReturn(companyCodeWithIdMap);
        final List<CompanyCodeResponse> companyCodeResponses = services.deleteCompanyCodeList(companyCodeRequestList);
        Assert.assertEquals("01", companyCodeResponses.get(0).getCompanyCode());
    }
    @Test
    public void testFetchCompanyCodesBasedOnCode() {
        Map<String, String> CompanyCodeCodeMap = new HashMap<>();
        CompanyCodeCodeMap.put("200001", "Australia");
        when(result.listOf(CompanyCodes.class)).thenReturn(companyCodeLists);

        when(result.listOf(CompanyCodes.class)).thenReturn(companyCodeLists);
        when(repository.fetchCompanyCodesBasedOnCode(companyCodes.getCompanyCode())).thenReturn(result);
        services.fetchCompanyCodesBasedOnCode(companyCodes.getCompanyCode());

    }

    @Test
    public void testGetUnitOfMeasureDetailsNull() {
        when(repository.fetchCompanyCodesBasedOnCode(companyCodes.getCompanyCode())).thenReturn(result);
        services.fetchCompanyCodesBasedOnCode(companyCodes.getCompanyCode());

    }



}
