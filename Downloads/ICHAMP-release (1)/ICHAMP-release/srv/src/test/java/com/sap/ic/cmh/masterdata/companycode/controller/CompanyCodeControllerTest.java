package com.sap.ic.cmh.masterdata.companycode.controller;


import com.sap.ic.cmh.masterdata.companycode.model.CompanyCodeRequest;
import com.sap.ic.cmh.masterdata.companycode.model.CompanyCodeResponse;
import com.sap.ic.cmh.masterdata.companycode.service.CompanyCodeService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;


public class CompanyCodeControllerTest {

    @Mock
    private CompanyCodeService service;
    static List<CompanyCodeResponse> responseList;
    List<CompanyCodeRequest> addressRequests;
    @InjectMocks
    private CompanyCodeController controller;

    @Before
    public void beforeClass()  {
        MockitoAnnotations.openMocks(this);
        CompanyCodeResponse response=new CompanyCodeResponse();

        response.setMessage("test");
        response.setRecordNo("2");
        response.setStatus("success");
    }
    @Test
    public  void  deleteCompanyCodeListsTest() {
        CompanyCodeRequest request1 = new CompanyCodeRequest();
        request1.setCompanyCode("F01");
        CompanyCodeRequest request2 = new CompanyCodeRequest();
        request2.setCompanyCode("F01");

        addressRequests = new ArrayList<>();
        addressRequests.add(request1);
        addressRequests.add(request2);

        service.deleteCompanyCodeList(addressRequests);

    }

    @Test
    public void deleteCompanyCodeListTest()
    {
        controller.deleteCompanyCodeLists(addressRequests);
    }
    @Test
    public void deleteCompanyCodeListNullTest()
    {
        controller.deleteCompanyCodeLists(null);
    }
}
