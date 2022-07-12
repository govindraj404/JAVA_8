package com.sap.ic.cmh.masterdata.address.controller;

import com.sap.ic.cmh.masterdata.address.model.AddressRequest;
import com.sap.ic.cmh.masterdata.address.model.AddressResponse;
import com.sap.ic.cmh.masterdata.address.service.AddressService;
import org.junit.Before;

import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;

public class AddressControllerTest{

    @Mock
    private AddressService service;
    static List<AddressResponse> responseList;
    List<AddressRequest> addressRequests;
    @InjectMocks
    private AddressController controller;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        AddressResponse response=new AddressResponse();
        response.setAddress("F01");
        response.setMessage("test");
        response.setRecordNo("2");
        response.setStatus("success");
    }
    @Test
    public void  deleteAddresList() throws Exception{
        AddressRequest request1 = new AddressRequest();
        request1.setAddress("F01");
        AddressRequest request2 = new AddressRequest();
        request2.setAddress("F02");

        addressRequests = new ArrayList<>();
        addressRequests.add(request1);
        addressRequests.add(request2);

        service.deleteAddressList(addressRequests);


    }

    @Test
    public void deleteAddressListTest()
    {
        controller.deleteAddressLists(addressRequests);
    }
    @Test
    public void deleteAddressListNullTest()
    {
        controller.deleteAddressLists(null);
    }
}
