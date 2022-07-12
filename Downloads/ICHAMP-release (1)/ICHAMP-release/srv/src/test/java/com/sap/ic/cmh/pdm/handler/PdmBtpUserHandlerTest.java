package com.sap.ic.cmh.pdm.handler;

import cds.gen.complaintservice.BTPUsers;
import com.sap.cds.Struct;
import com.sap.cds.services.cds.CdsReadEventContext;
import com.sap.cds.services.request.ParameterInfo;
import com.sap.ic.cmh.complaint.service.ComplaintService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.Mockito.when;

public class PdmBtpUserHandlerTest {

    @InjectMocks
    @Autowired
    PdmBtpUserHandler handler;

    @Mock
    CdsReadEventContext context;

    @Mock
    ParameterInfo parameterInfo;

    @Mock
    ComplaintService complaintService;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void testBeforeBTPUsersRead() {
        Map<String, String> map1 =new HashMap<>();
        map1.put("$filter" ,"ab' cd ghr");
        when(parameterInfo.getQueryParams()).thenReturn(map1);
        when(context.getParameterInfo()).thenReturn(parameterInfo);

        BTPUsers btpUsers = Struct.create(BTPUsers.class);
        btpUsers.setPersonResponsibleId("ghr");
        btpUsers.setPersonResponsibleNumber(" cd ghr");
        List< BTPUsers > list = new ArrayList<>();
        list.add(btpUsers);
        when(complaintService.getAllResponsiblePerson()).thenReturn(list);
        handler.beforeBTPUsersRead(context);
    }

    @Test
    public void testBeforeBTPUsersReadEmailValidate() {
        Map<String, String> map1 =new HashMap<>();
        map1.put("$filter" ,"ab' cd ghr");
        when(parameterInfo.getQueryParams()).thenReturn(map1);
        when(context.getParameterInfo()).thenReturn(parameterInfo);

        BTPUsers btpUsers = Struct.create(BTPUsers.class);
        btpUsers.setPersonResponsibleId("abc");
        btpUsers.setPersonResponsibleNumber("123456789");
        List< BTPUsers > list = new ArrayList<>();
        list.add(btpUsers);
        when(complaintService.getAllResponsiblePerson()).thenReturn(list);
        handler.beforeBTPUsersRead(context);
    }

    @Test
    public void testBeforeBTPUsersReadNull() {
        Map<String, String> map1 =new HashMap<>();
        map1.put("$filter" , null);
        when(parameterInfo.getQueryParams()).thenReturn(map1);
        when(context.getParameterInfo()).thenReturn(parameterInfo);

        BTPUsers btpUsers = Struct.create(BTPUsers.class);
        btpUsers.setPersonResponsibleId("12");
        btpUsers.setPersonResponsibleNumber("123456789");
        List< BTPUsers > list = new ArrayList<>();
        list.add(btpUsers);
        when(complaintService.getAllResponsiblePerson()).thenReturn(list);
        handler.beforeBTPUsersRead(context);
    }
}