package com.sap.ic.cmh.managereturnpurchaseorder.service;


import cds.gen.com.sap.ic.cmh.returnpurchaseorderstatusmapping.ReturnPurchaseOrderStatusMappings;
import cds.gen.complaintservice.Complaints;
import cds.gen.managereturnpurchaseorderservice.BusinessObjectStatuses;
import cds.gen.managereturnpurchaseorderservice.ReturnPurchaseOrders;
import com.sap.cds.Result;

import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.ic.cmh.businessobjects.persistency.BusinessObjectDao;
import com.sap.ic.cmh.businessobjects.service.BusinessObjectService;
import org.junit.Before;

import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class ManageReturnPurchaseOrderServiceImplTest {

    @InjectMocks
    @Autowired
    ManageReturnPurchaseOrderServiceImpl manageReturnPurchaseOrderService;

    @Mock
    BusinessObjectDao businessObjectDao;

    @Mock
    BusinessObjectService businessObjectService;

    @Mock
    ReturnPurchaseOrders manageReturnPurchaseOrders;

    @Mock
    Result result;

    private BusinessObjectStatuses businessObjectStatuses;
    List<BusinessObjectStatuses> bolist = new ArrayList<>();

    private Row row;
    private Optional<Row> opt;
    private ReturnPurchaseOrderStatusMappings returnPurchaseOrderStatusMappings;
    List<ReturnPurchaseOrderStatusMappings> returnPurchaseOrderStatusMappingsList = new ArrayList<>();
    private ReturnPurchaseOrders returnPurchaseOrders;


    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);

        businessObjectStatuses = Struct.create(BusinessObjectStatuses.class);
        businessObjectStatuses.setId("123");
        businessObjectStatuses.setBusinessObjectType("complaint");
        businessObjectStatuses.setBusinessObjectStatusCode("RPC");
        businessObjectStatuses.setBusinessObjectType("aaa");
        businessObjectStatuses.setBusinessObjectStatus(returnPurchaseOrders);

        bolist.add(businessObjectStatuses);

        row = Struct.create(Row.class);

        returnPurchaseOrderStatusMappings = Struct.create(ReturnPurchaseOrderStatusMappings.class);
        returnPurchaseOrderStatusMappings.setCode("RPC");
        returnPurchaseOrderStatusMappings.setStatusCode("RPC");
        returnPurchaseOrderStatusMappings.setStatus(returnPurchaseOrders);
        returnPurchaseOrderStatusMappingsList.add(returnPurchaseOrderStatusMappings);

        returnPurchaseOrders =Struct.create(ReturnPurchaseOrders.class);
        returnPurchaseOrders.setId("123");
        returnPurchaseOrders.setComplaintId("123");
        returnPurchaseOrders.setStatusCode("RPC");
        returnPurchaseOrders.setComplaintCode("123");
        returnPurchaseOrders.setBusinessObjectStatuses(bolist);
        //returnPurchaseOrders.setStatus();

    }

    @Test
    public void testGetStatus() {

        when(businessObjectService.determineReturnOrderStatus("backendStatusCode")).thenReturn("backendStatusCode");
        manageReturnPurchaseOrderService.getStatus("backendStatusCode");

    }

    @Test
    public void testFindExistingBusinessObjectStatusList(){
        List<Row> rowvalues = new ArrayList<>();
        row.put("BusinessObj", businessObjectStatuses);
        Optional<Row> opt = Optional.of(row);
        rowvalues.add(row);

        when(businessObjectDao.findBOStatusBasedOnBOId(businessObjectStatuses.getId())).thenReturn(result);
        when(result.listOf(BusinessObjectStatuses.class)).thenReturn(bolist);
        when(result.first()).thenReturn(opt);
        manageReturnPurchaseOrderService.findExistingBusinessObjectStatusList(businessObjectStatuses.getId());
    }

    @Test
    public void testSetBusinessObjectStatuses(){
        manageReturnPurchaseOrderService.setBusinessObjectStatuses(businessObjectStatuses,businessObjectStatuses.getId());
    }


    @Test
    public void testUpdateReturnPurchaseOrderStatus(){
        List<Row> rowvalues = new ArrayList<>();
        row.put("BusinessObj", businessObjectStatuses);
        Optional<Row> opt = Optional.of(row);
        rowvalues.add(row);
        when(businessObjectDao.getAllReturnPOStatusMappings()).thenReturn(returnPurchaseOrderStatusMappingsList);

        when(businessObjectDao.findBOStatusCode(businessObjectStatuses.getId(), returnPurchaseOrderStatusMappings.getStatusCode())).thenReturn(result);
        when(result.listOf(BusinessObjectStatuses.class)).thenReturn(bolist);
        when(result.first()).thenReturn(opt);
        when(businessObjectDao.findBOStatusBasedOnBOId(businessObjectStatuses.getId())).thenReturn(result);
        when(result.listOf(BusinessObjectStatuses.class)).thenReturn(bolist);
        when(result.first()).thenReturn(opt);
        when(businessObjectService.determineReturnOrderStatus("backendStatusCode")).thenReturn("backendStatusCode");
        manageReturnPurchaseOrderService.updateReturnPurchaseOrderStatus(returnPurchaseOrders);

    }

    @Test
    public void testUpdateReturnPurchaseOrderStatus_boStatusesNotExists(){
        List<Row> rowvalues = new ArrayList<>();
        row.put("", businessObjectStatuses);
        Optional<Row> opt = Optional.of(row);
        rowvalues.add(row);
        List<BusinessObjectStatuses> list = new ArrayList<>();

        when(businessObjectDao.getAllReturnPOStatusMappings()).thenReturn(returnPurchaseOrderStatusMappingsList);

        when(businessObjectDao.findBOStatusCode(businessObjectStatuses.getId(), returnPurchaseOrderStatusMappings.getStatusCode())).thenReturn(result);
        when(result.listOf(BusinessObjectStatuses.class)).thenReturn(list);
        when(result.first()).thenReturn(opt);
        when(businessObjectDao.findBOStatusBasedOnBOId(businessObjectStatuses.getId())).thenReturn(result);
        when(result.listOf(BusinessObjectStatuses.class)).thenReturn(list);
        when(result.first()).thenReturn(opt);
        when(businessObjectService.determineReturnOrderStatus("backendStatusCode")).thenReturn("backendStatusCode");
        manageReturnPurchaseOrderService.updateReturnPurchaseOrderStatus(returnPurchaseOrders);
    }



}
