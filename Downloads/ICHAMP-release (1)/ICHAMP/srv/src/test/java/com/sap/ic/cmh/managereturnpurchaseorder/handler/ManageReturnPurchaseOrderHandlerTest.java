package com.sap.ic.cmh.managereturnpurchaseorder.handler;

import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import static org.mockito.ArgumentMatchers.any;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.services.ServiceException;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.auditlog.AuditLogDifference;
import com.sap.ic.cmh.complaint.persistency.ComplaintsDao;
import com.sap.ic.cmh.configuration.service.ConfigurationService;
import com.sap.ic.cmh.managereturnpurchaseorder.service.ManageReturnPurchaseOrderService;
import com.sap.ic.cmh.returnpo.handler.ReturnPurchaseOrderHandler;
import com.sap.ic.cmh.returnpo.service.ReturnPurchaseOrderService;
import com.sap.ic.cmh.utils.CommonFunctions;

import cds.gen.managereturnpurchaseorderservice.BusinessObjectStatuses;
import cds.gen.managereturnpurchaseorderservice.ReturnPurchaseOrders;
import cds.gen.masterdataservice.BusinessPartners;

public class ManageReturnPurchaseOrderHandlerTest {

    @InjectMocks
    @Autowired
    ManageReturnPurchaseOrderHandler handler;

    @Mock
    ReturnPurchaseOrderHandler returnPurchaseOrderHandler;

    @Mock
    CommonFunctions commonFunctions;

    @Mock
    ConfigurationService configurationService;

    @Mock
    ComplaintsDao complaintDao;

    @Mock
    Result result;

    @Mock
    AuditLogHelper auditLogHelper;

    @Mock 
    private AuditLogDifference auditLogDifference;
    @Mock
    ManageReturnPurchaseOrderService manageReturnPurchaseOrderService;
    @Mock
    ReturnPurchaseOrderService returnPurchaseOrderService;

    private ReturnPurchaseOrders manageReturnPurchaseOrders;

    cds.gen.returnpurchaseorderservice.ReturnPurchaseOrders returnPurchaseOrders;

    private Row row;
    private Optional<Row> opt;
    private BusinessPartners partner;
    List<BusinessObjectStatuses> boList ;
    BusinessObjectStatuses boStatuses;

    

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);

        row = Struct.create(Row.class);

        manageReturnPurchaseOrders = Struct.create(ReturnPurchaseOrders.class);
        manageReturnPurchaseOrders.setComplaintId("S345");
        manageReturnPurchaseOrders.setCompanyId("WRR");
        manageReturnPurchaseOrders.setComplaintCode("F98877");
        manageReturnPurchaseOrders.setId("111");

        returnPurchaseOrders = Struct.create(cds.gen.returnpurchaseorderservice.ReturnPurchaseOrders.class);
        returnPurchaseOrders.setComplaintId("S345");
        returnPurchaseOrders.setCompanyId("WRR");

        partner = Struct.create(BusinessPartners.class);
        partner.setId("1234");
        
        boList = new ArrayList<>();
		boStatuses = Struct.create(BusinessObjectStatuses.class);
		boStatuses.setBusinessObjectStatusCode("RPOCRTD");
		boStatuses.setBusinessObjectType("RPO");
		boStatuses.setParent("111");
		boList.add(boStatuses);

    }
    public void testbeforeManageReturnPurchaseOrderUpdate() {
        manageReturnPurchaseOrders.setIdentifier("4500000555");
        when(returnPurchaseOrderService.getActiveReturnPurchaseOrders(manageReturnPurchaseOrders.getId())).thenReturn(manageReturnPurchaseOrders);
        handler.beforeManageReturnPurchaseOrderUpdate(manageReturnPurchaseOrders);
        //when(commonFunctions.convertManageReturnPOtoReturnOrders(manageReturnPurchaseOrders)).thenReturn(returnPurchaseOrders);
    }
    @Test
    public void testOnManageReturnPurchaseOrderUpdate() {
        manageReturnPurchaseOrders.setIdentifier("4500000555");

        manageReturnPurchaseOrders.setBusinessObjectStatuses(boList);
        when(commonFunctions.convertManageReturnPOtoReturnOrders(manageReturnPurchaseOrders))
                .thenReturn(returnPurchaseOrders);
        when(manageReturnPurchaseOrderService.updateReturnPurchaseOrderStatus(manageReturnPurchaseOrders))
                .thenReturn(boList);
        handler.onManageReturnPurchaseOrderUpdate(manageReturnPurchaseOrders);

    }
  /*  @Test
    public void testsAfterManageReturnPurchaseOrderUpdate() {
        manageReturnPurchaseOrders.setBusinessObjectStatuses(boList);
        manageReturnPurchaseOrders.setStatusCode("RPOCRTD");
        when(commonFunctions.convertManageReturnPOtoReturnOrders(manageReturnPurchaseOrders))
                .thenReturn(returnPurchaseOrders);
        handler.afterManageReturnPurchaseOrderUpdate(manageReturnPurchaseOrders);

    }*/


    @Test(expected = ServiceException.class)
    public void testBeforeManageReturnPurchaseOrderCreateException() {
        manageReturnPurchaseOrders.setComplaintCode("");
        when(commonFunctions.convertManageReturnPOtoReturnOrders(manageReturnPurchaseOrders)).thenReturn(returnPurchaseOrders);
        when(complaintDao.getComplaintBasedOnCode(manageReturnPurchaseOrders.getComplaintCode())).thenReturn(result);
        handler.beforeManageReturnPurchaseOrderCreate(manageReturnPurchaseOrders);
    }

    @Test
    public void testBeforeManageReturnPurchaseOrderCreate() {
        when(commonFunctions.convertManageReturnPOtoReturnOrders(manageReturnPurchaseOrders)).thenReturn(returnPurchaseOrders);
        when(complaintDao.getComplaintBasedOnCode(manageReturnPurchaseOrders.getComplaintCode())).thenReturn(result);
        handler.beforeManageReturnPurchaseOrderCreate(manageReturnPurchaseOrders);
    }

    @Test
    public void testBeforeManageReturnPurchaseOrderCreateCompPresent() {
        manageReturnPurchaseOrders.setContactPersonCode("C234567");
        manageReturnPurchaseOrders.setPersonResponsibleCode("C345");
        when(commonFunctions.convertManageReturnPOtoReturnOrders(manageReturnPurchaseOrders)).thenReturn(returnPurchaseOrders);
        when(complaintDao.getComplaintBasedOnCode(manageReturnPurchaseOrders.getComplaintCode())).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(configurationService.validateSupplierContactPerson(manageReturnPurchaseOrders.getContactPersonCode())).thenReturn(partner);
        when(configurationService.validatePersonResponsibleCode(manageReturnPurchaseOrders.getPersonResponsibleCode())).thenReturn(partner);

        handler.beforeManageReturnPurchaseOrderCreate(manageReturnPurchaseOrders);
    }


    @Test
    public void testOnManageReturnPurchaseOrderCreate() {
        when(commonFunctions.convertManageReturnPOtoReturnOrders(manageReturnPurchaseOrders)).thenReturn(returnPurchaseOrders);
        handler.onManageReturnPurchaseOrderCreate(manageReturnPurchaseOrders);
    }

    @Test
    public void testAfterManageReturnPurchaseOrderCreate() {
        when(commonFunctions.convertManageReturnPOtoReturnOrders(manageReturnPurchaseOrders)).thenReturn(returnPurchaseOrders);
        when(returnPurchaseOrderService.getReturnPurchaseOrderBasedOnId(any())).thenReturn(returnPurchaseOrders);
        handler.afterManageReturnPurchaseOrderCreate(manageReturnPurchaseOrders);
    }
    
    @Test
    public void testBeforeManageReturnPurchaseOrderUpdate() {
    	manageReturnPurchaseOrders.setIdentifier("4500000555");
    	when(returnPurchaseOrderService.getActiveReturnPurchaseOrders(manageReturnPurchaseOrders.getId())).thenReturn(manageReturnPurchaseOrders);
    	handler.beforeManageReturnPurchaseOrderUpdate(manageReturnPurchaseOrders);
    	//when(commonFunctions.convertManageReturnPOtoReturnOrders(manageReturnPurchaseOrders)).thenReturn(returnPurchaseOrders);
    }
    
	@Test
	public void testonManageReturnPurchaseOrderUpdate() {
		manageReturnPurchaseOrders.setIdentifier("4500000555");
		
		manageReturnPurchaseOrders.setBusinessObjectStatuses(boList);
		when(commonFunctions.convertManageReturnPOtoReturnOrders(manageReturnPurchaseOrders))
				.thenReturn(returnPurchaseOrders);
		when(manageReturnPurchaseOrderService.updateReturnPurchaseOrderStatus(manageReturnPurchaseOrders))
				.thenReturn(boList);
		handler.onManageReturnPurchaseOrderUpdate(manageReturnPurchaseOrders);

	}
  
	@Test
	public void testAfterManageReturnPurchaseOrderUpdate() {
		manageReturnPurchaseOrders.setBusinessObjectStatuses(boList);
		manageReturnPurchaseOrders.setStatusCode("RPOCRTD");
		when(commonFunctions.convertManageReturnPOtoReturnOrders(manageReturnPurchaseOrders))
		.thenReturn(returnPurchaseOrders);
		when(returnPurchaseOrderService.getReturnPurchaseOrderBasedOnId(any())).thenReturn(returnPurchaseOrders);
        handler.afterManageReturnPurchaseOrderUpdate(manageReturnPurchaseOrders);
		
	}
}
