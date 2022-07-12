package com.sap.ic.cmh.returnpo.persistency;

import cds.gen.complaintservice.Complaints;
import cds.gen.masterdataservice.Addresses;
import cds.gen.returnpurchaseorderservice.ReturnPurchaseOrders;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnDelete;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.cds.services.draft.DraftService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

public class ReturnPurchaseOrderDaoTest {

    @Mock
    DraftService draftService;

    @InjectMocks
    ReturnPurchaseOrderDao returnPurchaseOrderDao;
    @Mock
    PersistenceService db;
    @Mock
    CdsCreateEventContext createContextMock;
    @Mock
    CqnInsert cqnInsert;
    @Mock
    CqnUpdate cqnUpdate;
    @Mock
    Result result;
    private static ReturnPurchaseOrders returnPurchaseOrders;
    private static Complaints complaints;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        returnPurchaseOrders = Struct.create(ReturnPurchaseOrders.class);
        complaints = Struct.create(Complaints.class);
        complaints.setCompanyCodeId("12");
        complaints.setMaterialId("201");
        returnPurchaseOrders.setId("111");
        returnPurchaseOrders.setCompanyId("201");
        returnPurchaseOrders.setComplaintId("101");
        // returnPurchaseOrders.setContactPerson("sap");
        returnPurchaseOrders.setReturnPurchaseType("PO");
        returnPurchaseOrders.setIdentifier("test");
        returnPurchaseOrders.setSupplierId("201");
        when(createContextMock.getCqn()).thenReturn(cqnInsert);
        List<Map<String, Object>> entries = new ArrayList<>();
        when(cqnInsert.entries()).thenReturn(entries);
    }

    @Test
    public void getReturnPurchaseOrderDetailsTest() {
        returnPurchaseOrderDao.getReturnPurchaseOrderDetails(returnPurchaseOrders.getReturnPurchaseType());
    }

    @Test
    public void getReturnPurchaseOrderBasedOnIdTest() {
        returnPurchaseOrderDao.getReturnPurchaseOrderBasedOnId(returnPurchaseOrders.getId());
    }

    @Test
    public void checkIfReturnPOExistsBasedOnNumberTest() {
        returnPurchaseOrderDao.checkIfReturnPOExistsBasedOnNumber(returnPurchaseOrders.getReturnPurchaseType());
    }

    @Test
    public void getReturnPurchaseOrderDetailsBasedOnComplaintIdTest() {
        returnPurchaseOrderDao
                .getReturnPurchaseOrderDetailsBasedOnComplaintId(returnPurchaseOrders.getReturnPurchaseType());
    }

    @Test
    public void getActiveReturnPurchaseOrdersTest() {
        Optional<cds.gen.managereturnpurchaseorderservice.ReturnPurchaseOrders> opt = Optional.empty();
        when(db.run(any(CqnSelect.class), any(Object[].class))).thenReturn(result);
        returnPurchaseOrderDao.getActiveReturnPurchaseOrders(anyString());
    }
    @Test
    public void getDraftReturnOrderByComplaintIDTest() {
        returnPurchaseOrders.setComplaintId("101");
        Optional<cds.gen.managereturnpurchaseorderservice.ReturnPurchaseOrders> opt = Optional.empty();
        when(db.run(any(CqnSelect.class), any(Object[].class))).thenReturn(result);
        returnPurchaseOrderDao.getDraftReturnOrderByComplaintID(anyString());
    }

    @Test
    public void getReturnOrderStatusAndCompanyCodeTest() {
        returnPurchaseOrders.setStatusCode("RPOCRTD");
        returnPurchaseOrderDao.getReturnOrderStatusAndCompanyCode(returnPurchaseOrders.getId());
    }

    @Test
    public void testDeleteDraftReturnOrderByID() {
        returnPurchaseOrderDao.deleteDraftReturnOrderByID(returnPurchaseOrders.getId());
    }

}
