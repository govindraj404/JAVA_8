package com.sap.ic.cmh.returnpo.handler;

import cds.gen.complaintservice.Complaints;
import cds.gen.returnpurchaseorderservice.ReturnPurchaseOrders;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.cds.CdsReadEventContext;
import com.sap.cds.services.draft.DraftNewEventContext;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.cds.services.request.UserInfo;
import com.sap.cloud.sdk.cloudplatform.connectivity.HttpDestination;
import com.sap.cloud.sdk.cloudplatform.connectivity.ScpCfDestination;
import com.sap.cloud.sdk.cloudplatform.connectivity.ScpCfDestinationLoader;
import com.sap.ic.cmh.auditlog.AuditLogDifference;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.businessobjects.service.BusinessObjectService;
import com.sap.ic.cmh.complaint.service.ComplaintService;
import com.sap.ic.cmh.complaint.service.StreamService;
import com.sap.ic.cmh.configuration.persistency.DestinationConfigurationDao;
import com.sap.ic.cmh.network.service.DestinationService;
import com.sap.ic.cmh.returnpo.service.ReturnPurchaseOrderService;
import com.sap.ic.cmh.returnpo.validation.ReturnPurchaseOrderValidation;
import com.sap.ic.cmh.utils.CommonFunctions;
import com.sap.ic.cmh.utils.Constants;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.*;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class ReturnPurchaseOrderHandlerTest {
    @InjectMocks
    ReturnPurchaseOrderHandler handler;
    @Mock
    ReturnPurchaseOrderService returnPurchaseOrderService;
    @Mock
    ComplaintService complaintService;
    @Mock
    AuditLogHelper auditLogHelper;
    @Mock
    BusinessObjectService businessObjectService;
    @Mock
    StreamService streamService;
    @Mock
    Messages messages;
    @Mock
    CdsCreateEventContext createContextMock;
    @Mock
    CqnInsert cqnInsert;
    @Mock
    Result result;
    @Mock
    DraftNewEventContext context;
    @Mock
    protected PersistenceService mockDb;
    @Mock
    CdsReadEventContext readEventContext;
    @Mock
    UserInfo user;
    @Mock
    UserInfo userInfo;

    @Mock
    DestinationService destinationService;

    @Mock
    DestinationConfigurationDao destinationConfigurationDao;

    @Mock
    private Iterator<ScpCfDestination> readAllDestinations;

    @Mock
    private ScpCfDestination getDestination;

    @Mock
    HttpDestination asHttp;

    @Mock
    URI uri;

    @Mock
    ReturnPurchaseOrderValidation returnPurchaseOrderValidation;

    @Mock
    CommonFunctions commonFunctions;

    @Mock 
    private AuditLogDifference auditLogDifference;


    private static  ReturnPurchaseOrders returnPurchaseOrders;
    private static Complaints complaints;

    private Row row;
    private Optional<Row> opt;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        row = Struct.create(Row.class);
        returnPurchaseOrders = Struct.create(ReturnPurchaseOrders.class);
        complaints = Struct.create(Complaints.class);
        complaints.setCompanyCodeId("12");
        complaints.setMaterialId("201");
        returnPurchaseOrders.setId("123");
        returnPurchaseOrders.setCompanyId("201");
        returnPurchaseOrders.setComplaintId("101");
        //returnPurchaseOrders.setContactPerson("sap");
        returnPurchaseOrders.setReturnPurchaseType("PO");
        returnPurchaseOrders.setIdentifier("test");
        returnPurchaseOrders.setSupplierId("201");
        returnPurchaseOrders.setStatusCode("RPOCRTD");
        when(createContextMock.getCqn()).thenReturn(cqnInsert);
        List<Map<String, Object>> entries = new ArrayList<>();
        when(cqnInsert.entries()).thenReturn(entries);
    }
    @Test
    public void testBeforeReturnPurchaseOrderDraft()  {
        Optional<ReturnPurchaseOrders> emptyOpt = Optional.of(returnPurchaseOrders);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(ReturnPurchaseOrders.class)).thenReturn(emptyOpt);
        handler.beforeReturnPurchaseOrderDraft(context,returnPurchaseOrders);
    }
    @Test
    public void testBeforeReturnPurchaseOrderPatch()  {
        Optional<ReturnPurchaseOrders> emptyOpt = Optional.of(returnPurchaseOrders);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(complaintService.getMasterDataFromComplaints(any(String.class))).thenReturn(complaints);
        when(result.first(ReturnPurchaseOrders.class)).thenReturn(emptyOpt);
        handler.beforeReturnPurchaseOrderPatch(returnPurchaseOrders);
    }
    @Test
    public void testAfterReturnPurchaseOrderReadUpdateRPONull()  {
        Optional<ReturnPurchaseOrders> emptyOpt = Optional.of(returnPurchaseOrders);
        List<ReturnPurchaseOrders> listPo=new ArrayList<>();
        listPo.add(returnPurchaseOrders);
        when(readEventContext.getUserInfo()).thenReturn(user);
        when(user.hasRole(any(String.class))).thenReturn(true);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("navigationDestination", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(destinationService.readAllDestination(any(
                ScpCfDestinationLoader.class))).thenReturn(readAllDestinations);
        when(destinationConfigurationDao.getDestinationConfigBasedOnCompanyAndBOType("201",
                Constants.RETURNPO_CODE)).thenReturn(result);
        when(readAllDestinations.hasNext()).thenReturn(true).thenReturn(false);
        when(readAllDestinations.next()).thenReturn(getDestination);
        when(getDestination.getName()).thenReturn("ComplaintID");
        when(getDestination.isHttp()).thenReturn(true);
        handler.afterReturnPurchaseOrderReadUpdate(readEventContext,listPo);
    }


    @Test
    public void testAfterReturnPurchaseOrderReadUpdate()  {
        Optional<ReturnPurchaseOrders> emptyOpt = Optional.of(returnPurchaseOrders);
        List<ReturnPurchaseOrders> listPo=new ArrayList<>();
        listPo.add(returnPurchaseOrders);
        returnPurchaseOrders.setStatusCode("RPOCRTD");
        when(readEventContext.getUserInfo()).thenReturn(user);
        when(user.hasRole(any(String.class))).thenReturn(true);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("navigationDestination", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(destinationService.readAllDestination(any(
                ScpCfDestinationLoader.class))).thenReturn(readAllDestinations);
        when(destinationConfigurationDao.getDestinationConfigBasedOnCompanyAndBOType("201",
                Constants.RETURNPO_CODE)).thenReturn(result);
        when(returnPurchaseOrderService
                .getReturnOrderStatusAndCompanyCode(returnPurchaseOrders.getId())).thenReturn(returnPurchaseOrders);
        when(readAllDestinations.hasNext()).thenReturn(true).thenReturn(false);
        when(readAllDestinations.next()).thenReturn(getDestination);
        when(getDestination.getName()).thenReturn("ComplaintID");
        when(getDestination.isHttp()).thenReturn(true);
        when(getDestination.asHttp()).thenReturn(asHttp);
        when(asHttp.getUri()).thenReturn(uri);

        handler.afterReturnPurchaseOrderReadUpdate(readEventContext,listPo);
    }

    @Test
    public void testAfterReturnPurchaseOrderReadUpdateResultNull()  {
        Optional<ReturnPurchaseOrders> emptyOpt = Optional.of(returnPurchaseOrders);
        List<ReturnPurchaseOrders> listPo=new ArrayList<>();
        listPo.add(returnPurchaseOrders);
        returnPurchaseOrders.setStatusCode("NEW");
        when(readEventContext.getUserInfo()).thenReturn(user);
        when(user.hasRole(any(String.class))).thenReturn(true);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(destinationService.readAllDestination(any(
                ScpCfDestinationLoader.class))).thenReturn(readAllDestinations);
        when(destinationConfigurationDao.getDestinationConfigBasedOnCompanyAndBOType("201",
                Constants.RETURNPO_CODE)).thenReturn(result);
        when(returnPurchaseOrderService
                .getReturnOrderStatusAndCompanyCode(returnPurchaseOrders.getId())).thenReturn(returnPurchaseOrders);
        when(readAllDestinations.hasNext()).thenReturn(true).thenReturn(false);
        when(readAllDestinations.next()).thenReturn(getDestination);
        when(getDestination.getName()).thenReturn("ComplaintID");
        when(getDestination.isHttp()).thenReturn(true);
        when(getDestination.asHttp()).thenReturn(asHttp);
        when(asHttp.getUri()).thenReturn(uri);

        handler.afterReturnPurchaseOrderReadUpdate(readEventContext,listPo);
    }

    @Test
    public void testAfterReturnPurchaseOrderReadUpdateStatusClosed()  {
        Optional<ReturnPurchaseOrders> emptyOpt = Optional.of(returnPurchaseOrders);
        List<ReturnPurchaseOrders> listPo=new ArrayList<>();
        listPo.add(returnPurchaseOrders);
        returnPurchaseOrders.setStatusCode("RPOCLSD");
        when(readEventContext.getUserInfo()).thenReturn(user);
        when(user.hasRole(any(String.class))).thenReturn(true);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("navigationDestination", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(destinationService.readAllDestination(any(
                ScpCfDestinationLoader.class))).thenReturn(readAllDestinations);
        when(destinationConfigurationDao.getDestinationConfigBasedOnCompanyAndBOType("201", Constants.RETURNPO_CODE)).thenReturn(result);
        when(returnPurchaseOrderService.getReturnOrderStatusAndCompanyCode(returnPurchaseOrders.getId())).thenReturn(returnPurchaseOrders);
        when(readAllDestinations.hasNext()).thenReturn(true).thenReturn(false);
        when(readAllDestinations.next()).thenReturn(getDestination);
        when(getDestination.getName()).thenReturn("ComplaintID");
        when(getDestination.isHttp()).thenReturn(true);
        when(getDestination.asHttp()).thenReturn(asHttp);
        when(asHttp.getUri()).thenReturn(uri);
        handler.afterReturnPurchaseOrderReadUpdate(readEventContext,listPo);
    }



    @Test
    public void testBeforeReturnPurchaseOrderCreate()  {
        Optional<ReturnPurchaseOrders> emptyOpt = Optional.of(returnPurchaseOrders);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(complaintService.getMasterDataFromComplaints(any(String.class))).thenReturn(complaints);
        when(result.first(ReturnPurchaseOrders.class)).thenReturn(emptyOpt);
        handler.beforeReturnPurchaseOrderCreate(returnPurchaseOrders);
    }

    @Test
    public void testOnReturnPurchaseOrderCreate()  {
        Optional<ReturnPurchaseOrders> emptyOpt = Optional.of(returnPurchaseOrders);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(ReturnPurchaseOrders.class)).thenReturn(emptyOpt);
        handler.onReturnPurchaseOrderCreate(returnPurchaseOrders);
    }

    // @Test
    // public void testAfterReturnPurchaseOrderCreate()  {
    //     Optional<ReturnPurchaseOrders> emptyOpt = Optional.of(returnPurchaseOrders);
    //     when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
    //     when(result.first(ReturnPurchaseOrders.class)).thenReturn(emptyOpt);
    //     handler.afterReturnPurchaseOrderCreate(returnPurchaseOrders);
    // }

    @Test
    public void testUpdateStreamStatus()  {
        Optional<ReturnPurchaseOrders> emptyOpt = Optional.of(returnPurchaseOrders);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(ReturnPurchaseOrders.class)).thenReturn(emptyOpt);
        handler.updateStreamStatus(returnPurchaseOrders,"test",true);
    }

    @Test
    public void testBeforeReturnPurchaseOrderUpdate()  {
        Optional<ReturnPurchaseOrders> emptyOpt = Optional.of(returnPurchaseOrders);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(ReturnPurchaseOrders.class)).thenReturn(emptyOpt);
        when(returnPurchaseOrderService.getReturnPurchaseOrderBasedOnId(returnPurchaseOrders.getId())).thenReturn(returnPurchaseOrders);
        handler.beforeReturnPurchaseOrderUpdate(returnPurchaseOrders);
    }


    @Test
    public void testBeforeReturnPurchaseOrderReadAddress() {
        doNothing().when(commonFunctions).checkBeforeRead(readEventContext);
        handler.beforeReturnPurchaseOrderReadAddress(readEventContext);
    }
    @Test
    public void testBeforeReturnPurchaseOrderReadBusinessPartner() {
        doNothing().when(commonFunctions).checkBeforeRead(readEventContext);
        handler.beforeReturnPurchaseOrderReadBusinessPartner(readEventContext);
    }
    @Test
    public void setFieldControlTest() {
        returnPurchaseOrders.setIsActiveEntity(true);
        returnPurchaseOrders.setHasActiveEntity(true);
        handler.setFieldControl(returnPurchaseOrders);
    }
    @Test
    public void setFieldControlElseTest() {
        returnPurchaseOrders.setIsActiveEntity(true);
        returnPurchaseOrders.setHasActiveEntity(false);
        handler.setFieldControl(returnPurchaseOrders);
    }
    @Test
    public void testSetFieldControlElseOppoTest() {
        returnPurchaseOrders.setIsActiveEntity(false);
        returnPurchaseOrders.setHasActiveEntity(true);
        handler.setFieldControl(returnPurchaseOrders);
    }

    @Test
    public void testSetFieldControlElse() {
        returnPurchaseOrders.setIsActiveEntity(false);
        returnPurchaseOrders.setHasActiveEntity(false);
        handler.setFieldControl(returnPurchaseOrders);
    }

    @Test
    public void testBeforeReturnPurchaseOrderPatchElse()  {
        returnPurchaseOrders.setComplaintId(null);
        handler.beforeReturnPurchaseOrderPatch(returnPurchaseOrders);
    }
  @Test
    public void testBeforeReturnPurchaseOrderPatchNull()  {
        Optional<ReturnPurchaseOrders> emptyOpt = Optional.of(returnPurchaseOrders);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(complaintService.getMasterDataFromComplaints(any(String.class))).thenReturn(null);
        when(result.first(ReturnPurchaseOrders.class)).thenReturn(emptyOpt);
        handler.beforeReturnPurchaseOrderPatch(returnPurchaseOrders);
    }

    @Test
    public void testLogUpsert() {
        
        ReturnPurchaseOrders mockedObj = ReturnPurchaseOrders.create();
        mockedObj.setId("test-id");
        when(returnPurchaseOrderService.getReturnPurchaseOrderBasedOnId(anyString())).thenReturn(mockedObj);
        assertDoesNotThrow(() -> handler.logUpsert(mockedObj));
    }

    @Test
    public void testSetOldAuditData() {
        ReturnPurchaseOrders returnPurchaseOrders = ReturnPurchaseOrders.create();
        returnPurchaseOrders.setId("123");

        ReturnPurchaseOrders mockAction = mock(ReturnPurchaseOrders.class);
        when(returnPurchaseOrderService.getReturnPurchaseOrderBasedOnId(anyString())).thenReturn(mockAction);

        assertDoesNotThrow(
                () -> handler.setOldAuditData(returnPurchaseOrders));
    }

    @Test
    public void testAfterReturnPurchaseOrderCreate(){
        when(returnPurchaseOrderService.getReturnPurchaseOrderBasedOnId(returnPurchaseOrders.getId())).thenReturn(returnPurchaseOrders);
         assertDoesNotThrow(
                () -> handler.afterReturnPurchaseOrderCreate(returnPurchaseOrders));
    }



}
