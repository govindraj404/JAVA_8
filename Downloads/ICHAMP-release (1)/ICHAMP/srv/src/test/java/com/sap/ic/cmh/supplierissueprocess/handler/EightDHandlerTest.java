
package com.sap.ic.cmh.supplierissueprocess.handler;

import cds.gen.complaintservice.Complaints;
import cds.gen.qualitynotificationservice.QualityNotifications;
import cds.gen.supplierissueprocessservice.BusinessObjectStatuses;
import cds.gen.supplierissueprocessservice.Supplier8DProcesses;
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
import com.sap.cloud.sdk.s4hana.datamodel.odata.namespaces.defectprocessing.Defect;
import com.sap.ic.cmh.auditlog.AuditLogDifference;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.businessobjects.service.BusinessObjectService;
import com.sap.ic.cmh.complaint.persistency.ComplaintsDao;
import com.sap.ic.cmh.complaint.service.ComplaintService;
import com.sap.ic.cmh.complaint.service.StreamService;
import com.sap.ic.cmh.network.service.HttpService;
import com.sap.ic.cmh.qualitynotification.persistency.QualityNotificationDao;
import com.sap.ic.cmh.supplierissueprocess.service.EightDService;
import com.sap.ic.cmh.supplierissueprocess.validations.EightDValidation;
import com.sap.ic.cmh.utils.CommonFunctions;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Stream;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class EightDHandlerTest {
    @InjectMocks
    @Autowired
    EightDHandler handler;
    @Mock
    StreamService streamService;
    @Mock
    EightDService eightDService;
    @Mock
    AuditLogHelper auditLogHelper;
    @Mock
    ComplaintsDao complaintDao;
    @Mock
    HttpService httpService;
    @Mock
    QualityNotificationDao qnDao;
    @Mock
    LoggerHelper loggerHelper;
    @Mock
    DraftNewEventContext draftNewEventContext;
    @Mock
    protected PersistenceService mockDb;
    @Mock
    CdsCreateEventContext createContextMock;
    @Mock
    ComplaintService complaintService;
    @Mock
    Result result;
    @Mock
    CqnInsert cqnInsert;
    @Mock
    Row row;

    @Mock
    Messages messages;

    @Mock
    BusinessObjectService businessObjectService;
    @Mock
    EightDValidation eightDValidation;
    @Mock
    CdsReadEventContext cdsReadEventContext;
    @Mock
    Constants constants;
    @Mock
    UserInfo userInfo;
    @Mock
    CommonFunctions commonFunctions;

    @Mock 
    private AuditLogDifference auditLogDifference;

    private Supplier8DProcesses eightD;
    private Supplier8DProcesses supplierEightD;
    private Stream stream;
    private BusinessObjectStatuses boStatus;
    private Defect defect;
    private QualityNotifications qualityNotifications;
    private QualityNotifications qn;
    private Complaints complaints;

    private final List<Supplier8DProcesses> eightDList = new ArrayList<>();
    private final List<Stream> streamList = new ArrayList<>();
    private final List<BusinessObjectStatuses> boStatusesList = new ArrayList<>();
    private final Map<String, Object> map = new HashMap<String, Object>();
    private final List<Map<String, Object>> boStatusList = new ArrayList<Map<String, Object>>();
    private static final String boType = Constants.SUPPLIER_EIGHTD_CODE;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        qualityNotifications = Struct.create(QualityNotifications.class);
        map.put("Defects", new Defect());
        qualityNotifications.setDefect(map);
        eightD = Struct.create(Supplier8DProcesses.class);
        eightD.setId("EightDID");
        eightD.setIdentifier("002152458");
        eightD.setStatusCode("INTL");
        eightD.setComplaintId("1234");
        supplierEightD = Struct.create(Supplier8DProcesses.class);
        supplierEightD.setId(eightD.getId());
        supplierEightD.setStatusCode(eightD.getStatusCode());
        supplierEightD.setComplaintId(eightD.getComplaintId());
        stream = Struct.create(Stream.class);
        streamList.add(stream);
        boStatus = Struct.create(BusinessObjectStatuses.class);
        boStatus.setId("S8DACPTD");
        boStatus.setId("100");
        map.put("code", "S8DSUBTS");
        map.put("type_code", "S8D");
        map.put("visited", false);
        map.put("sequenceNumber", "004");
        map.put("toStatus", "");
        boStatusList.add(map);
        boStatusesList.add(boStatus);
        eightD.setBusinessObjectStatuses(boStatusesList);
        eightDList.add(eightD);
        complaints = Struct.create(Complaints.class);
        complaints.setPlantId("Plant");
        complaints.setMaterialId("Id");
        complaints.setSupplierId("Id");
        complaints.setPurchasingOrganizationId("Id");
        complaints.setQuantity(BigDecimal.ZERO);
        complaints.setCompanyCodeId("Id");
        complaints.setUnitCode("234");
    }

    @Test
    public void testBeforeQualityNotificationDraft() {
        Optional<Supplier8DProcesses> optional = Optional.of(supplierEightD);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(Supplier8DProcesses.class)).thenReturn(optional);
        when(eightDService.getEightDDetails(any(String.class))).thenReturn(supplierEightD);
        handler.beforeQualityNotificationDraft(draftNewEventContext, supplierEightD);
    }

    @Test
    public void testBeforeSupplier8DPatch() {
        when(qnDao.fetchQNForSupplier8d(eightD.getComplaintId())).thenReturn(qualityNotifications);
        when(complaintService.getMasterDataFromComplaints("1234")).thenReturn(complaints);
        when(eightDService.getEightDDetails(any(String.class))).thenReturn(supplierEightD);
        handler.beforeSupplier8DPatch(supplierEightD);
    }

    @Test
    public void testBeforeSupplier8DPatchComplaintIdNull() {
        supplierEightD.setComplaintId(null);
        handler.beforeSupplier8DPatch(supplierEightD);
    }

    @Test
    public void testBeforeSupplier8DCreate() {
        handler.beforeSupplier8DCreate(supplierEightD);
    }

    @Test
    public void testBeforeSupplier8DUpdate() {
        when(eightDService.getEightDDetails(supplierEightD.getId())).thenReturn(supplierEightD);
        handler.beforeSupplier8DUpdate(supplierEightD);
    }

    @Test
    public void testOnSupplier8DCreate() {
        UserInfo user = UserInfo.create();
        when(createContextMock.getUserInfo()).thenReturn(user);

        eightD = Struct.create(Supplier8DProcesses.class);
        eightD.setId("EightDID");
        eightD.setStatusCode("INTL");
        eightD.setComplaintId("1234");
        supplierEightD = Struct.create(Supplier8DProcesses.class);
        supplierEightD.setId(eightD.getId());
        supplierEightD.setStatusCode(eightD.getStatusCode());
        supplierEightD.setComplaintId(eightD.getComplaintId());
        Optional<Supplier8DProcesses> optional = Optional.of(supplierEightD);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(Supplier8DProcesses.class)).thenReturn(optional);
        when(eightDService.getEightDDetails(any(String.class))).thenReturn(supplierEightD);
        handler.onSupplier8DCreate(supplierEightD, createContextMock);
    }

    @Test
    public void testSetStatusCode() {
        eightD = Struct.create(Supplier8DProcesses.class);
        eightD.setId("EightDID");
        eightD.setStatusCode("INTL");
        eightD.setComplaintId("1234");
        supplierEightD = Struct.create(Supplier8DProcesses.class);
        supplierEightD.setId(eightD.getId());
        supplierEightD.setStatusCode(eightD.getStatusCode());
        supplierEightD.setComplaintId(eightD.getComplaintId());
    }

    @Test
    public void testAfterSupplier8DCreate(){
    Optional<Supplier8DProcesses> optional = Optional.of(supplierEightD);
    when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
    when(result.first(Supplier8DProcesses.class)).thenReturn(optional);
    when(eightDService.getEightDDetails(any(String.class))).thenReturn(supplierEightD);
    when(eightDService.getEightDBasedOnId(any(String.class))).thenReturn(supplierEightD);
    handler.afterSupplier8DCreate(eightD);
    }

    @Test
    public void testBeforeSupplier8DReadAddress() {
        doNothing().when(commonFunctions).checkBeforeRead(cdsReadEventContext);
        handler.beforeSupplier8DReadAddress(cdsReadEventContext);
    }

    @Test
    public void testBeforeSupplier8DReadBusinessPartner() {
        doNothing().when(commonFunctions).checkBeforeRead(cdsReadEventContext);
        handler.beforeSupplier8DReadBusinessPartner(cdsReadEventContext);
    }

    @Test
    public void testAfterSupplier8DReadUpdate() {
        UserInfo user = UserInfo.create();
        user.hasRole("SupplierIssueProcess.Update");
        when(cdsReadEventContext.getUserInfo()).thenReturn(user);
        eightD.setIsUpdateRestricted(user.hasRole("SupplierIssueProcess.Update"));
        Optional<Supplier8DProcesses> optional = Optional.of(supplierEightD);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(Supplier8DProcesses.class)).thenReturn(optional);
        when(eightDService.getEightDDetails(any(String.class))).thenReturn(supplierEightD);
        eightD.setIsSupplierFieldControl(constants.FIELD_CONTROL_READ_ONLY);
        eightD.getStatusCode().equalsIgnoreCase(constants.SUPPLIER_ISSUE_PROCESS_STATUS_CREATED);
        eightD.getStatusCode().equalsIgnoreCase(constants.SUPPLIER_ISSUE_PROCESS_STATUS_CLOSED);
        eightD.getIsSupplierFieldControl();
        handler.afterSupplier8DReadUpdate(cdsReadEventContext, eightDList);
    }

    @Test
    public void testAfterSupplier8DReadUpdateTrue() {
        when(userInfo.hasRole(any(String.class))).thenReturn(true);
        supplierEightD.setIsUpdateRestricted(false);
        supplierEightD.setStatusCode("S8DCMPL");
        when(cdsReadEventContext.getUserInfo()).thenReturn(userInfo);
        eightD.setIsUpdateRestricted(userInfo.hasRole("SupplierIssueProcess.Update"));
        Optional<Supplier8DProcesses> optional = Optional.of(supplierEightD);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(Supplier8DProcesses.class)).thenReturn(optional);
        when(eightDService.getEightDDetails(any(String.class))).thenReturn(supplierEightD);
        handler.afterSupplier8DReadUpdate(cdsReadEventContext, eightDList);
    }

    @Test
    public void testAfterSupplier8DReadUpdateTrueStatusS8DCRTD() {
        when(userInfo.hasRole(any(String.class))).thenReturn(true);
        supplierEightD.setIsUpdateRestricted(false);
        supplierEightD.setStatusCode("S8DCRTD");
        when(cdsReadEventContext.getUserInfo()).thenReturn(userInfo);
        eightD.setIsUpdateRestricted(userInfo.hasRole("SupplierIssueProcess.Update"));
        Optional<Supplier8DProcesses> optional = Optional.of(supplierEightD);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(Supplier8DProcesses.class)).thenReturn(optional);
        when(eightDService.getEightDDetails(any(String.class))).thenReturn(supplierEightD);
        handler.afterSupplier8DReadUpdate(cdsReadEventContext, eightDList);
    }

    @Test
    public void testAfterSupplier8DReadUpdateEightDNull() {
        when(cdsReadEventContext.getUserInfo()).thenReturn(userInfo);
        when(userInfo.hasRole(any(String.class))).thenReturn(true);
        Optional<Supplier8DProcesses> optional = Optional.of(supplierEightD);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(Supplier8DProcesses.class)).thenReturn(optional);
        when(eightDService.getEightDDetails(any(String.class))).thenReturn(null);
        handler.afterSupplier8DReadUpdate(cdsReadEventContext, eightDList);
    }

    @Test
    public void testLogUpsert() {
        
        Supplier8DProcesses mockedObj = Supplier8DProcesses.create();
        mockedObj.setId("test-id");
        when(eightDService.getEightDBasedOnId(anyString())).thenReturn(mockedObj);
        assertDoesNotThrow(() -> handler.logUpsert(mockedObj));
    }

    @Test
    public void testSetOldAuditData() {
        Supplier8DProcesses supplier8dProcesses = Supplier8DProcesses.create();
        supplier8dProcesses.setId("123");

        Supplier8DProcesses mockAction = mock(Supplier8DProcesses.class);
        when(eightDService.getEightDDetails(anyString())).thenReturn(mockAction);

        assertDoesNotThrow(
                () -> handler.setOldAuditData(supplier8dProcesses));
    }
    
}
