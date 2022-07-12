package com.sap.ic.cmh.qualitynotification.handler;

import cds.gen.complaintservice.Complaints;
import cds.gen.qualitynotificationservice.BusinessObjectStatuses;
import cds.gen.qualitynotificationservice.QualityNotifications;
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
import com.sap.ic.cmh.qualitynotification.persistency.QualityNotificationDao;
import com.sap.ic.cmh.qualitynotification.service.QualityNotificationService;
import com.sap.ic.cmh.qualitynotification.validations.QualityNotificationValidation;
import cds.gen.qualitynotificationservice.Defects;
import com.sap.ic.cmh.utils.CommonFunctions;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.utils.QnValidation;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import cds.gen.qualitynotificationservice.Defects;
import java.net.URI;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class QualityNotificationHandlerTest {

    @InjectMocks
    @Autowired
    private QualityNotificationHandler handler;
    @Mock
    QualityNotificationService qualityNotificationService;
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
    CdsReadEventContext cdsReadEventContext;
     @Mock
    DraftNewEventContext draftNewEventContext;
    @Mock
    CqnInsert cqnInsert;
    @Mock
    protected PersistenceService mockDb;
    @Mock
    Result result;
   @Mock
    LoggerHelper loggerHelper;
   @Mock
    QualityNotificationValidation qualityNotificationValidation;
    @Mock
   DestinationService destinationService;

   @Mock
   DestinationConfigurationDao destinationConfigurationDao;

    @Mock
    CommonFunctions commonFunctions;

   @Mock
   private Iterator<ScpCfDestination> readAllDestinations;

    @Mock
    private ScpCfDestination getDestination;

    @Mock
    HttpDestination asHttp;

    @Mock
    URI uri;
    @Mock
    CdsReadEventContext context;

    @Mock
    UserInfo userInfo;
    
    @Mock
    QnValidation qnValidation;

    @Mock 
    private AuditLogDifference auditLogDifference;

    @Mock 
    private AuditLogDifference auditLogDifferenceDefects;

    @Mock
    private QualityNotificationDao qualityNotificationDao;

    private QualityNotifications qualityNotifications;
    private Defects defects;
    private Complaints complaints;
    private BusinessObjectStatuses businessObjectStatuses;
    private List<BusinessObjectStatuses> list;

    private Row row;
    private Optional<Row> opt;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);

        row = Struct.create(Row.class);
        defects = Struct.create(Defects.class);
        defects.setDefectCodeCode("236");
        defects.setIdentifier("45267");
        defects.setDefectGroupCode("F566");
        qualityNotifications = Struct.create(QualityNotifications.class);
        qualityNotifications.setCompanyId("100");
        qualityNotifications.setId(UUID.randomUUID().toString());
        qualityNotifications.setComplaintId("1234");
        qualityNotifications.setMaterialId("F01");
        qualityNotifications.setSupplierId("201");
        qualityNotifications.setPlantId("30");
        qualityNotifications.setStatusCode("QNCRTD");
        complaints = Struct.create(Complaints.class);
        complaints.setCompanyCodeId("101");
        complaints.setMaterialId("F01");
        complaints.setCurrencyCode("In");
        complaints.setComplaintTypeCode("SREC");
        complaints.setReferenceNumber("112");
        complaints.setId("1234");
        when(createContextMock.getCqn()).thenReturn(cqnInsert);
        List<Map<String, Object>> entries = new ArrayList<>();
        when(cqnInsert.entries()).thenReturn(entries);
        businessObjectStatuses = Struct.create(BusinessObjectStatuses.class);
        businessObjectStatuses.setBusinessObjectStatusCode("QNCRTD");
        businessObjectStatuses.setBusinessObjectType("QN");
        list = new ArrayList<>();
        list.add(businessObjectStatuses);
        qualityNotifications.setBusinessObjectStatuses(list);
        


    }

    @Test
    public void beforeQualityNotificationDraftTest(){
        List<QualityNotifications> qnList = new ArrayList<>();
        qnList.add(qualityNotifications);
        List<Row> rowvalues = new ArrayList<>();
        row.put("destination", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(qualityNotificationDao.getDraftQNByComplaintID("1234")).thenReturn(result);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(result.listOf(QualityNotifications.class)).thenReturn(qnList);
        when(qualityNotificationService.getDraftQualityNotificationByComplaintID("1234")).thenReturn(qualityNotifications);
        when(qualityNotificationService.getDraftDefectByQualityNotificationID(qualityNotifications.getId())).thenReturn(defects);
        handler.beforeQualityNotificationDraft(draftNewEventContext,qualityNotifications);

    }

    @Test
    public void beforeQualityNotificationDraftTest_NullCompId(){
        qualityNotifications.setComplaintId(null);
        handler.beforeQualityNotificationDraft(draftNewEventContext,qualityNotifications);


    }

    @Test
    public void beforeQualityNotificationDraftTest_NullDraft(){
        handler.beforeQualityNotificationDraft(draftNewEventContext,qualityNotifications);
    }



    @Test
    public void beforeQualityNotificationPatchTest()  {
         Optional<Complaints> emptyOpt = Optional.of(complaints);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(Complaints.class)).thenReturn(emptyOpt);
        when(complaintService.getMasterDataFromComplaints(any(String.class))).thenReturn(complaints);
         handler.beforeQualityNotificationPatch(qualityNotifications);
    }

    @Test
    public void testBeforeQualityNotificationPatchComplaintNull()  {
        when(complaintService.getMasterDataFromComplaints(any(String.class))).thenReturn(null);
        handler.beforeQualityNotificationPatch(qualityNotifications);
    }

    @Test
    public void testBeforeQualityNotificationPatchComplaintIdNull()  {
        qualityNotifications.setComplaintId(null);
        handler.beforeQualityNotificationPatch(qualityNotifications);
    }

    @Test
    public void beforeQualityNotificationCreateTest()  {
        Optional<Complaints> emptyOpt = Optional.of(complaints);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(Complaints.class)).thenReturn(emptyOpt);
        when(complaintService.getMasterDataFromComplaints(any(String.class))).thenReturn(complaints);
        handler.beforeQualityNotificationCreate(qualityNotifications);
    }

    // @Test
    // public void beforeQualityNotificationUpdateTest()  {
    //     Optional<Complaints> emptyOpt = Optional.of(complaints);
    //     when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
    //     when(result.first(Complaints.class)).thenReturn(emptyOpt);
    //     when(complaintService.getMasterDataFromComplaints(any(String.class))).thenReturn(complaints);
    //     handler.beforeQualityNotificationUpdate(qualityNotifications);
    // }
    @Test
    public void onQualityNotificationCreateTest()  {
        Optional<Complaints> emptyOpt = Optional.of(complaints);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(Complaints.class)).thenReturn(emptyOpt);
        when(complaintService.getComplaintDetails(any(String.class))).thenReturn(complaints);
        handler.onQualityNotificationCreate(qualityNotifications);
    }
    
    @Test
    public void onQualityNotificationCreateTestEmptyBOList()  {
    	qualityNotifications.setBusinessObjectStatuses(new ArrayList<>());
        Optional<Complaints> emptyOpt = Optional.of(complaints);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(Complaints.class)).thenReturn(emptyOpt);
        when(complaintService.getComplaintDetails(any(String.class))).thenReturn(complaints);
        handler.onQualityNotificationCreate(qualityNotifications);
    }


    @Test
    public void testAfterQualityNotificationCreate()  {
        qualityNotifications.setIdentifier("234");
        //ConcurrentMap<String,String> qnNumberMap=new ConcurrentHashMap<String, String>();
        Optional<Complaints> emptyOpt = Optional.of(complaints);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(Complaints.class)).thenReturn(emptyOpt);
        //when(qnValidation.getQnMap()).thenReturn(qnNumberMap);
        when(complaintService.getComplaintDetails(any(String.class))).thenReturn(complaints);
       // handler.afterQualityNotificationCreateUpdate(qualityNotifications);
    }
    
    // @Test
    // public void testAfterQualityNotificationUpdate()  {
    //     qualityNotifications.setIdentifier("234");
    //     Optional<Complaints> emptyOpt = Optional.of(complaints);
    //     when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
    //     when(result.first(Complaints.class)).thenReturn(emptyOpt);
    //     when(complaintService.getComplaintDetails(any(String.class))).thenReturn(complaints);
    //     handler.afterQualityNotificationUpdate(qualityNotifications);
    // }
    @Test
    public void afterQualityNotificationCreateTest()  {
    	//ConcurrentMap<String,String> qnNumberMap=new ConcurrentHashMap<String, String>();
        Optional<Complaints> emptyOpt = Optional.of(complaints);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(Complaints.class)).thenReturn(emptyOpt);
       // when(qnValidation.getQnMap()).thenReturn(qnNumberMap);
        when(complaintService.getComplaintDetails(any(String.class))).thenReturn(complaints);
        //handler.afterQualityNotificationCreateUpdate(qualityNotifications);
    }

    @Test
    public void testBeforeQualityNotificationReadAddress() {
        doNothing().when(commonFunctions).checkBeforeRead(context);
        handler.beforeQualityNotificationReadAddress(context);
    }
    @Test
    public void testBeforeQualityNotificationReadBusinessPartner() {
        doNothing().when(commonFunctions).checkBeforeRead(context);
        handler.beforeQualityNotificationReadBusinessPartner(context);
    }

   @Test
   public void afterQualityNotificationReadUpdateTest()  {
        UserInfo user = UserInfo.create();
        user.hasRole("QualityNotification.Update");
        List<QualityNotifications> emptyOpt = new ArrayList<>();
        emptyOpt.add(qualityNotifications);
        when(destinationService.readAllDestination(any(
               ScpCfDestinationLoader.class))).thenReturn(readAllDestinations);
        when(complaintService.getComplaintDetails(any(String.class))).thenReturn(complaints);
        when(destinationConfigurationDao.getDestinationConfigBasedOnCompanyAndBOType(qualityNotifications.getCompanyId(),
                Constants.QUALITYNOTIFICATION_CODE)).thenReturn(result);

        when(qualityNotificationService.getStatusAndCompanyCode(any(String.class))).thenReturn(qualityNotifications);

        when(cdsReadEventContext.getUserInfo()).thenReturn(user);
       List<Row> rowvalues = new ArrayList<>();
       row.put("navigationDestination", "ComplaintID");
       opt = Optional.of(row);
       rowvalues.add(row);
       when(result.list()).thenReturn(rowvalues);
       when(result.first()).thenReturn(opt);
       when(readAllDestinations.hasNext()).thenReturn(true).thenReturn(false);
       when(readAllDestinations.next()).thenReturn(getDestination);
       when(getDestination.getName()).thenReturn("ComplaintID");
       when(getDestination.isHttp()).thenReturn(true);
       handler.afterQualityNotificationReadUpdate(cdsReadEventContext, emptyOpt);
   }

    @Test
    public void testAfterQualityNotificationReadUpdate()  {
        qualityNotifications.setStatusCode("QNCMPL");
        List<QualityNotifications> emptyOpt = new ArrayList<>();
        emptyOpt.add(qualityNotifications);
        when(destinationService.readAllDestination(any(
                ScpCfDestinationLoader.class))).thenReturn(readAllDestinations);
        when(complaintService.getComplaintDetails(any(String.class))).thenReturn(complaints);
        when(destinationConfigurationDao.getDestinationConfigBasedOnCompanyAndBOType(qualityNotifications.getCompanyId(),
                Constants.QUALITYNOTIFICATION_CODE)).thenReturn(result);

        when(qualityNotificationService.getStatusAndCompanyCode(any(String.class))).thenReturn(qualityNotifications);

        when(cdsReadEventContext.getUserInfo()).thenReturn(userInfo);
        when(userInfo.hasRole(any(String.class))).thenReturn(true);
        List<Row> rowvalues = new ArrayList<>();
        row.put("navigationDestination", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(readAllDestinations.hasNext()).thenReturn(true).thenReturn(false);
        when(readAllDestinations.next()).thenReturn(getDestination);
        when(getDestination.getName()).thenReturn("ComplaintID");
        when(getDestination.isHttp()).thenReturn(true);
        handler.afterQualityNotificationReadUpdate(cdsReadEventContext, emptyOpt);
    }


    @Test
    public void afterQualityNotificationReadUpdateNegativeTest(){
        when(cdsReadEventContext.getUserInfo()).thenReturn(userInfo);
        when(userInfo.hasRole(any(String.class))).thenReturn(true);
        qualityNotifications.setCompanyId(null);
        when(destinationConfigurationDao.getDestinationConfigBasedOnCompanyAndBOType(qualityNotifications.getCompanyId(),
                Constants.QUALITYNOTIFICATION_CODE)).thenReturn(result);
        when(destinationService.readAllDestination(any(
                ScpCfDestinationLoader.class))).thenReturn(readAllDestinations);
        List<QualityNotifications> emptyOpt = new ArrayList<>();
        emptyOpt.add(qualityNotifications);
        List<Row> rowvalues = new ArrayList<>();
        row.put("navigationDestination", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(readAllDestinations.hasNext()).thenReturn(true).thenReturn(false);
        when(readAllDestinations.next()).thenReturn(getDestination);
        when(getDestination.getName()).thenReturn("ComplaintID");
        when(getDestination.isHttp()).thenReturn(true);
        when(qualityNotificationService.getStatusAndCompanyCode(any(String.class))).thenReturn(qualityNotifications);
        handler.afterQualityNotificationReadUpdate(cdsReadEventContext, emptyOpt);
    }

    @Test
    public void testAfterQualityNotificationReadUpdateResultNull(){
        when(cdsReadEventContext.getUserInfo()).thenReturn(userInfo);
        when(userInfo.hasRole(any(String.class))).thenReturn(true);
        qualityNotifications.setCompanyId(null);
        when(destinationConfigurationDao.getDestinationConfigBasedOnCompanyAndBOType(qualityNotifications.getCompanyId(),
                Constants.QUALITYNOTIFICATION_CODE)).thenReturn(result);
        when(destinationService.readAllDestination(any(
                ScpCfDestinationLoader.class))).thenReturn(readAllDestinations);
        List<QualityNotifications> emptyOpt = new ArrayList<>();
        emptyOpt.add(qualityNotifications);
        List<Row> rowvalues = new ArrayList<>();
        row.put("Destination", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(readAllDestinations.hasNext()).thenReturn(true).thenReturn(false);
        when(readAllDestinations.next()).thenReturn(getDestination);
        when(getDestination.getName()).thenReturn("ComplaintID");
        when(getDestination.isHttp()).thenReturn(true);
        when(qualityNotificationService.getStatusAndCompanyCode(any(String.class))).thenReturn(qualityNotifications);
        handler.afterQualityNotificationReadUpdate(cdsReadEventContext, emptyOpt);
    }



    @Test
    public void testSetFieldControlTrue(){
        qualityNotifications.setIsActiveEntity(true);
        qualityNotifications.setHasActiveEntity(false);
        qualityNotifications.setIsQualityNotificationFieldControlMandatory(Constants.FIELD_CONTROL_READ_ONLY);
        qualityNotifications.setIsQualityNotificationFieldControl(Constants.FIELD_CONTROL_READ_ONLY);
        handler.setFieldControl(qualityNotifications);
    }

    @Test
    public void testSetFieldControlFalse(){
        qualityNotifications.setIsActiveEntity(false);
        qualityNotifications.setHasActiveEntity(true);
        qualityNotifications.setIsQualityNotificationFieldControlMandatory(Constants.FIELD_CONTROL_MANDATORY);
        handler.setFieldControl(qualityNotifications);
    }

    @Test
    public void testSetFieldControlBooleanValueNull(){
        qualityNotifications.setIsActiveEntity(null);
        qualityNotifications.setHasActiveEntity(null);
        handler.setFieldControl(qualityNotifications);
    }

    @Test
    public void testLogUpsert() {
        
        QualityNotifications mockedObj = QualityNotifications.create();
        mockedObj.setId("test-id");
        when(qualityNotificationService.getQualityNotificationDetails(anyString())).thenReturn(mockedObj);
        assertDoesNotThrow(() -> handler.logUpsert(mockedObj));
    }

    @Test
    public void testSetOldAuditData() {
        QualityNotifications qualityNotifications = QualityNotifications.create();
        qualityNotifications.setId("123");

        QualityNotifications mockAction = mock(QualityNotifications.class);
        when(qualityNotificationService.getQualityNotificationDetails(anyString())).thenReturn(mockAction);

        assertDoesNotThrow(
                () -> handler.setOldAuditData(qualityNotifications));
    }

    @Test
    public void testAfterQualityNotification_Create(){
        when(qualityNotificationService.getQualityNotificationDetails(qualityNotifications.getId())).thenReturn(qualityNotifications);
        when(qualityNotificationService.getDefectBasedOnQN(qualityNotifications.getId())).thenReturn(defects); 
        assertDoesNotThrow(
                () -> handler.afterQualityNotificationCreate(qualityNotifications));
    }

    @Test
    public void testafterQualityNotificationUpdate(){
        when(qualityNotificationService.getQualityNotificationDetails(qualityNotifications.getId())).thenReturn(qualityNotifications);
        when(qualityNotificationService.getDefectBasedOnQN(qualityNotifications.getId())).thenReturn(defects); 
        assertDoesNotThrow(
                () -> handler.afterQualityNotificationUpdate(qualityNotifications));
    }

    @Test
    public void testAfterQualityNotificationCreateUpdate(){
        handler.afterQualityNotificationCreateUpdate(qualityNotifications);
    }

    @Test
    public void testBeforeQualityNotificationUpdate(){
        handler.beforeQualityNotificationUpdate(qualityNotifications);
    }

    @Test
    public void testSetOldDefectAuditData(){

        when(qualityNotificationService.getDefectBasedOnQN(qualityNotifications.getId())).thenReturn(defects);
        handler.setOldDefectAuditData(qualityNotifications);
    }
}