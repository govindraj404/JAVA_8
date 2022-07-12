package com.sap.ic.cmh.qualitynotification.persistency;

import cds.gen.complaintservice.Complaints;
import cds.gen.qualitynotificationservice.Defects;
import cds.gen.qualitynotificationservice.QualityNotifications;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnDelete;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.draft.DraftService;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.complaint.service.ComplaintService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class QualityNotificationDAOTest {
    @InjectMocks
    @Autowired
    QualityNotificationDao qualityNotificationDao;
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
    DraftService draftService;

    private Complaints complaints;
    private QualityNotifications qn;
    private QualityNotifications qualityNotifications;
    private List<QualityNotifications> boList = new ArrayList<>();

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        qn = Struct.create(QualityNotifications.class);
        qn.setId("111");
        qn.setStatusCode("QNCRTD");
        qn.setComplaintId("444");
        qualityNotifications = Struct.create(QualityNotifications.class);
        qualityNotifications.setId(qn.getId());
        qualityNotifications.setStatusCode(qn.getStatusCode());
        qualityNotifications.setComplaintId(qn.getComplaintId());
        boList.add(qn);
    }

    @Test
    public void testUpdateQualityNotification() {
        when(mockDb.run(any(CqnUpdate.class))).thenReturn(result);
        when(result.single(QualityNotifications.class)).thenReturn(qn);
        qualityNotificationDao.updateQualityNotification(qn);
    }

    @Test
    public void testGetQualityNotificationDetails() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(QualityNotifications.class)).thenReturn(boList);
        List<Row> rowvalues = new ArrayList<>();
        Optional<Row> opt = Optional.of(row);
        row.put("ID", "111");
        rowvalues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        qualityNotificationDao.getQualityNotificationDetails(qn.getId());

    }

    @Test
    public void testGetQualityNotificationDetailsByComplaintId() {
        when(result.listOf(QualityNotifications.class)).thenReturn(boList);
        List<Row> rowvalues = new ArrayList<>();
        Optional<Row> opt = Optional.of(row);
        row.put("COMPLAINT_ID", "444");
        rowvalues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first()).thenReturn(opt);
        qualityNotificationDao.getQualityNotificationDetailsByComplaintId(qn.getComplaintId());

    }

    @Test
    public void testCheckIfQNExistsBasedOnNumber() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(QualityNotifications.class)).thenReturn(boList);
        qualityNotificationDao.checkIfQNExistsBasedOnNumber(qn.getId());
    }

    @Test
    public void testFetchQNForSupplier8d() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(QualityNotifications.class)).thenReturn(boList);
        List<Row> rowvalues = new ArrayList<>();
        Optional<Row> opt = Optional.of(row);
        row.put("COMPLAINT_ID", "444");
        rowvalues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        qualityNotificationDao.fetchQNForSupplier8d(qn.getComplaintId());
    }

    @Test
    public void testGetDefectCode() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(QualityNotifications.class)).thenReturn(qn);
        qualityNotificationDao.getDefectCode("F004");
    }

    @Test
    public void testGetDefectGroup() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(QualityNotifications.class)).thenReturn(qn);
        qualityNotificationDao.getDefectGroup("group");
    }

    @Test
    public void testGetDefectBasedOnQN() {
        Defects defects = Struct.create(Defects.class);
        defects.setDefectCodeCode("2345");
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(Defects.class)).thenReturn(defects);
        qualityNotificationDao.getDefectBasedOnQN("group");
    }

    @Test
    public void testDeleteAutomaticQN() {
        when(mockDb.run(any(CqnDelete.class))).thenReturn(result);
        qualityNotificationDao.deleteAutomaticQN("1234");
    }

    @Test
    public void testGetStatusAndCompanyCode() {
        qn.setCompanyId("123");
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        qualityNotificationDao.getStatusAndCompanyCode(qn.getId());
    }

    @Test
    public void testGetDraftQNByComplaintID(){
        qn.setComplaintId("123");
        when(draftService.run(any(CqnSelect.class))).thenReturn(result);
        qualityNotificationDao.getDraftQNByComplaintID(qn.getId());
    }

    @Test
    public void testDeleteDraftQNByID(){
        qualityNotificationDao.deleteDraftQNByID(qn.getId());
    }

    @Test
    public void testGetDraftDefectByQualityNotificationID(){
        when(draftService.run(any(CqnSelect.class))).thenReturn(result);
        qualityNotificationDao.getDraftDefectByQualityNotificationID(qn.getId());
    }

    @Test
    public void testGetQualityNotificationDetailsFromActive(){
        when(draftService.run(any(CqnSelect.class))).thenReturn(result);
        qualityNotificationDao.getQualityNotificationDetailsFromActive(qn.getId());
    }

    @Test
    public void testGetDefectBasedOnQNFromActive(){
        when(draftService.run(any(CqnSelect.class))).thenReturn(result);
        qualityNotificationDao.getDefectBasedOnQNFromActive(qn.getId());
    }

    @Test
    public void testCheckIfActiveQNExistsBasedOnNumber(){
        when(draftService.run(any(CqnSelect.class))).thenReturn(result);
        qualityNotificationDao.checkIfActiveQNExistsBasedOnNumber(qn.getNumber());
    }
}
