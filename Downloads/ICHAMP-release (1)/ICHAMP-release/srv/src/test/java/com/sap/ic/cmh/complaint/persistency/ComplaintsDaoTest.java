package com.sap.ic.cmh.complaint.persistency;

import cds.gen.complaintservice.Complaints;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnDelete;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.persistence.PersistenceService;
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
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;


public class ComplaintsDaoTest {


    @InjectMocks @Autowired
    ComplaintsDao complaintDao;
    @Mock
    CqnUpdate update;
    @Mock
    protected PersistenceService mockDb;
    @Mock
    Result result;

    private Row row;
    private Optional<Row> opt;

    Complaints complaint;
    private List<Complaints> complaintLists = new ArrayList<>();

    @Before
    public void setup() {
        MockitoAnnotations.openMocks(this);
        complaint = Struct.create(Complaints.class);
        complaint.setId("ComplaintID");
        complaint.setReferenceNumber("1234");
        complaintLists.add(complaint);

        row = Struct.create(Row.class);
    }

    @Test
    public void testGetDefaultType() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        complaintDao.getDefaultType();
    }

    @Test
    public void testGetDefaultStatus() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        complaintDao.getDefaultStatus();
    }

    @Test
    public void testUpdateComplaint() {
        when(mockDb.run(any(CqnUpdate.class))).thenReturn(result);
        complaintDao.updateComplaint(complaint);
    }

    @Test
    public void testGetComplaintDetails() {
        complaintDao.getComplaintDetails(complaint.getId());
    }

    @Test
    public void testGetAllComplaints() {
        complaintDao.getAllComplaints();
    }
    @Test
    public void getActiveComplaintsForSupplierTest() {
        when(mockDb.run(any(CqnUpdate.class))).thenReturn(result);
        complaintDao.getActiveComplaintsForSupplier(complaint.getId());
    }

    @Test
    public void getActiveComplaintsForPersonResponsibleTest() {
        when(mockDb.run(any(CqnUpdate.class))).thenReturn(result);
        complaintDao.getActiveComplaintsForPersonResponsible(complaint.getId());
    }
    @Test
    public void testGetComplaintBasedOnCode() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        complaintDao.getComplaintBasedOnCode("F768");
    }

    @Test
    public  void testGetIsComplaintStatusClosedBasedOnSupplier(){
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        complaintDao.getIsComplaintStatusClosedBasedOnSupplier("1233");
    }

    @Test
    public  void testGetIsComplaintStatusClosedBasedOnSupplierResultNotNull(){
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        Optional<Row> row1 = mock(Optional.class);
        when(result.first()).thenReturn(row1);
        when(row1.isPresent()).thenReturn(true);
        complaintDao.getIsComplaintStatusClosedBasedOnSupplier("1233");
    }

    @Test
    public  void testGetIsComplaintStatusClosedBasedOnContactPerson(){
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        complaintDao.getIsComplaintStatusClosedBasedOnContactPerson("1234");
    }

    @Test
    public  void testGetIsComplaintStatusClosedBasedOnContactPersonNull(){
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        Optional<Row> row1 = mock(Optional.class);
        when(result.first()).thenReturn(row1);
        when(row1.isPresent()).thenReturn(false);
        complaintDao.getIsComplaintStatusClosedBasedOnContactPerson("1234");
    }

    @Test
    public void testDeleteAutomaticComplaint() {
        when(mockDb.run(any(CqnDelete.class))).thenReturn(result);
        complaintDao.deleteAutomaticComplaint("1234");
    }

    @Test
    public void testGetMasterDataFromComplaints() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        complaintDao.getMasterDataFromComplaints("1234");
    }

    @Test
    public void testGetComplaintCreationType() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        complaintDao.getComplaintCreationTypeAndCompanyCode("1234");
    }
    
    @Test
    public void testGetComplaintCategoryBasedOnCode() {
    	when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
    	complaintDao.getComplaintCategoryBasedOnCode("code");
    }
}
