package com.sap.ic.cmh.auditlog;

import static org.junit.Assert.assertEquals;
import java.util.List;
import java.util.Optional;

import cds.gen.complaintservice.BusinessPartners;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import cds.gen.complaintservice.Complaints;


public class AuditLogDifferenceTest {
    @InjectMocks @Autowired
    private AuditLogDifference<Complaints> auditLogDifference;

    @Mock
    AuditOldDataContext<Complaints> auditOldDataContext;

    
	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
	}
    @Test
    public void TestSetOldData(){
        auditLogDifference.setOldData(Complaints.create());
    }

    @Test
    public void testGetDifferenceForCreate() {
        Complaints oldDocType = null;

        Mockito.when(auditOldDataContext.get()).thenReturn(oldDocType);

        Complaints newDocType = Complaints.create();
        newDocType.setId("ABC");

        List<ObjectDiff> diffList = auditLogDifference.getDifference(newDocType);

        assertEquals(1, diffList.size());
        assertEquals("ABC", diffList.get(0).getNewValue());
    }

    @Test
    public void testGetDifferenceForUpdate() {
        Complaints oldDocType = Complaints.create();
        oldDocType.setId("ABC");

        Mockito.when(auditOldDataContext.get()).thenReturn(oldDocType);

        Complaints newDocType = Complaints.create();
        newDocType.setId("ABCD");

        List<ObjectDiff> diffList = auditLogDifference.getDifference(newDocType);

        assertEquals(1, diffList.size());
        assertEquals("ABC", diffList.get(0).getOldValue());
        assertEquals("ABCD", diffList.get(0).getNewValue());
    }

    @Test
    public void testGetDifferenceForUpdate1() {
        Complaints oldDocType = Complaints.create();
        oldDocType.setId("ABC");

        Mockito.when(auditOldDataContext.get()).thenReturn(oldDocType);

        Complaints newDocType = Complaints.create();
        newDocType.setId("ABCD");
        newDocType.setIdentifier("ABCDE");


        List<ObjectDiff> diffList = auditLogDifference.getDifference(newDocType);

        assertEquals(2, diffList.size());
        assertEquals("ABC", diffList.get(0).getOldValue());
        assertEquals("ABCD", diffList.get(0).getNewValue());
    }

        @Test
    public void testGetDifferenceForUpdate2() {
        Complaints oldDocType = Complaints.create();
        oldDocType.setId("ABC");
        oldDocType.setIdentifier("ABCDE");

        Mockito.when(auditOldDataContext.get()).thenReturn(oldDocType);

        Complaints newDocType = Complaints.create();
        newDocType.setId("ABCD");


        List<ObjectDiff> diffList = auditLogDifference.getDifference(newDocType);

        assertEquals(2, diffList.size());
        assertEquals("ABC", diffList.get(0).getOldValue());
    }

}
