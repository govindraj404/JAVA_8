package com.sap.ic.cmh.auditlog;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import cds.gen.complaintservice.Complaints;


public class AuditOldDataContextTest {
    
    @Autowired @InjectMocks 
    private AuditOldDataContext<Complaints> auditOldDataContext;

    @Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
	}

    @Test
    public void testGetterSetter() {
        Complaints complaints = Complaints.create();
        complaints.setId("ABC");
        auditOldDataContext.set(complaints);

        Complaints retrievedcomComplaints = auditOldDataContext.get();
        assertEquals("ABC", retrievedcomComplaints.getId());
    }

    @Test
    public void testReset() {
        Complaints complaints = Complaints.create();
        complaints.setId("ABC");
        auditOldDataContext.set(complaints);

        auditOldDataContext.reset();

        Complaints retrievedComplaints = auditOldDataContext.get();
        assertNull(retrievedComplaints);
    }
}
