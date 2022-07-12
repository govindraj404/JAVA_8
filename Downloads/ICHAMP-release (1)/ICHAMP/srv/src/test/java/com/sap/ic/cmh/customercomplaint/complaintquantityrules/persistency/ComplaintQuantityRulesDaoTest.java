package com.sap.ic.cmh.customercomplaint.complaintquantityrules.persistency;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.sap.cds.Result;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class ComplaintQuantityRulesDaoTest {
	
	@InjectMocks
	ComplaintQuantityRulesDao dao;
	
	@Mock
	PersistenceService mockDb;
	
	@Mock
	Result result;
	
	@Before
	public void setBefore() {
		MockitoAnnotations.openMocks(this);
	}
	
	@Test
	public void testGetComplaintQuantityRulesBasedOnCode() {
		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		dao.getComplaintQuantityRulesBasedOnCode(any(String.class));
	}

}

