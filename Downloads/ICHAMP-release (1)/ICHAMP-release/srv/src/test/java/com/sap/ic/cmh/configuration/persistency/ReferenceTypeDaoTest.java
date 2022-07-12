package com.sap.ic.cmh.configuration.persistency;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.sap.cds.Result;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;

public class ReferenceTypeDaoTest {
	
	@InjectMocks
	ReferenceTypeDao dao;
	
	@Mock
	PersistenceService mockDb;
	
	Result result;
	
	@Before
	public void setBefore() {
		MockitoAnnotations.openMocks(this);
	}
	
	@Test
	public void testGetReferenceTypeBasedOnId() {
		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		dao.getReferenceTypeBasedOnId(any(String.class));
	}
	
	@Test
	public void testGetReferenceDetailsTypeBasedOnId() {
		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		dao.getReferenceTypeDetailsBasedOnId(any(String.class));
	}

	@Test
	public void testGetReferenceTypeDetails() {
		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		dao.getReferenceTypeDetails();
	}
	
	@Test
	public void testGetReferenceTypeBasedOnCode() {
		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		dao.getReferenceTypeBasedOnCode(any(String.class));
	}
}
