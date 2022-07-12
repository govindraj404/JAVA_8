package com.sap.ic.cmh.customercomplaint.referencedocumentcategory.persistency;

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

public class ReferenceDocumentCategoriesDaoTest {
	
	@InjectMocks
	ReferenceDocumentCategoriesDao dao;
	
	@Mock
	PersistenceService mockDb;
	
	@Mock
	Result result;
	
	@Before
	public void setBefore() {
		MockitoAnnotations.openMocks(this);
	}
	
	@Test
	public void testgetReferenceDocumentCategoriesBasedOnCode() {
		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		dao.getReferenceDocumentCategoriesBasedOnCode(any(String.class));
	}

}

