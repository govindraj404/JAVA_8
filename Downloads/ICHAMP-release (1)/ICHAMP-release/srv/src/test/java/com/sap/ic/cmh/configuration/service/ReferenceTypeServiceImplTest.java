package com.sap.ic.cmh.configuration.service;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.sap.cds.Result;
import com.sap.ic.cmh.configuration.persistency.ItemCategoriesDao;
import com.sap.ic.cmh.configuration.persistency.ReferenceTypeDao;

public class ReferenceTypeServiceImplTest {
	
	@InjectMocks
	ReferenceTypeServiceImpl service;
	
	@Mock
	ReferenceTypeDao referenceTypeDao;
	
	@Mock
	Result result;
	
	@Before
	public void setBefore() {
		MockitoAnnotations.openMocks(this);
	}
	
	@Test
	public void testGetReferenceTypes() {
		when(referenceTypeDao.getReferenceTypeDetails()).thenReturn(result);
		service.getReferenceTypes();
	}
	
	@Test
	public void testGetReferenceTypeDetails() {
		when(referenceTypeDao.getReferenceTypeDetailsBasedOnId(anyString())).thenReturn(result);
		service.getReferenceTypesDetails("1e93d863-5b9d-4aae-9a96-375aab27270b");
	}

}


