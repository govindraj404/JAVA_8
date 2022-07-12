package com.sap.ic.cmh.configuration.persistency;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;

import cds.gen.configurationservice.ComplaintCategories;

public class ConfigureItemDaoTest {

	@InjectMocks
	@Autowired
	ConfigureItemDao configureItemDao;

	@Mock
	PersistenceService db;

	@Mock
	Result result;
	
	ComplaintCategories complaintCategories;
	
	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		complaintCategories = Struct.create(ComplaintCategories.class);
		complaintCategories.setCode("SREC");
		complaintCategories.setName("test");
		
		when(db.run(any(CqnSelect.class))).thenReturn(result);
	}

	@Test
	public void testGetComplaintCategoriesBasedOnCode() {
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		configureItemDao.getComplaintCategoriesBasedOnCode("SREC");
	}

}
