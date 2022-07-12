// package com.sap.ic.cmh.configuration.persistency;

// import static org.mockito.ArgumentMatchers.any;
// import static org.mockito.Mockito.when;

// import org.junit.Before;
// import org.junit.Test;
// import org.mockito.InjectMocks;
// import org.mockito.Mock;
// import org.mockito.MockitoAnnotations;
// import org.springframework.beans.factory.annotation.Autowired;

// import com.sap.cds.Result;
// import com.sap.cds.ql.cqn.CqnSelect;
// import com.sap.cds.services.persistence.PersistenceService;

// public class DestinationMappingDaoTest {
	
// 	@InjectMocks
// 	@Autowired
// 	DestinationMappingDao destinationMappingDao;
	
// 	@Mock
// 	Result result;
	
// 	@Mock
// 	PersistenceService mockDb;
	
	
// 	@Before
// 	public void beforeClass() {
// 		MockitoAnnotations.openMocks(this);
// 	}
	
// 	@Test
// 	public void testGetSalesAreaDestinationSystemMapsBasedOnComplaintType() {
// 		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
// 		destinationMappingDao.getDestinationMappingBasedOnComplaintTypeConfig("123"); 
// 	}

// 	@Test
// 	public void testGetDestinationMappingBasedOnComplaintTypeConfigurationId() {
// 		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
// 		destinationMappingDao.getDestinationMappingBasedOnComplaintTypeConfigurationId("123"); 
// 	}
// }

