// package com.sap.ic.cmh.configuration.persistency;

// import static org.mockito.ArgumentMatchers.any;
// import static org.mockito.Mockito.when;
// import org.junit.Before;
// import org.junit.Test;
// import org.mockito.InjectMocks;
// import org.mockito.Mock;
// import org.mockito.MockitoAnnotations;
// import com.sap.cds.Result;
// import com.sap.cds.ql.cqn.CqnSelect;
// import com.sap.cds.services.persistence.PersistenceService;

// public class ComplaintItemCategoriesDaoTest {

// @InjectMocks
// ComplaintItemCategoriesDao ComplaintItemCategoriesDao;

// @Mock
// Result result;

// @Mock
// PersistenceService mockDb;

// @Before
// public void beforeClass() {
// MockitoAnnotations.openMocks(this);
// when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
// }

// @Test
// public void testGetComplaintItemCategory() {
// when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
// ComplaintItemCategoriesDao.getComplaintItemCategory("id");
// }
// }
