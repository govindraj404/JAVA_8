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

// public class ComplaintCatItemCatMappingDaoTest {

// @InjectMocks
// ComplaintCatItemCatMappingDao dao;

// @Mock
// PersistenceService mockDb;
// Result result;

// @Before
// public void setBefore() {
// MockitoAnnotations.openMocks(this);
// }

// @Test
// public void testGetComplaintCategoryItemCategoryMaps() {
// when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
// dao.getComplaintCatItemCatMappings();
// }

// @Test
// public void testGetComplaintCategoryItemCategoryMapDetails() {
// when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
// dao.getComplaintCatItemCatMappingDetails(null, null);
// }

// @Test
// public void testGetComplaintCatItemCatMappingBasedOnId() {
// when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
// dao.getComplaintCatItemCatMappingBasedOnId(null);
// }

// @Test
// public void testGetAllComplaintCatItemCatMappingById() {
// when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
// dao.getAllComplaintCatItemCatMappingById(null);
// }
// }
