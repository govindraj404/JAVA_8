// package com.sap.ic.cmh.configuration.service;

// import static org.mockito.Mockito.when;

// import org.junit.Before;
// import org.junit.Test;
// import org.mockito.InjectMocks;
// import org.mockito.Mock;
// import org.mockito.MockitoAnnotations;

// import com.sap.cds.Result;
// import
// com.sap.ic.cmh.configuration.persistency.ComplaintCatItemCatMappingDao;

// public class ComplaintCatItemCatMappingServiceImplTest {

// @InjectMocks
// ComplaintCatItemCatMappingServiceImpl service;

// @Mock
// ComplaintCatItemCatMappingDao dao;

// @Mock
// Result result;

// @Before
// public void setBefore() {
// MockitoAnnotations.openMocks(this);
// }

// @Test
// public void testGetComplaintCategoryItemCategoryMaps() {
// when(dao.getComplaintCatItemCatMappings()).thenReturn(result);
// service.getComplaintCatItemCatMappings();
// }

// @Test
// public void testGetComplaintCatItemCatMappingsById() {
// when(dao.getComplaintCatItemCatMappingBasedOnId(null)).thenReturn(result);
// service.getComplaintCatItemCatMappingsById(null);
// }

// @Test
// public void testGetAllComplaintCatItemCatMappingById() {
// when(dao.getAllComplaintCatItemCatMappingById(null)).thenReturn(result);
// service.getAllComplaintCatItemCatMappingById(null);
// }
// }
