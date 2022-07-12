// package com.sap.ic.cmh.configuration.service;

// import static org.mockito.Mockito.when;

// import org.junit.Before;
// import org.junit.Test;
// import org.mockito.InjectMocks;
// import org.mockito.Mock;
// import org.mockito.MockitoAnnotations;

// import com.sap.cds.Result;
// import
// com.sap.ic.cmh.configuration.persistency.ReferenceDocumentCatMappingDao;

// public class ReferenceDocumentCatMappingServiceImplTest {

// @InjectMocks
// ReferenceDocumentCatMappingServiceImpl service;

// @Mock
// ReferenceDocumentCatMappingDao referenceDocumentCatMapDao;

// @Mock
// Result result;

// @Before
// public void setBefore() {
// MockitoAnnotations.openMocks(this);
// }

// @Test
// public void testgetReferenceDocumentCategoryMappings() {
// when(referenceDocumentCatMapDao.geReferenceDocumentCategoryMappings()).thenReturn(result);
// service.getReferenceDocumentCategoryMappings();
// }

// }
