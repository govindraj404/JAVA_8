// package com.sap.ic.cmh.configuration.persistency;

// import org.junit.Before;
// import org.junit.Test;
// import org.mockito.InjectMocks;
// import org.mockito.Mock;
// import org.mockito.MockitoAnnotations;

// import com.sap.cds.Result;
// import com.sap.cds.Struct;
// import com.sap.cds.ql.cqn.CqnSelect;
// import com.sap.cds.services.persistence.PersistenceService;

// import cds.gen.configurationservice.ReferenceDocumentCategoryMappings;

// import static org.mockito.ArgumentMatchers.any;
// import static org.mockito.Mockito.when;

// public class ReferenceDocumentCatMappingDaoTest {

// @InjectMocks
// ReferenceDocumentCatMappingDao dao;

// @Mock
// PersistenceService mockDb;

// @Mock
// Result result;

// ReferenceDocumentCategoryMappings
// refDocCatMap=Struct.create(ReferenceDocumentCategoryMappings.class);

// @Before
// public void setBefore() {
// MockitoAnnotations.openMocks(this);
// }

// @Test
// public void testgeReferenceDocumentCategoryMappings() {
// when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
// dao.geReferenceDocumentCategoryMappings();
// }

// @Test
// public void testgetReferenceDocumentCategoryMappingDetails() {
// when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
// dao.getReferenceDocumentCategoryMappingDetails(refDocCatMap);
// }

// }
