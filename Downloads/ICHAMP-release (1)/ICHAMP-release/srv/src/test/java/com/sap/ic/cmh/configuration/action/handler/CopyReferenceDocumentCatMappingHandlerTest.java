// package com.sap.ic.cmh.configuration.action.handler;

// import static org.mockito.ArgumentMatchers.any;
// import static org.mockito.Mockito.when;

// import org.junit.Before;
// import org.junit.Test;
// import org.mockito.InjectMocks;
// import org.mockito.Mock;
// import org.mockito.MockitoAnnotations;

// import com.sap.cds.Result;
// import com.sap.cds.Struct;
// import com.sap.cds.ql.cqn.CqnInsert;
// import com.sap.cds.ql.cqn.CqnSelect;
// import com.sap.cds.services.cds.CdsService;
// import com.sap.cds.services.draft.DraftService;
// import com.sap.cds.services.messages.Messages;
// import
// com.sap.ic.cmh.configuration.persistency.SourceReferenceTypeMappingDao;

// import
// cds.gen.configurationservice.CopyReferenceDocumentCategoryMappingsContext;
// import cds.gen.configurationservice.ReferenceDocumentCategoryMappings;

// public class CopyReferenceDocumentCatMappingHandlerTest {

// @InjectMocks
// CopyReferenceDocumentCatMappingHandler handler;

// @Mock
// DraftService draftService;

// @Mock
// SourceReferenceTypeMappingDao sourceReferenceTypeMappingDao;

// @Mock
// Messages messages;

// @Mock
// Result result;

// @Mock
// CopyReferenceDocumentCategoryMappingsContext context;

// @Mock
// CdsService cdsService;

// @Mock
// CqnSelect cqnSelect;

// ReferenceDocumentCategoryMappings referenceDocumentCategoryMappings;

// @Before
// public void setBefore() {
// MockitoAnnotations.openMocks(this);
// referenceDocumentCategoryMappings=Struct.create(ReferenceDocumentCategoryMappings.class);
// referenceDocumentCategoryMappings.setId("id");
// }

// @Test
// public void testCopyComplaintCatItemCatMaps() {
// when(context.getCqn()).thenReturn(cqnSelect);
// when(context.getService()).thenReturn(cdsService);
// when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
// when(result.single(ReferenceDocumentCategoryMappings.class)).thenReturn(referenceDocumentCategoryMappings);
// when(sourceReferenceTypeMappingDao.getSourceReferenceTypeMappingBasedOnReferenceDocumentCatMapping("id")).thenReturn(result);
// when(draftService.newDraft(any(CqnInsert.class))).thenReturn(result);
// handler.copyReferenceDocumentCatMapping(context);

// }
// }
