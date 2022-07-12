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
// com.sap.ic.cmh.configuration.persistency.TargetReferenceTypeMappingDao;

// import cds.gen.configurationservice.ComplaintCatItemCatMappings;
// import cds.gen.configurationservice.CopyComplaintCatItemCatMappingsContext;

// public class CopyComplaintCatItemCatMappingHandlerTest {

// @InjectMocks
// CopyComplaintCatItemCatMappingHandler handler;

// @Mock
// DraftService draftService;

// @Mock
// TargetReferenceTypeMappingDao complaintCatRefTypeMapDao;

// @Mock
// Messages messages;

// @Mock
// Result result;

// @Mock
// CopyComplaintCatItemCatMappingsContext context;

// @Mock
// CdsService cdsService;

// @Mock
// CqnSelect cqnSelect;

// ComplaintCatItemCatMappings complaintCategoryItemCategoryMaps;

// @Before
// public void setBefore() {
// MockitoAnnotations.openMocks(this);
// complaintCategoryItemCategoryMaps=Struct.create(ComplaintCatItemCatMappings.class);
// complaintCategoryItemCategoryMaps.setId("id");
// }

// @Test
// public void testCopyComplaintCatItemCatMaps() {
// when(context.getCqn()).thenReturn(cqnSelect);
// when(context.getService()).thenReturn(cdsService);
// when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
// when(result.single(ComplaintCatItemCatMappings.class)).thenReturn(complaintCategoryItemCategoryMaps);
// when(complaintCatRefTypeMapDao.getTargetReferenceTypeMappingBasedOnComplaintCatItemCatMap("id")).thenReturn(result);
// when(draftService.newDraft(any(CqnInsert.class))).thenReturn(result);
// handler.copyComplaintCatItemCatMaps(context);

// }
// }
