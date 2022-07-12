// package com.sap.ic.cmh.configuration.handler;

// import static org.mockito.Mockito.doNothing;
// import static org.mockito.Mockito.when;

// import java.util.Optional;

// import org.junit.Before;
// import org.junit.Test;
// import org.mockito.InjectMocks;
// import org.mockito.Mock;
// import org.mockito.MockitoAnnotations;

// import com.sap.cds.Result;
// import com.sap.cds.Row;
// import com.sap.cds.Struct;
// import com.sap.cds.services.draft.DraftNewEventContext;
// import com.sap.cds.services.messages.Messages;
// import
// com.sap.ic.cmh.configuration.service.ReferenceDocumentCatMappingService;
// import
// com.sap.ic.cmh.configuration.validations.ReferenceDocumentCatMappingValidation;

// import cds.gen.configurationservice.ReferenceDocumentCategoryMappings;

// public class ReferenceDocumentCatMappingHandlerTest {

// @InjectMocks
// ReferenceDocumentCatMappingHandler referenceDocumentCatMappingHandler;

// @Mock
// ReferenceDocumentCatMappingValidation validation;

// @Mock
// ReferenceDocumentCatMappingService service;

// @Mock
// DraftNewEventContext context;

// @Mock
// Result result;

// ReferenceDocumentCategoryMappings refDocCatMap;

// @Mock
// Messages messages;

// private Row row;
// private Optional<Row> opt;

// @Before
// public void setBfore() {
// MockitoAnnotations.openMocks(this);
// refDocCatMap=Struct.create(ReferenceDocumentCategoryMappings.class);
// refDocCatMap.setSalesOrganizationId("salesid");
// refDocCatMap.setDistributionChannelId("distributionid");
// refDocCatMap.setDivisionId("divisionid");

// row = Struct.create(Row.class);
// row.put("code", "CODE100");
// row.put("identifier", "11");
// opt = Optional.of(row);
// }

// @Test
// public void testbeforeCreateReferenceDocumentCategoryMappingDraft() {
// referenceDocumentCatMappingHandler.beforeCreateReferenceDocumentCategoryMappingDraft(context,
// refDocCatMap);
// }

// @Test
// public void testbeforeCreateReferenceDocumentCategoryMapping() {
// doNothing().when(validation).validateReferenceDocumentCatMapping(refDocCatMap);
// referenceDocumentCatMappingHandler.beforeCreateReferenceDocumentCategoryMapping(refDocCatMap);
// }

// @Test
// public void testonCreateSourceReferenceTypeMap() {
// when(service.getReferenceDocumentCategoryMappings()).thenReturn(result);
// when(result.first()).thenReturn(opt);
// referenceDocumentCatMappingHandler.onCreateSourceReferenceTypeMap(refDocCatMap);
// }

// @Test
// public void testbeforeUpdateSourceReferenceTypeMap() {
// doNothing().when(validation).validateReferenceDocumentCatMapping(refDocCatMap);
// referenceDocumentCatMappingHandler.beforeUpdateSourceReferenceTypeMap(refDocCatMap);
// }
// }
