// package com.sap.ic.cmh.configuration.handler;

// import static org.mockito.ArgumentMatchers.any;
// import static org.mockito.Mockito.doNothing;
// import static org.mockito.Mockito.when;
// import java.util.*;

// import org.junit.Before;
// import org.junit.Test;
// import org.mockito.InjectMocks;
// import org.mockito.Mock;
// import org.mockito.MockitoAnnotations;
// import com.sap.cds.services.cds.CdsDeleteEventContext;
// import com.sap.cds.Result;
// import com.sap.cds.Row;
// import com.sap.cds.Struct;
// import com.sap.ic.cmh.auditlog.AuditLogDifference;
// import com.sap.cds.services.draft.DraftNewEventContext;
// import com.sap.cds.services.messages.Message;
// import com.sap.cds.services.messages.Messages;
// import com.sap.ic.cmh.utils.CqnAnalyzerUtil;
// import com.sap.ic.cmh.auditlog.AuditLogHelper;
// import static org.mockito.ArgumentMatchers.anyString;
// import
// com.sap.ic.cmh.configuration.persistency.TargetReferenceTypeMappingDao;
// import
// com.sap.ic.cmh.configuration.service.ComplaintCatItemCatMappingService;
// import
// com.sap.ic.cmh.configuration.validations.ComplaintCatItemCatMapValidation;
// import
// com.sap.ic.cmh.configuration.validations.ConfigurationFieldsValidation;
// import cds.gen.configurationservice.ComplaintCatItemCatMappings;
// import cds.gen.configurationservice.TargetReferenceTypeMappings;

// public class ComplaintCatItemCatMappingHandlerTest {

// @InjectMocks
// ComplaintCatItemCatMappingHandler handler;

// @Mock
// ComplaintCatItemCatMappingService service;

// @Mock
// ConfigurationFieldsValidation fieldValidation;

// @Mock
// ComplaintCatItemCatMapValidation mapValidation;

// @Mock
// DraftNewEventContext context;

// @Mock
// private AuditLogDifference<ComplaintCatItemCatMappings> auditLogDifference;

// @Mock
// private AuditLogDifference<TargetReferenceTypeMappings>
// auditLogDifferenceTargetReferenceTypeMappings;

// @Mock
// Messages messages;

// @Mock
// private Message msg;

// @Mock
// AuditLogHelper auditLogHelper;

// @Mock
// CqnAnalyzerUtil cqnAnalyzerUtil;

// @Mock
// Result result;

// @Mock
// CdsDeleteEventContext deleteEventContext;

// @Mock
// TargetReferenceTypeMappingDao targetReferenceTypeMappingDao;

// ComplaintCatItemCatMappings complaintCategoryItemCategoryMaps;
// List<ComplaintCatItemCatMappings> listComplaintCatItemCatMappings=new
// ArrayList<>();
// List<TargetReferenceTypeMappings> mapTargetReferenceTypeMappings=new
// ArrayList<>();
// TargetReferenceTypeMappings targetReferenceTypeMap;
// private Row row;
// private Optional<Row> opt;

// @Before
// public void setBefore() {
// MockitoAnnotations.openMocks(this);
// targetReferenceTypeMap=Struct.create(TargetReferenceTypeMappings.class);
// complaintCategoryItemCategoryMaps=Struct.create(ComplaintCatItemCatMappings.class);
// targetReferenceTypeMap.setComplaintCatItemCatMappingId("id");
// targetReferenceTypeMap.setReferenceTypeId("id1");
// targetReferenceTypeMap.setId("tRid");
// mapTargetReferenceTypeMappings.add(targetReferenceTypeMap);

// complaintCategoryItemCategoryMaps.setComplaintCategoryCode("code");
// complaintCategoryItemCategoryMaps.setItemCategoryId("id");
// complaintCategoryItemCategoryMaps.setId("id1");

// listComplaintCatItemCatMappings.add(complaintCategoryItemCategoryMaps);

// row = Struct.create(Row.class);
// row.put("code", "CODE100");
// row.put("identifier", "11");
// opt = Optional.of(row);
// }

// @Test
// public void testBeforeComplaintCategoryItemCategoryMapDraft() {
// handler.beforeComplaintCategoryItemCategoryMapDraft(context,
// complaintCategoryItemCategoryMaps);
// }

// @Test
// public void testBeforeCreateComplaintCategoryItemCategoryMap() {
// when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
// when(msg.target(any(String.class))).thenReturn(msg);
// doNothing().when(mapValidation).validateComplaintCatItemCatMappings(complaintCategoryItemCategoryMaps);
// handler.beforeCreateComplaintCatItemCatMapping(complaintCategoryItemCategoryMaps);
// }

// @Test
// public void testOnCreateComplaintCategoryItemCategoryMap() {
// when(service.getComplaintCatItemCatMappings()).thenReturn(result);
// when(result.first()).thenReturn(opt);
// handler.onCreateComplaintCatItemCatMapping(complaintCategoryItemCategoryMaps);

// }

// @Test
// public void testOnCreateComplaintCategoryItemCategoryMapWithEmptyData() {
// opt=Optional.empty();
// when(service.getComplaintCatItemCatMappings()).thenReturn(result);
// when(result.first()).thenReturn(opt);
// handler.onCreateComplaintCatItemCatMapping(complaintCategoryItemCategoryMaps);

// }

// @Test
// public void testBeforeUpdateComplaintCategoryItemCategoryMap() {
// when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
// when(msg.target(any(String.class))).thenReturn(msg);
// when(service.getComplaintCatItemCatMappingsById("id1")).thenReturn(result);
// when(service.getAllComplaintCatItemCatMappingById("id1")).thenReturn(result);
// when(targetReferenceTypeMappingDao.getTargetReferenceTypeMappingBasedOnComplaintCatItemCatMapId("id1")).thenReturn(result);
// when(result.listOf(ComplaintCatItemCatMappings.class)).thenReturn(listComplaintCatItemCatMappings);
// handler.beforeUpdateComplaintCatItemCatMapping(complaintCategoryItemCategoryMaps);
// }

// @Test
// public void testAfterCreateUpdateComplaintCategoryItemCategoryMap() {
// when(result.single(ComplaintCatItemCatMappings.class)).thenReturn(complaintCategoryItemCategoryMaps);
// when(result.single(TargetReferenceTypeMappings.class)).thenReturn(targetReferenceTypeMap);
// when(service.getAllComplaintCatItemCatMappingById(complaintCategoryItemCategoryMaps.getId())).thenReturn(result);
// when(targetReferenceTypeMappingDao.getTargetReferenceTypeMappingBasedOnComplaintCatItemCatMapId("id1")).thenReturn(result);
// handler.afterCreateUpdateComplaintCategoryItemCategoryMap(complaintCategoryItemCategoryMaps);
// }

// @Test
// public void testAfterComplaintCategoryItemCategoryMapDeletion() {
// Map<String, Object> targetKeys = new HashMap<>();
// targetKeys.put("ID", "ComplaintCategoryItemCategoryMaps");
// when(cqnAnalyzerUtil.provideTargetKeys(any())).thenReturn(targetKeys);
// handler.afterComplaintCategoryItemCategoryMapDeletion(deleteEventContext);
// }

// }
