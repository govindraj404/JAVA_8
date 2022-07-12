// package com.sap.ic.cmh.configuration.validations;

// import static org.mockito.ArgumentMatchers.any;
// import static org.mockito.Mockito.when;

// import java.util.ArrayList;
// import java.util.List;
// import java.util.Optional;

// import org.junit.Before;
// import org.junit.Test;
// import org.mockito.InjectMocks;
// import org.mockito.Mock;
// import org.mockito.MockitoAnnotations;

// import com.sap.cds.Result;
// import com.sap.cds.Row;
// import com.sap.cds.Struct;
// import com.sap.cds.services.messages.Message;
// import com.sap.cds.services.messages.Messages;
// import
// com.sap.ic.cmh.configuration.persistency.ComplaintCatItemCatMappingDao;

// import cds.gen.configurationservice.ComplaintCatItemCatMappings;
// import cds.gen.configurationservice.TargetReferenceTypeMappings;

// public class ComplaintCatItemCatMapValidationImplTest {

// @InjectMocks
// ComplaintCatItemCatMapValidationImpl validator;

// @Mock
// ConfigurationFieldsValidation fieldsValidation;

// @Mock
// Messages messages;

// @Mock
// private Message msg;

// @Mock
// ComplaintCatItemCatMappingDao complaintCatItemCatMappingsDao;
// @Mock
// Result result;
// List<TargetReferenceTypeMappings> map=new ArrayList<>();
// TargetReferenceTypeMappings targetReferenceTypeMap;
// ComplaintCatItemCatMappings complaintCategoryItemCategoryMaps,
// complaintCategoryItemCategoryMaps1;
// List<ComplaintCatItemCatMappings> listComplaintCatItemCatMappings=new
// ArrayList<>();
// private Row row;
// private Optional<Row> opt;

// @Before
// public void setBefore() {
// MockitoAnnotations.openMocks(this);
// complaintCategoryItemCategoryMaps
// =Struct.create(ComplaintCatItemCatMappings.class);
// complaintCategoryItemCategoryMaps1=Struct.create(ComplaintCatItemCatMappings.class);
// targetReferenceTypeMap=Struct.create(TargetReferenceTypeMappings.class);

// targetReferenceTypeMap.setComplaintCatItemCatMappingId("id");
// targetReferenceTypeMap.setReferenceTypeId("id1");
// map.add(targetReferenceTypeMap);

// complaintCategoryItemCategoryMaps.setComplaintCategoryCode("test");
// complaintCategoryItemCategoryMaps.setItemCategoryId("id");
// complaintCategoryItemCategoryMaps.setId("id");

// complaintCategoryItemCategoryMaps1.setId("id1");
// listComplaintCatItemCatMappings.add(complaintCategoryItemCategoryMaps1);

// row=Struct.create(Row.class);
// row.put("id","test");
// opt=Optional.of(row);
// }

// @Test
// public void testValidateComplaintCatItemCatMappings() {
// when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
// when(msg.target(any(String.class))).thenReturn(msg);
// when(complaintCatItemCatMappingsDao.getComplaintCatItemCatMappingDetails("test","id")).thenReturn(result);
// when(result.listOf(ComplaintCatItemCatMappings.class)).thenReturn(listComplaintCatItemCatMappings);
// validator.validateComplaintCatItemCatMappings(complaintCategoryItemCategoryMaps);
// }

// @Test
// public void testValidatetargetReferenceTypeMappingsIsUnique() {
// when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
// when(msg.target(any(String.class))).thenReturn(msg);
// validator.validatetargetReferenceTypeMappings(map);
// }

// @Test
// public void testValidatetargetReferenceTypeMappingsWithNullData() {
// when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
// when(msg.target(any(String.class))).thenReturn(msg);
// validator.validatetargetReferenceTypeMappings(null);
// }

// @Test
// public void testValidatetargetReferenceTypeMappingsIsNotUnique() {
// map.add(targetReferenceTypeMap);
// when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
// when(msg.target(any(String.class))).thenReturn(msg);
// validator.validatetargetReferenceTypeMappings(map);
// }

// @Test
// public void testValidateUniquecomplaintCatItemCatMappings() {
// List<Row> list=new ArrayList<>();
// list.add(row);

// when(complaintCatItemCatMappingsDao.getComplaintCatItemCatMappingDetails("test","id")).thenReturn(result);
// when(result.list()).thenReturn(list);
// when(result.listOf(ComplaintCatItemCatMappings.class)).thenReturn(listComplaintCatItemCatMappings);
// when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
// when(msg.target(any(String.class))).thenReturn(msg);
// validator.validateUniquecomplaintCatItemCatMappings(complaintCategoryItemCategoryMaps);
// }

// @Test
// public void testValidateUniquecomplaintCatItemCatMappingsWithEmptyData() {
// opt=Optional.empty();
// when(complaintCatItemCatMappingsDao.getComplaintCatItemCatMappingDetails("test","id")).thenReturn(result);
// when(result.listOf(ComplaintCatItemCatMappings.class)).thenReturn(listComplaintCatItemCatMappings);
// when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
// when(msg.target(any(String.class))).thenReturn(msg);
// validator.validateUniquecomplaintCatItemCatMappings(complaintCategoryItemCategoryMaps);
// }

// }
