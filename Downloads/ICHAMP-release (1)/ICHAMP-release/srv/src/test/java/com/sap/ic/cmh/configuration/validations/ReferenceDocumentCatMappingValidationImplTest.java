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
// com.sap.ic.cmh.configuration.persistency.ReferenceDocumentCatMappingDao;
// import
// com.sap.ic.cmh.customercomplaint.referencedocumentcategory.persistency.ReferenceDocumentCategoriesDao;

// import cds.gen.configurationservice.ReferenceDocumentCategoryMappings;
// import cds.gen.configurationservice.SourceReferenceTypeMappings;

// public class ReferenceDocumentCatMappingValidationImplTest {

// @InjectMocks
// ReferenceDocumentCatMappingValidationImpl validation;

// @Mock
// MasterDataValidation masterDataValidation;

// @Mock
// ConfigurationFieldsValidation fieldsValidation;

// @Mock
// Messages messages;

// @Mock
// ReferenceDocumentCategoriesDao referenceDocumentCategoryDao;

// @Mock
// ReferenceDocumentCatMappingDao refDocumentCatMappingDao;

// @Mock
// Result result;

// @Mock
// private Message msg;

// ReferenceDocumentCategoryMappings refDocCatMap,refDocCatMap1;
// SourceReferenceTypeMappings srcRefTypeMap;
// List<ReferenceDocumentCategoryMappings> refDocCatMapList=new ArrayList<>();
// List<SourceReferenceTypeMappings> srcRefTypeMapList=new ArrayList<>();
// private Row row;
// private Optional<Row> opt;

// @Before
// public void setBefore() {
// MockitoAnnotations.openMocks(this);
// refDocCatMap=Struct.create(ReferenceDocumentCategoryMappings.class);
// srcRefTypeMap=Struct.create(SourceReferenceTypeMappings.class);
// refDocCatMap.setSalesOrganizationId("salesid");
// refDocCatMap.setDistributionChannelId("distributionid");
// refDocCatMap.setDivisionId("divisionid");
// refDocCatMap.setSourceSystem("source");
// refDocCatMap.setId("id");

// srcRefTypeMap.setReferenceTypeId("id");
// srcRefTypeMapList.add(srcRefTypeMap);

// refDocCatMap1=Struct.create(ReferenceDocumentCategoryMappings.class);
// refDocCatMap1.setId("id1");

// refDocCatMapList.add(refDocCatMap1);

// row = Struct.create(Row.class);
// row.put("code", "CODE100");
// row.put("identifier", "11");
// opt = Optional.of(row);
// }

// @Test
// public void testvalidateReferenceDocumentCatMapping1() {
// List<Row>list=new ArrayList<>();
// list.add(row);
// when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
// when(msg.target(any(String.class))).thenReturn(msg);
// when(fieldsValidation.validateDestination(refDocCatMap.getSourceSystem())).thenReturn(false);
// when(refDocumentCatMappingDao.getReferenceDocumentCategoryMappingDetails(refDocCatMap)).thenReturn(result);
// when(result.list()).thenReturn(list);
// when(result.listOf(ReferenceDocumentCategoryMappings.class)).thenReturn(refDocCatMapList);
// validation.validateReferenceDocumentCatMapping(refDocCatMap);
// }

// @Test
// public void testvalidateReferenceDocumentCatMapping2() {
// List<Row>list=new ArrayList<>();
// list.add(row);
// when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
// when(msg.target(any(String.class))).thenReturn(msg);
// when(fieldsValidation.validateDestination(refDocCatMap.getSourceSystem())).thenReturn(true);
// when(fieldsValidation.validateDestinationValue(refDocCatMap.getSourceSystem())).thenReturn(false);
// when(refDocumentCatMappingDao.getReferenceDocumentCategoryMappingDetails(refDocCatMap)).thenReturn(result);
// when(result.list()).thenReturn(list);
// when(result.listOf(ReferenceDocumentCategoryMappings.class)).thenReturn(refDocCatMapList);
// validation.validateReferenceDocumentCatMapping(refDocCatMap);
// }

// @Test
// public void testvalidateSourceReferenceTypeMappings1() {
// validation.validateSourceReferenceTypeMappings(srcRefTypeMapList);
// }

// @Test
// public void testvalidateSourceReferenceTypeMappings2() {
// srcRefTypeMapList.add(srcRefTypeMap);
// when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
// when(msg.target(any(String.class))).thenReturn(msg);
// validation.validateSourceReferenceTypeMappings(srcRefTypeMapList);
// }

// @Test
// public void testvalidateReferenceDocumentCategory() {
// when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
// when(msg.target(any(String.class))).thenReturn(msg);
// when(referenceDocumentCategoryDao.getReferenceDocumentCategoriesBasedOnCode("code")).thenReturn(result);
// validation.validateReferenceDocumentCategory("code");
// }

// }
