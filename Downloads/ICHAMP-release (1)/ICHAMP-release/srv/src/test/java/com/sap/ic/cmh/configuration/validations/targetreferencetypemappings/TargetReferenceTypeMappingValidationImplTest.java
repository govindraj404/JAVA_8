package com.sap.ic.cmh.configuration.validations.targetreferencetypemappings;

import cds.gen.configurationservice.TargetDocumentCategories;
import cds.gen.configurationservice.TargetReferenceTypeMappings;
import cds.gen.configurationservice.TargetReferenceTypes;
import cds.gen.configurationservice.TargetTypes;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.impl.ResultImpl;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.service.ComplaintTypeConfigurationService;
import com.sap.ic.cmh.configuration.service.ItemCategoryService;
import com.sap.ic.cmh.configuration.service.targetreferencetypemappings.TargetReferenceTypeMappingService;
import com.sap.ic.cmh.configuration.validations.ConfigurationFieldsValidation;
import com.sap.ic.cmh.configuration.validations.MasterDataValidation;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

public class TargetReferenceTypeMappingValidationImplTest {
    @InjectMocks
    TargetReferenceTypeMappingValidationImpl validation;

    @Mock
    Result result;

    private TargetReferenceTypeMappings mapping;

    @Mock
    Messages message;
    @Mock
    MasterDataValidation masterDataValidation;

    @Mock
    ConfigurationFieldsValidation configurationFieldsValidation;


    List<TargetReferenceTypes> list = new ArrayList<>();
    @Mock
    List<TargetReferenceTypes> list1;
    @Mock
    Message message1;

    @Mock
    protected PersistenceService mockDb;
    @Mock
    TargetReferenceTypeMappingService targetReferenceTypeMappingService;

    @Mock
    ComplaintTypeConfigurationService service;

    @Mock
    ItemCategoryService iService;
    @Mock
    Stream<Message> stream;

    TargetReferenceTypes type;

    TargetTypes targetType;

    TargetDocumentCategories targetDocumentCategory;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        mapping = Struct.create(TargetReferenceTypeMappings.class);
        targetDocumentCategory = Struct.create(TargetDocumentCategories.class);
        targetType = Struct.create(TargetTypes.class);
        type = Struct.create(TargetReferenceTypes.class);

        targetDocumentCategory.setCode("CRDMEM");
        targetDocumentCategory.setDescription("Credit Memo");
        targetType.setId("12312-1231-1232-123");
        targetType.setTargetDocumentCategory(targetDocumentCategory);
        Result result1 = ResultImpl.insertedRows(Collections.singletonList(targetType)).result();
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result1);
    }

    @Test
    public void validateTargetReferenceTypeMappingTest() {
        mapping.setSalesOrganizationId("11");
        mapping.setId("20");
        mapping.setComplaintTypeId("121H");
        mapping.setItemCategoryId("45I");
        type.setTargetTypeId("12312-1231-1232-123");
        type.setTargetType(targetType);
        list.add(type);

        mapping.setTargetTypes(list);
        when(message.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(targetReferenceTypeMappingService.getTargetReferenceTypeMappingBasedOnValues(mapping)).thenReturn(result);
        validation.validateTargetReferenceTypeMapping(mapping);
    }

    @Test
    public void validateTargetReferenceTypeMappingDuplicateCategoryTest() {
        mapping.setSalesOrganizationId("11");
        mapping.setId("20");
        mapping.setComplaintTypeId("121H");
        mapping.setItemCategoryId("45I");
        targetDocumentCategory.setCode("CRDMEM");
        targetDocumentCategory.setDescription("Credit Memo");

        targetType.setId("12312-1231-1232-123");
        targetType.setTargetDocumentCategory(targetDocumentCategory);
        type.setTargetTypeId("12312-1231-1232-123");
        type.setTargetType(targetType);

        TargetTypes targetType1 = Struct.create(TargetTypes.class);
        targetType1.setId("12312-1231-1232-124");
        targetType1.setTargetDocumentCategory(targetDocumentCategory);
        TargetReferenceTypes type1 = Struct.create(TargetReferenceTypes.class);
        type1.setTargetTypeId("12312-1231-1232-124");
        type1.setTargetType(targetType1);

        list.add(type);
        list.add(type1);

        mapping.setTargetTypes(list);
        when(message.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(targetReferenceTypeMappingService.getTargetReferenceTypeMappingBasedOnValues(mapping)).thenReturn(result);
        validation.validateTargetReferenceTypeMapping(mapping);
    }


    @Test
    public void validateComplaintTypeNullTest() {
        mapping.setSalesOrganizationId("11");
        mapping.setId("201");
        type.setTargetTypeId("12312-1231-1232-123");
        mapping.setItemCategoryId("4532");
        type.setTargetType(targetType);
        list.add(type);
        mapping.setTargetTypes(list);
        when(message.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(targetReferenceTypeMappingService.getTargetReferenceTypeMappingBasedOnValues(mapping)).thenReturn(result);
        validation.validateTargetReferenceTypeMapping(mapping);
    }

    @Test
    public void validateReferenceListEmptyTest() {
        mapping.setSalesOrganizationId("11");
        mapping.setId("201");
        mapping.setComplaintTypeId("121H");
        mapping.setItemCategoryId("45I");
        when(list1.isEmpty()).thenReturn(true);
        mapping.setTargetTypes(list1);
        when(message.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(targetReferenceTypeMappingService.getTargetReferenceTypeMappingBasedOnValues(mapping)).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
        row.put("ID", "201");
        row.put("identifier", "2");
        row.put("code", "reasoncode");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(1L);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        validation.validateTargetReferenceTypeMapping(mapping);
    }

    @Test
    public void validateTargetSystemBlankTest() {
        mapping.setSalesOrganizationId("11");
        mapping.setId("201");
        mapping.setComplaintTypeId("121H");
        mapping.setItemCategoryId("45I");
        mapping.setTargetTypes(list);
        when(message.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(targetReferenceTypeMappingService.getTargetReferenceTypeMappingBasedOnValues(mapping)).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
        row.put("ID", "201");
        row.put("identifier", "2");
        row.put("code", "reasoncode");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(1L);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        validation.validateTargetReferenceTypeMapping(mapping);
    }

    @Test
    public void validateResultNotPresentTest() {
        mapping.setSalesOrganizationId("11");
        mapping.setId("201");
        mapping.setComplaintTypeId("121H");
        mapping.setItemCategoryId("45I");
        type.setTargetTypeId("12312-1231-1232-123");
        type.setTargetType(targetType);
        list.add(type);
        mapping.setTargetTypes(list);
        when(message.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(targetReferenceTypeMappingService.getTargetReferenceTypeMappingBasedOnValues(mapping)).thenReturn(result);
        validation.validateTargetReferenceTypeMapping(mapping);
    }

    @Test
    public void validateTargetReferenceTypeMappingMismatchIDTest() {
        mapping.setSalesOrganizationId("11");
        mapping.setId("201");
        mapping.setComplaintTypeId("121H");
        mapping.setItemCategoryId("45I");
        list.add(type);
        mapping.setTargetTypes(list);
        when(message.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(targetReferenceTypeMappingService.getTargetReferenceTypeMappingBasedOnValues(mapping)).thenReturn(result);
        validation.validateTargetReferenceTypeMapping(mapping);
    }


    @Test
    public void validateTargetReferenceTypeMappingFieldErrorAnyTest() {
        mapping.setSalesOrganizationId("11");
        mapping.setId("20");
        mapping.setComplaintTypeId("121H");
        mapping.setItemCategoryId("45I");
        list.add(type);
        mapping.setTargetTypes(list);
        when(message.stream()).thenReturn(stream);
        when(stream.noneMatch(any())).thenReturn(false);
        when(targetReferenceTypeMappingService.getTargetReferenceTypeMappingBasedOnValues(mapping)).thenReturn(result);
        when(iService.getActive(anyString())).thenReturn(true);
        when(service.getActive(anyString())).thenReturn(true);
        validation.validateTargetReferenceTypeMapping(mapping);
    }

    @Test
    public void validateItemCategoryNullTest() {
        mapping.setSalesOrganizationId("11");
        mapping.setId("201");
        type.setTargetTypeId("12312-1231-1232-123");
        mapping.setItemCategoryId("");
        type.setTargetType(targetType);
        list.add(type);
        mapping.setTargetTypes(list);
        when(message.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(targetReferenceTypeMappingService.getTargetReferenceTypeMappingBasedOnValues(mapping)).thenReturn(result);
        validation.validateTargetReferenceTypeMapping(mapping);
    }
}
