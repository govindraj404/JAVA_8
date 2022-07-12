package com.sap.ic.cmh.configuration.validations;

import cds.gen.configurationservice.SourceReferenceTypeMappings;
import cds.gen.configurationservice.SourceReferenceTypes;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.persistency.SourceReferenceTypeMappingDao;
import com.sap.ic.cmh.configuration.service.sourcereferencetypemappings.SourceReferenceTypeMappingService;
import com.sap.ic.cmh.configuration.validations.sourcereferencetypemappings.SourceReferenceTypeMappingValidationImpl;
import com.sap.ic.cmh.gen.MessageKeys;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class SourceReferenceTypeMappingValidationImplTest {

    @InjectMocks
    SourceReferenceTypeMappingValidationImpl validation;

    @Mock
    Result result;
    private SourceReferenceTypeMappings mapping;

    @Mock
    Messages message;
    @Mock
    MasterDataValidation masterDataValidation;

    @Mock
    ConfigurationFieldsValidation configurationFieldsValidation;


    @Mock
    List<SourceReferenceTypes> list;
    @Mock
    Message message1;

    @Mock
    protected PersistenceService mockDb;
    @Mock
    SourceReferenceTypeMappingService sourceReferenceTypeMappingService;
    @Mock
    Stream<Message> stream;
    @Mock
    SourceReferenceTypes type;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        mapping = Struct.create(SourceReferenceTypeMappings.class);

    }

    @Test
    public void validateSourceReferenceTypeMappingTest() {
        mapping.setSalesOrganizationId("11");

        mapping.setSourceSystem("test");
        mapping.setId("20");
        mapping.setComplaintTypeId("121H");
        list.add(type);
        when(list.size()).thenReturn(2);
        mapping.setReferenceTypes(list);

        when(message.error(MessageKeys.COMPLAINT_TYPE_IS_MANDATORY)).thenReturn(message1);
        when(message.error(MessageKeys.REFERENCE_TYPES_IS_MANDATORY)).thenReturn(message1);
        when(message.error(MessageKeys.SOURCE_SYSTEM_IS_MANDATORY)).thenReturn(message1);
        when(message.error(MessageKeys.ONLY_ONE_SOURCE_REFERENCE_TYPE_MAPPING_PER_ATTRIBUTES)).thenReturn(message1);
        when(sourceReferenceTypeMappingService.getSourceReferenceTypeMappingBasedOnValues(mapping)).thenReturn(result);
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

        validation.validateSourceReferenceTypeMapping(mapping);
    }


    @Test
    public void validateComplaintTypeNullTest() {
        mapping.setSalesOrganizationId("11");

        mapping.setSourceSystem("test");
        mapping.setId("201");

        list.add(type);
        when(list.size()).thenReturn(2);
        mapping.setReferenceTypes(list);

        when(message.error(MessageKeys.COMPLAINT_TYPE_IS_MANDATORY)).thenReturn(message1);
        when(message.error(MessageKeys.REFERENCE_TYPES_IS_MANDATORY)).thenReturn(message1);
        when(message.error(MessageKeys.SOURCE_SYSTEM_IS_MANDATORY)).thenReturn(message1);
        when(message.error(MessageKeys.ONLY_ONE_SOURCE_REFERENCE_TYPE_MAPPING_PER_ATTRIBUTES)).thenReturn(message1);
        when(sourceReferenceTypeMappingService.getSourceReferenceTypeMappingBasedOnValues(mapping)).thenReturn(result);
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

        validation.validateSourceReferenceTypeMapping(mapping);
    }

    @Test
    public void validateRefrenceListEmptyTest() {
        mapping.setSalesOrganizationId("11");

        mapping.setSourceSystem("test");
        mapping.setId("201");
        mapping.setComplaintTypeId("121H");
        when(list.isEmpty()).thenReturn(true);
        mapping.setReferenceTypes(list);

        when(message.error(MessageKeys.COMPLAINT_TYPE_IS_MANDATORY)).thenReturn(message1);
        when(message.error(MessageKeys.REFERENCE_TYPES_IS_MANDATORY)).thenReturn(message1);
        when(message.error(MessageKeys.SOURCE_SYSTEM_IS_MANDATORY)).thenReturn(message1);
        when(message.error(MessageKeys.ONLY_ONE_SOURCE_REFERENCE_TYPE_MAPPING_PER_ATTRIBUTES)).thenReturn(message1);
        when(sourceReferenceTypeMappingService.getSourceReferenceTypeMappingBasedOnValues(mapping)).thenReturn(result);
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

        validation.validateSourceReferenceTypeMapping(mapping);
    }

    @Test
    public void validateSourceSystemBlankTest() {
        mapping.setSalesOrganizationId("11");

        mapping.setSourceSystem("");
        mapping.setId("201");
        mapping.setComplaintTypeId("121H");
        when(list.size()).thenReturn(0);
        mapping.setReferenceTypes(list);

        when(message.error(MessageKeys.COMPLAINT_TYPE_IS_MANDATORY)).thenReturn(message1);
        when(message.error(MessageKeys.REFERENCE_TYPES_IS_MANDATORY)).thenReturn(message1);
        when(message.error(MessageKeys.SOURCE_SYSTEM_IS_MANDATORY)).thenReturn(message1);
        when(message.error(MessageKeys.ONLY_ONE_SOURCE_REFERENCE_TYPE_MAPPING_PER_ATTRIBUTES)).thenReturn(message1);
        when(sourceReferenceTypeMappingService.getSourceReferenceTypeMappingBasedOnValues(mapping)).thenReturn(result);
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

        validation.validateSourceReferenceTypeMapping(mapping);
    }

    @Test
    public void validateResultNotPresentTest() {
        mapping.setSalesOrganizationId("11");

        mapping.setSourceSystem("test");
        mapping.setId("201");
        mapping.setComplaintTypeId("121H");
        list.add(type);
        when(list.size()).thenReturn(2);
        mapping.setReferenceTypes(list);

        when(message.error(MessageKeys.COMPLAINT_TYPE_IS_MANDATORY)).thenReturn(message1);
        when(message.error(MessageKeys.REFERENCE_TYPES_IS_MANDATORY)).thenReturn(message1);
        when(message.error(MessageKeys.SOURCE_SYSTEM_IS_MANDATORY)).thenReturn(message1);
        when(message.error(MessageKeys.ONLY_ONE_SOURCE_REFERENCE_TYPE_MAPPING_PER_ATTRIBUTES)).thenReturn(message1);
        when(sourceReferenceTypeMappingService.getSourceReferenceTypeMappingBasedOnValues(mapping)).thenReturn(result);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        validation.validateSourceReferenceTypeMapping(mapping);
    }

    @Test
    public void validateSourceReferenceTypeMappingMismatchIDTest() {
        mapping.setSalesOrganizationId("11");

        mapping.setSourceSystem("test");
        mapping.setId("201");
        mapping.setComplaintTypeId("121H");
        list.add(type);
        when(list.size()).thenReturn(2);
        mapping.setReferenceTypes(list);

        when(message.error(MessageKeys.COMPLAINT_TYPE_IS_MANDATORY)).thenReturn(message1);
        when(message.error(MessageKeys.REFERENCE_TYPES_IS_MANDATORY)).thenReturn(message1);
        when(message.error(MessageKeys.SOURCE_SYSTEM_IS_MANDATORY)).thenReturn(message1);
        when(message.error(MessageKeys.ONLY_ONE_SOURCE_REFERENCE_TYPE_MAPPING_PER_ATTRIBUTES)).thenReturn(message1);
        when(sourceReferenceTypeMappingService.getSourceReferenceTypeMappingBasedOnValues(mapping)).thenReturn(result);
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

        validation.validateSourceReferenceTypeMapping(mapping);
    }


    @Test
    public void validateSourceReferenceTypeMappingFieldErrorAnyTest() {
        mapping.setSalesOrganizationId("11");
        mapping.setSourceSystem("test");
        mapping.setId("20");
        mapping.setComplaintTypeId("121H");
        list.add(type);
        when(list.size()).thenReturn(2);
        mapping.setReferenceTypes(list);
        when(message.stream()).thenReturn(stream);
        when(stream.noneMatch(any())).thenReturn(false);
        when(sourceReferenceTypeMappingService.getSourceReferenceTypeMappingBasedOnValues(mapping)).thenReturn(result);
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
        validation.validateSourceReferenceTypeMapping(mapping);
    }
}
