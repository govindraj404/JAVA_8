package com.sap.ic.cmh.configuration.handler;

import cds.gen.configurationservice.ComplaintCategories;
import cds.gen.configurationservice.SourceReferenceTypeMappings;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.service.sourcereferencetypemappings.SourceReferenceTypeMappingService;
import com.sap.ic.cmh.configuration.validations.sourcereferencetypemappings.SourceReferenceTypeMappingValidation;
import com.sap.ic.cmh.gen.MessageKeys;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.cds.services.auditlog.Action;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class SourceReferenceTypeMappingHandlerTest {
    @Mock
    SourceReferenceTypeMappingValidation sourceReferenceTypeMappingValidator;
    @Mock
    Messages messages;
    @Mock
    Message message1;
    @InjectMocks
    SourceReferenceTypeMappingHandler handler;
    @Mock
    protected PersistenceService mockDb;
    @Mock
    Result result;
    @Mock
    Action action;
    @Mock
    private AuditLogHelper<SourceReferenceTypeMappings> auditLogHelper;
    private Row row;
    private Optional<Row> opt;

    @Mock
    private SourceReferenceTypeMappings sourceReferenceTypeMapping;
    @Mock
    SourceReferenceTypeMappingService sourceReferenceTypeMappingService;
    @Mock
    Stream<SourceReferenceTypeMappings> sourceReferenceTypeMappings;
    @Mock
    Stream<SourceReferenceTypeMappings> sourceReferenceTypeMappingsStream;
    List<ComplaintCategories> list;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        sourceReferenceTypeMapping = Struct.create(SourceReferenceTypeMappings.class);
        list = new ArrayList<>();
        sourceReferenceTypeMapping.setComplaintTypeId("11");
        sourceReferenceTypeMapping.setSalesOrganizationId("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        sourceReferenceTypeMapping.setDistributionChannelId("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        sourceReferenceTypeMapping.setDivisionId("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        sourceReferenceTypeMapping.setItemCategoryId("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");

    }


    @Test
    public void beforeSourceReferenceTypeMappingCreateUpdateTest() {
        when(sourceReferenceTypeMappingService.getSourceReferenceTypeMappings()).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
        row.put("ID", "201");
        row.put("identifier", "1");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(1L);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(message1.target(any(String.class))).thenReturn(message1);
        when(messages.error(MessageKeys.ONLY_ONE_SOURCE_REFERENCE_TYPE_MAPPING_PER_ATTRIBUTES)).thenReturn(message1);
        handler.beforeSourceReferenceTypeMappingCreateUpdate(sourceReferenceTypeMapping);
    }



    @Test
    public void onCreateSourceReferenceTypeMappingTest() {
        when(sourceReferenceTypeMappingService.getSourceReferenceTypeMappings()).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
        row.put("ID", "201");
        row.put("identifier", "1");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(1L);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        handler.onCreateSourceReferenceTypeMapping(sourceReferenceTypeMapping);
    }

    @Test
    public void onCreateSourceReferenceTypeMappingIdentifierNullTest() {
        when(sourceReferenceTypeMappingService.getSourceReferenceTypeMappings()).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
        row.put("ID", "201");
        row.put("identifier", null);
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(1L);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        handler.onCreateSourceReferenceTypeMapping(sourceReferenceTypeMapping);
    }

    @Test
    public void onCreateSourceReferenceTypeMappingEmptyTest() {
        when(sourceReferenceTypeMappingService.getSourceReferenceTypeMappings()).thenReturn(result);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(1L);
        handler.onCreateSourceReferenceTypeMapping(sourceReferenceTypeMapping);
    }

    @Test
    public void afterReadReferenceTypesTest() {
        sourceReferenceTypeMappingsStream = Stream.of(sourceReferenceTypeMapping);
        handler.afterReadReferenceTypes(sourceReferenceTypeMappingsStream);
    }

    @Test
    public void afterReadReferenceTypesNullComplaintTest() {
        sourceReferenceTypeMapping.setComplaintTypeId(null);
        sourceReferenceTypeMapping.setItemCategoryId("sourceReferenceTypeMapping");
        sourceReferenceTypeMappingsStream = Stream.of(sourceReferenceTypeMapping);
        handler.afterReadReferenceTypes(sourceReferenceTypeMappingsStream);
    }

    @Test
    public void afterReadReferenceTypesNullComplaintNullItemTest() {
        sourceReferenceTypeMapping.setComplaintTypeId(null);
        sourceReferenceTypeMapping.setItemCategoryId(null);
        sourceReferenceTypeMappingsStream = Stream.of(sourceReferenceTypeMapping);
        handler.afterReadReferenceTypes(sourceReferenceTypeMappingsStream);
    }

    @Test
    public void afterReadReferenceTypesNullItemTest() {
        sourceReferenceTypeMapping.setComplaintTypeId("11");
        sourceReferenceTypeMapping.setItemCategoryId(null);
        sourceReferenceTypeMappingsStream = Stream.of(sourceReferenceTypeMapping);
        handler.afterReadReferenceTypes(sourceReferenceTypeMappingsStream);
    }

    @Test
    public void beforeSourceReferenceTypeMappingsPatchTest() {
        sourceReferenceTypeMapping.setComplaintTypeId("11");
        handler.beforeSourceReferenceTypeMappingsPatch(sourceReferenceTypeMapping);
    }

    @Test
    public void beforeSourceReferenceTypeMappingsPatchNullTest() {
        sourceReferenceTypeMapping.setComplaintTypeId("11");
        sourceReferenceTypeMapping.setItemCategoryId(null);
        handler.beforeSourceReferenceTypeMappingsPatch(sourceReferenceTypeMapping);
    }

    @Test
    public void beforeSourceReferenceTypeMappingsPatchNullComplaintTest() {
        sourceReferenceTypeMapping.setComplaintTypeId(null);
        handler.beforeSourceReferenceTypeMappingsPatch(sourceReferenceTypeMapping);
    }
    @Test
    public void testAfterReadSourceReferenceTypeMappingsNulltest(){
        List<SourceReferenceTypeMappings> list=new ArrayList<>();
        list.add(sourceReferenceTypeMapping);
        handler.afterReadSourceReferenceTypeMapping(list.stream());

    }

    @Test
    public void afterSourceReferenceTypeMappingsReadTest()
    {
        List<SourceReferenceTypeMappings> list=new ArrayList<>();
        sourceReferenceTypeMapping.setIsActiveEntity(true);
        sourceReferenceTypeMapping.setHasDraftEntity(true);
        sourceReferenceTypeMapping.setIsActive(true);
        list.add(sourceReferenceTypeMapping);

        handler.afterReadSourceReferenceTypeMapping(list.stream());
    }
    @Test
    public void afterSourceReferenceTypeMappingsReadNullIsActiveTest()
    {
        List<SourceReferenceTypeMappings> list=new ArrayList<>();
        sourceReferenceTypeMapping.setIsActiveEntity(true);
        sourceReferenceTypeMapping.setHasDraftEntity(true);

        list.add(sourceReferenceTypeMapping);

        handler.afterReadSourceReferenceTypeMapping(list.stream());
    }
    @Test
    public void afterSourceReferenceTypeMappingsReadNullHasActiveTest()
    {
        List<SourceReferenceTypeMappings> list=new ArrayList<>();
        sourceReferenceTypeMapping.setIsActiveEntity(true);
        sourceReferenceTypeMapping.setIsActive(true);
        list.add(sourceReferenceTypeMapping);
        handler.afterReadSourceReferenceTypeMapping(list.stream());
    }
    @Test
    public void afterSourceReferenceTypeMappingsReadFalseTest()
    {
        List<SourceReferenceTypeMappings> list=new ArrayList<>();
        sourceReferenceTypeMapping.setIsActiveEntity(true);
        sourceReferenceTypeMapping.setHasDraftEntity(false);
        sourceReferenceTypeMapping.setIsActive(true);
        list.add(sourceReferenceTypeMapping);

        handler.afterReadSourceReferenceTypeMapping(list.stream());
    }
    @Test
    public void afterSourceReferenceTypeMappingsReadFalseTest1()
    {
        List<SourceReferenceTypeMappings> list=new ArrayList<>();
        sourceReferenceTypeMapping.setIsActiveEntity(false);
        sourceReferenceTypeMapping.setHasDraftEntity(true);
        sourceReferenceTypeMapping.setIsActive(true);
        list.add(sourceReferenceTypeMapping);
        handler.afterReadSourceReferenceTypeMapping(list.stream());
    }
    @Test
    public void afterSourceReferenceTypeMappingsReadTrueTest()
    {
        List<SourceReferenceTypeMappings> list=new ArrayList<>();
        sourceReferenceTypeMapping.setIsActiveEntity(true);
        sourceReferenceTypeMapping.setHasDraftEntity(false);
        sourceReferenceTypeMapping.setIsActive(false);
        list.add(sourceReferenceTypeMapping);
        handler.afterReadSourceReferenceTypeMapping(list.stream());
    }
    @Test
    public void afterUpdateSourceReferenceTypeMappingTest() {
        handler.afterUpdateSourceReferenceTypeMapping(sourceReferenceTypeMapping);
    }
    @Test
    public void afterCreateSourceReferenceTypeMappingTest() {
        handler.afterCreateSourceReferenceTypeMapping(sourceReferenceTypeMapping);
    }
    @Test
    public void logUpsertTest()
    {
        handler.logUpsert(Action.UPDATE,sourceReferenceTypeMapping);
    }
    @Test
    public void beforeSourceReferenceTypeMappingUpdateTest()
    {
        handler.beforeSourceReferenceTypeMappingUpdate(sourceReferenceTypeMapping);
    }

}