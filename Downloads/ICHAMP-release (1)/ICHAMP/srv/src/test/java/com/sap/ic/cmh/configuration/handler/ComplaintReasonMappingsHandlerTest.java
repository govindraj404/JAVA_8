package com.sap.ic.cmh.configuration.handler;

import cds.gen.configurationservice.ComplaintReasonMappings;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.service.ComplaintReasonMappingsService;
import com.sap.ic.cmh.configuration.validations.ComplaintReasonMappingsValidation;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import com.sap.cds.services.auditlog.Action;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import com.sap.cds.services.cds.CdsReadEventContext;
import java.util.stream.Stream;
public class ComplaintReasonMappingsHandlerTest {
    @InjectMocks
    ComplaintReasonMappingsHandler handler;


    @Mock
    ComplaintReasonMappingsValidation validation;

    @Mock
    ComplaintReasonMappingsService complaintReasonMappingsService;


    @Mock
    Messages messages;
    @Mock
    Message message1;
    @Mock
    protected PersistenceService mockDb;
    @Mock
    Result result;

    private ComplaintReasonMappings salesAreaItemCategoryReasonMaps;
    @Mock
    CdsReadEventContext context;

    @Mock
    Stream<ComplaintReasonMappings> ComplaintReasonsMapStream;
    @Mock
    private AuditLogHelper<ComplaintReasonMappings> auditLogHelper;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        salesAreaItemCategoryReasonMaps = Struct.create(ComplaintReasonMappings.class);
    }

    @Test
    public void beforeCreateSalesItemReasonTest() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(1L);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
        handler.beforeCreateComplaintReasonMap(salesAreaItemCategoryReasonMaps);
    }

    @Test
    public void beforeCreateSalesItemReasonUpdateTest() {
        Row row = Struct.create(Row.class);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(1L);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
        handler.beforeUpdateComplaintReasonMap(salesAreaItemCategoryReasonMaps);
    }

    @Test
    public void onCreateComplaintItemReasonTest() {
        when(complaintReasonMappingsService.getComplaintReasonMapIdentifier()).thenReturn(result);
        Row row = Struct.create(Row.class);
        Optional<Row> opt = Optional.of(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first()).thenReturn(opt);
        handler.onCreateComplaintReasonMap(salesAreaItemCategoryReasonMaps);
    }

    @Test
    public void onCreateComplaintItemReasonTestWithIdentifierValue() {
        when(complaintReasonMappingsService.getComplaintReasonMapIdentifier()).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
        row.put("identifier", "11");
        Optional<Row> opt = Optional.of(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first()).thenReturn(opt);
        handler.onCreateComplaintReasonMap(salesAreaItemCategoryReasonMaps);
    }

    @Test
    public void onCreateComplaintItemReasonWithEmptyDataTest() {
        when(complaintReasonMappingsService.getComplaintReasonMapIdentifier()).thenReturn(result);
        Optional<Row> opt;
        opt = Optional.empty();
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first()).thenReturn(opt);
        handler.onCreateComplaintReasonMap(salesAreaItemCategoryReasonMaps);
    }
    @Test
    public void afterComplaintReasonMappingNullTest()
    {
        List<ComplaintReasonMappings> list=new ArrayList<>();
        list.add(salesAreaItemCategoryReasonMaps);

        handler.afterComplaintReasonMapping(context,list.stream());
    }

    @Test
    public void afterComplaintReasonMappingTest()
    {
        List<ComplaintReasonMappings> list=new ArrayList<>();
        salesAreaItemCategoryReasonMaps.setIsActiveEntity(true);
        salesAreaItemCategoryReasonMaps.setHasDraftEntity(true);
        salesAreaItemCategoryReasonMaps.setIsActive(true);
        list.add(salesAreaItemCategoryReasonMaps);
        handler.afterComplaintReasonMapping(context,list.stream());
    }
    @Test
    public void afterComplaintReasonMappingNullIsActiveTest()
    {
        List<ComplaintReasonMappings> list=new ArrayList<>();
        salesAreaItemCategoryReasonMaps.setIsActiveEntity(true);
        salesAreaItemCategoryReasonMaps.setHasDraftEntity(true);

        list.add(salesAreaItemCategoryReasonMaps);

        handler.afterComplaintReasonMapping(context,list.stream());
    }
    @Test
    public void afterComplaintReasonMappingNullHasActiveTest()
    {
        List<ComplaintReasonMappings> list=new ArrayList<>();
        salesAreaItemCategoryReasonMaps.setIsActiveEntity(true);

        salesAreaItemCategoryReasonMaps.setIsActive(true);
        list.add(salesAreaItemCategoryReasonMaps);

        handler.afterComplaintReasonMapping(context,list.stream());
    }
    @Test
    public void afterComplaintReasonMappingFalseTest()
    {
        List<ComplaintReasonMappings> list=new ArrayList<>();
        salesAreaItemCategoryReasonMaps.setIsActiveEntity(true);
        salesAreaItemCategoryReasonMaps.setHasDraftEntity(false);
        salesAreaItemCategoryReasonMaps.setIsActive(true);
        list.add(salesAreaItemCategoryReasonMaps);

        handler.afterComplaintReasonMapping(context,list.stream());
    }
    @Test
    public void afterComplaintReasonMappingFalseTest1()
    {
        List<ComplaintReasonMappings> list=new ArrayList<>();
        salesAreaItemCategoryReasonMaps.setIsActiveEntity(false);
        salesAreaItemCategoryReasonMaps.setHasDraftEntity(true);
        salesAreaItemCategoryReasonMaps.setIsActive(true);
        list.add(salesAreaItemCategoryReasonMaps);

        handler.afterComplaintReasonMapping(context,list.stream());
    }
    @Test
    public void afterComplaintReasonMappingTrueTest()
    {
        List<ComplaintReasonMappings> list=new ArrayList<>();
        salesAreaItemCategoryReasonMaps.setIsActiveEntity(true);
        salesAreaItemCategoryReasonMaps.setHasDraftEntity(false);
        salesAreaItemCategoryReasonMaps.setIsActive(false);
        list.add(salesAreaItemCategoryReasonMaps);

        handler.afterComplaintReasonMapping(context,list.stream());
    }
    @Test
    public void afterCreateComplaintReasonMapTest()
    {
        handler.afterCreateComplaintReasonMap(salesAreaItemCategoryReasonMaps);
    }
    @Test
    public void afterUpdateComplaintReasonMapTest()
    {
        handler.afterUpdateComplaintReasonMap(salesAreaItemCategoryReasonMaps);
    }
    @Test
    public void logUpsertTest()
    {
        handler.logUpsert(Action.UPDATE,salesAreaItemCategoryReasonMaps);
    }

}
