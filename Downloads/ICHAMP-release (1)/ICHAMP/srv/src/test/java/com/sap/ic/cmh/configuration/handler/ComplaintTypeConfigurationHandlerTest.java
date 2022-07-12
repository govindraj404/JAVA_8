package com.sap.ic.cmh.configuration.handler;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.auditlog.Action;
import com.sap.cds.services.persistence.PersistenceService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import com.sap.ic.cmh.auditlog.AuditLogHelper;

import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.configuration.persistency.ComplaintTypeToSalesAreaMappingDao;
import com.sap.ic.cmh.configuration.service.ComplaintTypeConfigurationService;
import com.sap.ic.cmh.configuration.validations.ComplaintTypeConfigurationValidation;

import cds.gen.configurationservice.ComplaintTypeConfigurations;
import cds.gen.configurationservice.ComplaintTypeToSalesAreaMappings;

public class ComplaintTypeConfigurationHandlerTest {

    @InjectMocks
    ComplaintTypeConfigurationHandler handler;

    @Mock
    ComplaintTypeConfigurationService service;

    @Mock
    ComplaintTypeConfigurationValidation complaintTypeValidation;

    @Mock
    Messages Messages;

    @Mock
    Result result;

//    @Mock
//    AuditLogHelper auditLogHelper;

    @Mock
    ComplaintTypeToSalesAreaMappingDao ComplaintTypeToSalesAreaMappingDao;
    @Mock
    protected PersistenceService mockDb;

    private ComplaintTypeConfigurations complaintType;
    private ComplaintTypeConfigurations complaintType1;
    private ComplaintTypeConfigurations complaintType2;
    private ComplaintTypeToSalesAreaMappings destinationMapping;
    List<ComplaintTypeToSalesAreaMappings> destinationMappingList = new ArrayList<>();
    List<ComplaintTypeConfigurations> ComplaintTypeConfigurationsList = new ArrayList<>();
    private Row row;
    private Optional<Row> opt;

    @Mock
    private AuditLogHelper<ComplaintTypeConfigurations> auditLogHelper;
    @Before
    public void setBefore() {
        MockitoAnnotations.openMocks(this);
        complaintType = Struct.create(ComplaintTypeConfigurations.class);
        complaintType1 = Struct.create(ComplaintTypeConfigurations.class);
        complaintType2 = Struct.create(ComplaintTypeConfigurations.class);
        destinationMapping = Struct.create(ComplaintTypeToSalesAreaMappings.class);
        destinationMapping.setId("789");
        destinationMapping.setSalesOrganizationId("12234");
        destinationMapping.setDistributionChannelId("456");
        destinationMapping.setDivisionId("123");
        destinationMappingList.add(destinationMapping);
        complaintType.setId("id");
        complaintType.setCode("CODE100");
        complaintType.setComplaintTypeToSalesAreaMappings(destinationMappingList);
        complaintType1.setCode("CODE10");
        complaintType1.setId("id");
        complaintType1.setIndividualComplaintType(false);
        ComplaintTypeConfigurationsList.add(complaintType);
        ComplaintTypeConfigurationsList.add(complaintType1);
        row = Struct.create(Row.class);
        row.put("code", "CODE100");
        row.put("identifier", "11");
        opt = Optional.of(row);

    }

    @Test
    public void testBeforeCreateComplaintTypeConfig() {
        handler.beforeCreateComplaintTypeConfiguration(complaintType);
    }

    @Test
    public void testOnCreateComplaintTypeConfig() {
        when(service.getComplaintTypeConfiguration()).thenReturn(result);
        when(result.first()).thenReturn(opt);
        handler.onCreateComplaintTypeConfiguration(complaintType);
    }

    @Test
    public void testOnCreateComplaintTypeConfigEmptyObject() {
        Optional<Row> emptyOpt = Optional.empty();
        when(service.getComplaintTypeConfiguration()).thenReturn(result);
        when(result.first()).thenReturn(emptyOpt);
        handler.onCreateComplaintTypeConfiguration(complaintType);
    }

    @Test
    public void testonCreateComplaintTypeConfigurationElse1Test() {
        when(service.getComplaintTypeConfiguration()).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        handler.onCreateComplaintTypeConfiguration(complaintType);
    }

    @Test
    public void testBeforeUpdateComplaintTypeWhenCodeIsSame() {
        when(service.getComplaintTypeConfiguration()).thenReturn(result);
        when(ComplaintTypeToSalesAreaMappingDao.getSalesAreaMappingBasedOnComplaintTypeConfig(complaintType.getId())).thenReturn(result);
        when(result.first()).thenReturn(opt);
        when(result.listOf(ComplaintTypeConfigurations.class)).thenReturn(ComplaintTypeConfigurationsList);
        handler.beforeUpdateComplaintTypeConfiguration(complaintType);
    }

    @Test
    public void testAfterComplaintTypeConfigurationsPatch() {
        when(result.single(ComplaintTypeToSalesAreaMappings.class)).thenReturn(destinationMapping);
        when(result.single(ComplaintTypeConfigurations.class)).thenReturn(complaintType);
        when(service.getComplaintTypeConfiguration()).thenReturn(result);
        when(ComplaintTypeToSalesAreaMappingDao.getSalesAreaMappingBasedOnComplaintTypeConfig("id")).thenReturn(result);
        complaintType.setIndividualComplaintType(true);
        ComplaintTypeConfigurationsList.add(complaintType);
        handler.afterReadComplaintTypeConfiguration(ComplaintTypeConfigurationsList.stream());
    }

    @Test
    public void testAfterComplaintTypeConfigurationsPatchElse() {
        when(result.single(ComplaintTypeToSalesAreaMappings.class)).thenReturn(destinationMapping);
        when(result.single(ComplaintTypeConfigurations.class)).thenReturn(complaintType);
        when(service.getComplaintTypeConfiguration()).thenReturn(result);
        when(ComplaintTypeToSalesAreaMappingDao.getSalesAreaMappingBasedOnComplaintTypeConfig("id")).thenReturn(result);
        ComplaintTypeConfigurationsList.add(complaintType2);
        handler.afterReadComplaintTypeConfiguration(ComplaintTypeConfigurationsList.stream());
    }

    @Test
    public void testBeforeComplaintTypeConfigurationsPatch() {
        complaintType1.setIndividualComplaintType(true);
        handler.beforeComplaintTypeConfigurationsPatch(complaintType1);

    }

    @Test
    public void testBeforeComplaintTypeConfigurationsPatchElse() {
        complaintType1.setIndividualComplaintType(false);
        handler.beforeComplaintTypeConfigurationsPatch(complaintType1);

    }

    @Test
    public void testBeforeComplaintTypeConfigurationsEmptyList() {
        complaintType1.setIndividualComplaintType(false);
        handler.beforeComplaintTypeConfigurationsPatch(complaintType2);

    }
    @Test
    public void testfterReadComplaintTypeConfigurationsNulltest(){

        handler.afterReadComplaintTypeConfigurationsActivity(ComplaintTypeConfigurationsList.stream());

    }

    @Test
    public void afterComplaintTypeConfigurationsReadTest()
    {
        List<ComplaintTypeConfigurations> list=new ArrayList<>();
        complaintType.setIsActiveEntity(true);
        complaintType.setHasDraftEntity(true);
        complaintType.setIsActive(true);
        list.add(complaintType);

        handler.afterReadComplaintTypeConfigurationsActivity(list.stream());
    }
    @Test
    public void afterComplaintTypeConfigurationsReadNullIsActiveTest()
    {
        List<ComplaintTypeConfigurations> list=new ArrayList<>();
        complaintType.setIsActiveEntity(true);
        complaintType.setHasDraftEntity(true);

        list.add(complaintType);

        handler.afterReadComplaintTypeConfigurationsActivity(list.stream());
    }
    @Test
    public void afterComplaintTypeConfigurationsReadNullHasActiveTest()
    {
        List<ComplaintTypeConfigurations> list=new ArrayList<>();
        complaintType.setIsActiveEntity(true);
        complaintType.setIsActive(true);
        list.add(complaintType);
        handler.afterReadComplaintTypeConfigurationsActivity(list.stream());
    }
    @Test
    public void afterComplaintTypeConfigurationsReadFalseTest()
    {
        List<ComplaintTypeConfigurations> list=new ArrayList<>();
        complaintType.setIsActiveEntity(true);
        complaintType.setHasDraftEntity(false);
        complaintType.setIsActive(true);
        list.add(complaintType);

        handler.afterReadComplaintTypeConfigurationsActivity(list.stream());
    }
    @Test
    public void afterComplaintTypeConfigurationsReadFalseTest1()
    {
        List<ComplaintTypeConfigurations> list=new ArrayList<>();
        complaintType.setIsActiveEntity(false);
        complaintType.setHasDraftEntity(true);
        complaintType.setIsActive(true);
        list.add(complaintType);
        handler.afterReadComplaintTypeConfigurationsActivity(list.stream());
    }
    @Test
    public void afterComplaintTypeConfigurationsReadTrueTest()
    {
        List<ComplaintTypeConfigurations> list=new ArrayList<>();
        complaintType.setIsActiveEntity(true);
        complaintType.setHasDraftEntity(false);
        complaintType.setIsActive(false);
        list.add(complaintType);
        handler.afterReadComplaintTypeConfigurationsActivity(list.stream());
    }
    @Test
    public void afterUpdateComplaintTypeTest()
    {

        handler.afterUpdateComplaintType(complaintType);
    }
    @Test
    public void afterComplaintTypeCreationTest()
    {

        handler.afterComplaintTypeCreation(complaintType);
    }
    @Test
    public void logUpsertTest()
    {
        handler.logUpsert(Action.UPDATE,complaintType);
    }
}

