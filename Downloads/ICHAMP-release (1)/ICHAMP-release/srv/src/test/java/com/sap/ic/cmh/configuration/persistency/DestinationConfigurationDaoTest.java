package com.sap.ic.cmh.configuration.persistency;

import cds.gen.configurationservice.ConditionTypes_;
import cds.gen.configurationservice.DestinationConfigurations;
import cds.gen.configurationservice.DestinationConfigurations_;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnDelete;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.persistence.PersistenceService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class DestinationConfigurationDaoTest {
    @InjectMocks
    DestinationConfigurationDao dao;
    @Mock
    PersistenceService db;
    @Mock
    Result result;
    @Mock
    Runnable run;
    @Mock
    CdsCreateEventContext createContextMock;
    @Mock
    CqnInsert cqnInsert;
    DestinationConfigurations destinationConfigurations;

    private List<DestinationConfigurations_> boList = new ArrayList<>();
    Optional<Row> row;

    List<DestinationConfigurations> destinationConfigurationsList = new ArrayList<>();
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        destinationConfigurations =  Struct.create(DestinationConfigurations.class);
        destinationConfigurations.setIdentifier(10);
        destinationConfigurations.setDestination("100");
        destinationConfigurations.setBusinessObjectTypeCode("120");
        destinationConfigurations.setCompanyCodeId("1");
        destinationConfigurations.setDescription("2");
        destinationConfigurations.setId("100");

        destinationConfigurationsList.add(destinationConfigurations);
        when(db.run(any(CqnDelete.class))).thenReturn(result);
        when(createContextMock.getCqn()).thenReturn(cqnInsert);
        List<Map<String, Object>> entries = new ArrayList<>();
        when(cqnInsert.entries()).thenReturn(entries);
    }

    @Test
    public void getDestinationConfigurationTest(){
        dao.getDestinationConfiguration();
    }
    @Test
    public void getDestinationConfigBasedOnCompanyAndDestinationTest(){
        dao.getDestinationConfigBasedOnCompanyAndDestination(destinationConfigurations.getCompanyCodeId(),destinationConfigurations.getBusinessObjectTypeCode());
    }
    @Test
    public void getDestinationConfigBasedOnCompanyAndBOTypeTest(){
        dao.getDestinationConfigBasedOnCompanyAndBOType(destinationConfigurations.getCompanyCodeId(),destinationConfigurations.getDestination());
    }

    @Test
    public void testGetDestinationConfigBasedOnBOType(){
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first()).thenReturn(row);
        when(result.listOf(DestinationConfigurations.class)).thenReturn(destinationConfigurationsList);
        dao.getDestinationConfigBasedOnBOType(destinationConfigurations.getDestination());
    }
    @Test
    public void testGetDestinationConfigDetail() {
        Mockito.when(db.run(any(CqnSelect.class))).thenReturn(result);
        Mockito.when(result.listOf(DestinationConfigurations_.class)).thenReturn(boList);
        dao.getDestinationConfigDetail(destinationConfigurations.getId());
    }
}
