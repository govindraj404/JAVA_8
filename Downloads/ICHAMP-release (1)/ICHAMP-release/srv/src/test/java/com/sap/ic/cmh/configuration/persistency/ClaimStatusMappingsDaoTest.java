package com.sap.ic.cmh.configuration.persistency;

import cds.gen.configurationservice.BusinessObjectConfigurations;
import cds.gen.configurationservice.BusinessObjectConfigurations_;
import cds.gen.configurationservice.ClaimStatusMappings;
import cds.gen.configurationservice.ClaimStatusMappings_;
import cds.gen.masterdataservice.Addresses;
import com.sap.cds.Result;
import com.sap.cds.Struct;
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
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class ClaimStatusMappingsDaoTest {
    @InjectMocks
    ClaimStatusMappingsDao dao;
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
    ClaimStatusMappings claimStatusMappings;

    private List<ClaimStatusMappings_> boList = new ArrayList<>();

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        claimStatusMappings = Struct.create(ClaimStatusMappings.class);
        claimStatusMappings.setStatusCode("200");
        claimStatusMappings.setCode("201");
    }
    @Test
    public void getBusinessObjectConfigurationsTest(){
        Optional<ClaimStatusMappings> opt = Optional.of(claimStatusMappings);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(ClaimStatusMappings.class)).thenReturn(opt);
        dao.getClaimStatusMappings(claimStatusMappings.getCode());
    }
    
    @Test
    public void getClaimStatusMappingsBasedOnIdentifierTest(){
        dao.getClaimStatusMappingsBasedOnIdentifier();
    }
    @Test
    public void testGetClaimStatusMappingDetails() {
        Mockito.when(db.run(any(CqnSelect.class))).thenReturn(result);
        Mockito.when(result.listOf(ClaimStatusMappings_.class)).thenReturn(boList);
        dao.getClaimStatusMappingDetails(claimStatusMappings.getId());
    }
}
