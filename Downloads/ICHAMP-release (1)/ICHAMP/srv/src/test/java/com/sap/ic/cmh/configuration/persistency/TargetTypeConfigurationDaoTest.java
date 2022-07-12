package com.sap.ic.cmh.configuration.persistency;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import cds.gen.configurationservice.TargetTypes_;
import com.sap.cds.Struct;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.sap.cds.Result;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;

public class TargetTypeConfigurationDaoTest {

    @InjectMocks
    TargetTypeConfigurationDao dao;

    @Mock
    PersistenceService mockDb;

    Result result;

    @Before
    public void setBefore() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void testGetTargetTypeConfigurations() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        dao.getTargetTypeConfigurations();
    }
    @Test
    public void testGetTargetTypeConfigurationCodeBasedOnId() {

        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        dao.getTargetTypeConfigurationCodeBasedOnId(any(String.class));
    }
    @Test
    public void testGetTargetTypeConfigurationCodeBasedOnId1() {

        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        dao.getTargetTypeConfigurationCodeBasedOnId1(any(String.class));
    }


    @Test
    public void testGetTargetTypeConfigurationIdBasedOnCode() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        dao.getTargetTypeConfigurationIdBasedOnCode(any(String.class));
    }
}