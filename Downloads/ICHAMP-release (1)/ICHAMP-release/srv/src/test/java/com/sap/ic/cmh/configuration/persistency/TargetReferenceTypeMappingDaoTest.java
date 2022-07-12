package com.sap.ic.cmh.configuration.persistency;

import cds.gen.configurationservice.TargetReferenceTypeMappings;
import com.sap.cds.Result;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;


public class TargetReferenceTypeMappingDaoTest {


    @InjectMocks
    TargetReferenceTypeMappingsDao dao;
    @Mock
    PersistenceService mockDb;
    @Mock
    Result result;
    @Mock
    TargetReferenceTypeMappings maps;
    @Before
    public void BeforeSet() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void testGetTargetReferenceTypeMappings() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        dao.getTargetReferenceTypeMappings();
    }

    @Test
    public void testGetTargetReferenceTypeMappingBasedOnValues() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        dao.getTargetReferenceTypeMappingBasedOnValues(maps);
    }
    @Test
    public void getTargetReferenceTypeMappingDetailsBasedOnIDTest()
    {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        dao.getTargetReferenceTypeMappingDetailsBasedOnID("1");
    }



}