package com.sap.ic.cmh.configuration.persistency;

import cds.gen.configurationservice.SourceReferenceTypeMappings;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class SourceReferenceTypeMappingDaoTest {
    @InjectMocks
    SourceReferenceTypeMappingDao dao;
    @Mock
    PersistenceService mockDb;
    @Mock
    Result result;
    private SourceReferenceTypeMappings mapping;

    @Before
    public void setBefore() {
        MockitoAnnotations.openMocks(this);
        mapping = Struct.create(SourceReferenceTypeMappings.class);
    }

    @Test
    public void getSourceReferenceTypeMappingBasedOnValuesTest() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        dao.getSourceReferenceTypeMappingBasedOnValues(mapping);

    }

    @Test
    public void getSourceReferenceTypeMappingsTest() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        dao.getSourceReferenceTypeMappings();

    }
    @Test
    public void getTargetReferenceTypeMappingDetailsBasedOnIDTest()
    {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        dao.getSourceReferenceTypeMappingBasedOnId("1");
    }
}
