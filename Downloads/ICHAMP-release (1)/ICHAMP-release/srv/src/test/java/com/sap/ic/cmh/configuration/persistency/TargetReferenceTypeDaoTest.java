package com.sap.ic.cmh.configuration.persistency;

import com.sap.cds.services.persistence.PersistenceService;
import org.junit.Before;

import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.mockito.ArgumentMatchers.any;

public class TargetReferenceTypeDaoTest {

    @InjectMocks
    TargetReferenceTypeDao dao;

    @Mock
    PersistenceService db;

    @Before
    public void beforeClass()
    {
        MockitoAnnotations.openMocks(this);

    }

    @Test
    public void getTargetReferenceTypesTest()
    {
        dao.getTargetReferenceTypes();

    }
    @Test
    public void getTargetReferenceTypesBasedOnTargetMappingTest()
    {
        dao.getTargetReferenceTypesBasedOnTargetMapping(any(String.class));

    }



}
