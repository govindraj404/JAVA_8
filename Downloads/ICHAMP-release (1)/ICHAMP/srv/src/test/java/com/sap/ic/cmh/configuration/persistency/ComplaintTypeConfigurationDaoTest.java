package com.sap.ic.cmh.configuration.persistency;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import com.sap.cds.Result;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;

public class ComplaintTypeConfigurationDaoTest {

    @InjectMocks
    @Autowired
    ComplaintTypeConfigurationDao complaintTypeConfigurationDao;

    @Mock
    Result result;

    @Mock
    PersistenceService mockDb;


    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void testGetComplaintTypeConfigurationBasedOnCode() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        complaintTypeConfigurationDao.getComplaintTypeConfigurationBasedOnCode("123");
    }
    @Test
    public void testGetComplaintTypeConfiguration() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        complaintTypeConfigurationDao.getComplaintTypeConfiguration();
    }
    @Test
    public void getAllComplaintTypesDetailsTest() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        complaintTypeConfigurationDao.getAllComplaintTypesDetails("123");
    }

    @Test
    public void getComplaintTypeDetailsBasedOnIdTest() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        complaintTypeConfigurationDao.getComplaintTypeDetailsBasedOnId("123");
    }
}
