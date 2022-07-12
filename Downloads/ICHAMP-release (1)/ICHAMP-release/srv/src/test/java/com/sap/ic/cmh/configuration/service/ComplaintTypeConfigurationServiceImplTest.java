
package com.sap.ic.cmh.configuration.service;

import cds.gen.configurationservice.ComplaintTypeConfigurations;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.persistency.ComplaintTypeConfigurationDao;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

public class ComplaintTypeConfigurationServiceImplTest {

    @InjectMocks
    ComplaintTypeConfigurationServiceImpl complaintTypeServiceImpl;

    @Mock
    ComplaintTypeConfigurationDao complaintTypeDao;

    @Mock
    Result result;

    @Mock
    public Row row;

    @Mock
    protected PersistenceService mockDb;
    protected ComplaintTypeConfigurations complaintTypeConfigurations;

    @Before
    public void setBefore() {
        MockitoAnnotations.openMocks(this);
        complaintTypeConfigurations = Struct.create(ComplaintTypeConfigurations.class);
    }

    @Test
    public void testGetComplaintTypes() {
        when(complaintTypeDao.getComplaintTypeConfiguration()).thenReturn(result);
        complaintTypeServiceImpl.getComplaintTypeConfiguration();
    }

    @Test
    public void testGetComplaintTypeCodeBasedOnCode() {
        when(complaintTypeDao.getComplaintTypeConfigurationBasedOnCode(any(String.class))).thenReturn(result);
        complaintTypeServiceImpl.getComplaintTypeConfiguration();
    }

    @Test
    public void getAllComplaintTypesDetailsTest() {
        complaintTypeConfigurations = Struct.create(ComplaintTypeConfigurations.class);
        complaintTypeConfigurations.setCode("111");
        when(complaintTypeDao.getAllComplaintTypesDetails(any(String.class))).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
        row.put("ID", "201");
        row.put("identifier", "2");
        row.put("code", "reasoncode");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(2L);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        List<ComplaintTypeConfigurations> list = new ArrayList<>();
        list.add(complaintTypeConfigurations);
        when(result.listOf(ComplaintTypeConfigurations.class)).thenReturn(list);
        complaintTypeServiceImpl.getAllComplaintTypesDetails("1");
    }

    @Test
    public void getAllComplaintTypesDetailsEmptyTest() {
        when(complaintTypeDao.getAllComplaintTypesDetails(any(String.class))).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(0L);
        when(result.list()).thenReturn(rowValues);
        List<ComplaintTypeConfigurations> list = new ArrayList<>();
        when(result.listOf(ComplaintTypeConfigurations.class)).thenReturn(list);
        complaintTypeServiceImpl.getAllComplaintTypesDetails("1");
    }

    @Test
    public void getActiveTest() {
        when(complaintTypeDao.getAllComplaintTypesDetails(anyString())).thenReturn(result);
        row.put("id", "ID");
        Optional<Row> op = Optional.of(row);
        when(result.first()).thenReturn(op);
        ComplaintTypeConfigurations complaintTypeConfig = Struct.create(ComplaintTypeConfigurations.class);
        complaintTypeConfig.setId("ID");
        complaintTypeConfig.setIsActive(true);
        List<ComplaintTypeConfigurations> complaintTypeConfiguration = new ArrayList<>();
        complaintTypeConfiguration.add(complaintTypeConfig);
        when(result.listOf(ComplaintTypeConfigurations.class)).thenReturn(complaintTypeConfiguration);
        complaintTypeServiceImpl.getActive("1e93d863-5b9d-4aae-9a96-375aab27270b");
    }

    @Test
    public void getActivenullTest() {
        when(complaintTypeDao.getAllComplaintTypesDetails(anyString())).thenReturn(result);
        complaintTypeServiceImpl.getActive("1e93d863-5b9d-4aae-9a96-375aab27270b");
    }
}


