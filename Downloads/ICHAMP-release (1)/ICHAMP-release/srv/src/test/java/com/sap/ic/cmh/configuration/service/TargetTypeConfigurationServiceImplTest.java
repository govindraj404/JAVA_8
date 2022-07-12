package com.sap.ic.cmh.configuration.service;

import cds.gen.configurationservice.TargetTypes;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import com.sap.ic.cmh.configuration.persistency.TargetTypeConfigurationDao;


public class TargetTypeConfigurationServiceImplTest {

    @InjectMocks
    TargetTypeConfigurationServiceImpl service;

    @Mock
    TargetTypeConfigurationDao targetTypeDao;

    @Mock
    Result result;

    @Mock
    TargetTypes targetTypes;
    private Row row;
    @Before
    public void setBefore() {
        row = Struct.create(Row.class);
        row.put("code", "CODE100");
        row.put("identifier", "11");
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void testGetTargetTypeConfigurations() {
        when(targetTypeDao.getTargetTypeConfigurations()).thenReturn(result);
        service.getTargetTypeConfigurations();
    }
    @Test
    public void getTargetTypeConfigurationCodeBasedOnIdTest() {
        when(targetTypeDao.getTargetTypeConfigurationCodeBasedOnId(any(String.class))).thenReturn(result);
        service.getTargetTypeConfigurationCodeBasedOnId(any(String.class));
    }
    @Test
    public void getTargetTypeConfigurationIdBasedOnCodeTest() {
        when(targetTypeDao.getTargetTypeConfigurationIdBasedOnCode(any(String.class))).thenReturn(result);
        service.getTargetTypeConfigurationIdBasedOnCode(any(String.class));
    }
    @Test
    public void getTargetTypeConfigurationsBasedOnID1Test()
    {
        when(targetTypeDao.getTargetTypeConfigurationCodeBasedOnId1(any(String.class))).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "201");
        row.put("identifier", "2");
        row.put("code", "reasoncode");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(result.first()).thenReturn(opt);
        List<TargetTypes> list=new ArrayList<>();
        list.add(targetTypes);
        when(result.listOf(TargetTypes.class)).thenReturn(list);
        service.getTargetTypesDetails("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
    }
    @Test
    public void testGetTargetTypesDetails() {
        when(targetTypeDao.getTargetTypeConfigurationCodeBasedOnId1(any(String.class))).thenReturn(result);
        service.getTargetTypesDetails("1e93d863-5b9d-4aae-9a96-375aab27270b");
    }

    @Test
    public void getActiveTest() {
        when(targetTypeDao.getTargetTypeConfigurationCodeBasedOnId1(anyString())).thenReturn(result);
        row.put("id", "ID");
        Optional<Row> op = Optional.of(row);
        when(result.first()).thenReturn(op);
        TargetTypes targetType = Struct.create(TargetTypes.class);
        targetType.setId("ID");
        targetType.setIsActive(true);
        List<TargetTypes> targetTypeList = new ArrayList<>();
        targetTypeList.add(targetType);
        when(result.listOf(TargetTypes.class)).thenReturn(targetTypeList);
        service.getActive("1e93d863-5b9d-4aae-9a96-375aab27270b");
    }
    @Test
    public void getActivenullTest() {
        when(targetTypeDao.getTargetTypeConfigurationCodeBasedOnId1(anyString())).thenReturn(result);
        service.getActive("1e93d863-5b9d-4aae-9a96-375aab27270b");
    }

}