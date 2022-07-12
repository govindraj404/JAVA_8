
package com.sap.ic.cmh.configuration.service;

import cds.gen.configurationservice.TargetReferenceTypeMappings;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.persistency.TargetReferenceTypeMappingsDao;
import com.sap.ic.cmh.configuration.service.targetreferencetypemappings.TargetReferenceTypeMappingServiceImpl;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@Service
public class TargetReferenceTypeMappingServiceImplTest {

    @InjectMocks
    TargetReferenceTypeMappingServiceImpl service;
    @Mock
    TargetReferenceTypeMappingsDao targetReferenceTypeMappingDao;

    @Mock
    Result result;

    @Mock
    TargetReferenceTypeMappings maps;
    @Mock
    PersistenceService mockDb;

    @Before
    public void setBefore() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void testGetTargetReferenceTypeMappings() {
        when(targetReferenceTypeMappingDao.getTargetReferenceTypeMappings()).thenReturn(result);
        service.getTargetReferenceTypeMappings();
    }

    @Test
    public void testGetTargetReferenceTypeMappingBasedOnValues() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        targetReferenceTypeMappingDao.getTargetReferenceTypeMappingBasedOnValues(maps);
        service.getTargetReferenceTypeMappingBasedOnValues(maps);
    }

    @Test
    public void getRefrenceTypeMappingBasedOnIDTest()
    {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(targetReferenceTypeMappingDao.getTargetReferenceTypeMappingDetailsBasedOnID(maps.getId())).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row= Struct.create(Row.class);
        row.put("ID", "201");
        row.put("identifier", "2");
        row.put("code", "reasoncode");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(1L);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        List<TargetReferenceTypeMappings> list=new ArrayList<>();
        list.add(maps);
        when(result.listOf(TargetReferenceTypeMappings.class)).thenReturn(list);
        service.getRefrenceTypeMappingBasedOnID(maps.getId());
    }
    @Test
    public void getRefrenceTypeMappingBasedOnIDEmptyTest()
    {
        when(targetReferenceTypeMappingDao.getTargetReferenceTypeMappingDetailsBasedOnID(maps.getId())).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(0L);
        when(result.list()).thenReturn(rowValues);
        List<TargetReferenceTypeMappings> list=new ArrayList<>();
        list.add(maps);
        when(result.listOf(TargetReferenceTypeMappings.class)).thenReturn(list);
        service.getRefrenceTypeMappingBasedOnID(maps.getId());
    }
}