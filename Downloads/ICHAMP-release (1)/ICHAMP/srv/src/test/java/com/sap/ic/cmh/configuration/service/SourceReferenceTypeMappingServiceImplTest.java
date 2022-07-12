package com.sap.ic.cmh.configuration.service;

import cds.gen.configurationservice.SourceReferenceTypeMappings;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.persistency.SourceReferenceTypeMappingDao;
import com.sap.ic.cmh.configuration.service.sourcereferencetypemappings.SourceReferenceTypeMappingServiceImpl;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import java.util.List;
import java.util.ArrayList;
import java.util.Optional;
import com.sap.cds.Row;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class SourceReferenceTypeMappingServiceImplTest {

    @Mock
    SourceReferenceTypeMappingDao sourceReferenceTypeMappingDao;
    @InjectMocks
    SourceReferenceTypeMappingServiceImpl service;
    private SourceReferenceTypeMappings mapping;
    @Mock
    Result result;
    @Mock
    SourceReferenceTypeMappings maps;
    @Mock
    PersistenceService mockDb;

    @Before
    public void before() {
        MockitoAnnotations.openMocks(this);
        mapping = Struct.create(SourceReferenceTypeMappings.class);
    }

    @Test
    public void getSourceReferenceTypeMappingsTest() {
        when(sourceReferenceTypeMappingDao.getSourceReferenceTypeMappings()).thenReturn(result);
        service.getSourceReferenceTypeMappings();
    }

    @Test
    public void getSourceReferenceTypeMappingBasedOnValuesTest() {
        when(sourceReferenceTypeMappingDao.getSourceReferenceTypeMappingBasedOnValues(mapping)).thenReturn(result);
        service.getSourceReferenceTypeMappingBasedOnValues(mapping);
    }

    @Test
    public void getRefrenceTypeMappingBasedOnIDTest()
    {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(sourceReferenceTypeMappingDao.getSourceReferenceTypeMappingBasedOnId(maps.getId())).thenReturn(result);
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
        List<SourceReferenceTypeMappings> list=new ArrayList<>();
        list.add(maps);
        when(result.listOf(SourceReferenceTypeMappings.class)).thenReturn(list);
        service.getSourceReferenceTypeMappingsDetails(maps.getId());
    }
    @Test
    public void getRefrenceTypeMappingBasedOnIDEmptyTest()
    {
        when(sourceReferenceTypeMappingDao.getSourceReferenceTypeMappingBasedOnId(maps.getId())).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(0L);
        when(result.list()).thenReturn(rowValues);
        List<SourceReferenceTypeMappings> list=new ArrayList<>();
        list.add(maps);
        when(result.listOf(SourceReferenceTypeMappings.class)).thenReturn(list);
        service.getSourceReferenceTypeMappingsDetails(maps.getId());
    }
}
