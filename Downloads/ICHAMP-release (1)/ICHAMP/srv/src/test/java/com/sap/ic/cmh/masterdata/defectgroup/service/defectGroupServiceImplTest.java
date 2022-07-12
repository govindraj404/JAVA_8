package com.sap.ic.cmh.masterdata.defectgroup.service;

import cds.gen.masterdataservice.DefectCodes;
import cds.gen.masterdataservice.DefectGroups;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.masterdata.defectcode.repository.DefectCodeRepository;
import com.sap.ic.cmh.masterdata.defectcode.service.DefectCodeServiceImpl;
import com.sap.ic.cmh.masterdata.defectgroup.repository.DefectGroupRepository;
import com.sap.ic.cmh.utils.LocaleMessageHelper;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.*;

import static org.mockito.Mockito.when;

public class defectGroupServiceImplTest {
    @Mock
    private DefectGroupRepository repository;
    @InjectMocks
    private DefectGroupServiceImpl services;
    @Mock
    LocaleMessageHelper messageHelper;

    @Mock
    protected PersistenceService db;
    @Mock
    Result result;

    private Row row;
    private Optional<Row> opt;

    List<DefectGroups> groupItemList = new ArrayList<>();
    private DefectGroups group;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);

        group = Struct.create(DefectGroups.class);
        group.setCode("QM-E");
        group.setDescription("test");
    }

    @Test
    public void testFetchDefectGroupCode() {
        Map<String, String> DefectGroupsMap = new HashMap<>();
        DefectGroupsMap.put("200001", "Australia");
        when(result.listOf(DefectGroups.class)).thenReturn(groupItemList);

        when(repository.fetchDefectGroupCode(group.getCode())).thenReturn(result);
       services.fetchDefectGroupCode(group.getCode());

    }

    @Test
    public void testFetchDefectGroupCodeNull() {
        when(repository.fetchDefectGroupCode(group.getCode())).thenReturn(result);
        services.fetchDefectGroupCode(group.getCode());

    }
}
