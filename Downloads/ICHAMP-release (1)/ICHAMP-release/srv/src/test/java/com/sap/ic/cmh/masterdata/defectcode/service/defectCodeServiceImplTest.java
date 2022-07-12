package com.sap.ic.cmh.masterdata.defectcode.service;

import cds.gen.com.sap.ic.cmh.defectcode.DefectCode_;
import cds.gen.masterdataservice.DefectCodes;
import cds.gen.masterdataservice.DefectGroups;
import cds.gen.masterdataservice.PurchasingGroups;
import cds.gen.masterdataservice.UnitOfMeasures;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.masterdata.defectcode.repository.DefectCodeRepository;
import com.sap.ic.cmh.masterdata.materialmastergeneraldata.model.MaterialMasterGeneralDataRequest;
import com.sap.ic.cmh.masterdata.materialmastergeneraldata.model.MaterialMasterGeneralDataResponse;
import com.sap.ic.cmh.masterdata.materialmastergeneraldata.repository.MaterialMasterGeneralDataRepository;
import com.sap.ic.cmh.masterdata.materialmastergeneraldata.service.MaterialMasterGeneralDataServiceImpl;
import com.sap.ic.cmh.utils.LocaleMessageHelper;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.*;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class defectCodeServiceImplTest {
    @Mock
    private DefectCodeRepository repository;
    @InjectMocks
    private DefectCodeServiceImpl services;
    @Mock
    LocaleMessageHelper messageHelper;

    @Mock
    protected PersistenceService db;
    @Mock
    Result result;

    private Row row;
    private Optional<Row> opt;

    List<DefectCodes> codeItemList = new ArrayList<>();
    private  Map<String, String> dataMap;

     private  String id = null;
    private DefectCodes codeItem;
    private DefectGroups group;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);

        group = Struct.create(DefectGroups.class);
        group.setCode("QM-E");
        group.setDescription("test");
        codeItem = Struct.create(DefectCodes.class);
        codeItem.setCode("1");
        codeItem.setDescription("test");
        codeItem.setDefectGroupCode(group.getCode());
    }

    @Test
    public void testGetUnitOfMeasureDetails() {
        Map<String, String> DefectCodeMap = new HashMap<>();
        DefectCodeMap.put("200001", "Australia");
        when(result.listOf(DefectCodes.class)).thenReturn(codeItemList);

        when(result.listOf(DefectCodes.class)).thenReturn(codeItemList);
        when(repository.fetchDefectCode(codeItem.getCode(),codeItem.getDefectGroupCode())).thenReturn(result);
       services.fetchDefectCode(codeItem.getCode(),codeItem.getDefectGroupCode());

    }

    @Test
    public void testGetUnitOfMeasureDetailsNull() {
        when(repository.fetchDefectCode(codeItem.getCode(),codeItem.getDefectGroupCode())).thenReturn(result);
        services.fetchDefectCode(codeItem.getCode(),codeItem.getDefectGroupCode());

    }
}
