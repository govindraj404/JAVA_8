package com.sap.ic.cmh.masterdata.materialmastergeneraldata.handler;


import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;


import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.*;
import java.util.Optional;

import cds.gen.masterdataservice.MaterialMasterGeneralDatas_;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.cds.CdsUpdateEventContext;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.masterdata.materialmastergeneraldata.service.MaterialMasterGeneralDataService;
import com.sap.ic.cmh.masterdata.materialmastergeneraldata.validation.MaterialMasterGeneralDataValidator;
import com.sap.ic.cmh.masterdata.unitofmeasure.service.UnitOfMeasureService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import cds.gen.com.sap.ic.cmh.unitofmeasure.UnitOfMeasures;
import cds.gen.masterdataservice.MaterialMasterGeneralDatas;

public class MaterialMasterGeneralDataHandlerTest {
    @InjectMocks
    @Autowired
    private MaterialMasterGeneralDataHandler handler;
    @Mock
     PersistenceService mockDb;
    @Mock
    private Messages messages;
    @Mock
    private Message msg;
    @Mock
    private MaterialMasterGeneralDataValidator validator;

    @Mock
    CdsService cdsServ;
    @Mock
    Result result;
    @Mock
    Row row;
    @Mock
    CdsCreateEventContext createContextMock;
    @Mock
    CqnInsert cqnInsert;
    @Mock
    MaterialMasterGeneralDataService materialMasterGeneralDataService;

    @Mock
    Message message;

    @Mock
    UnitOfMeasureService unitOfMeasureService;

    @Mock
    CdsUpdateEventContext cdsUpdateEventContext;

    private MaterialMasterGeneralDatas masterGeneralDatas;
    
    private UnitOfMeasures unitofMeaseure;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        masterGeneralDatas = Struct.create(MaterialMasterGeneralDatas.class);
        masterGeneralDatas.setEanUpc("test");
        masterGeneralDatas.setMaterialCode("test123");
        masterGeneralDatas.setMaterialDescription("materialDescription");
        masterGeneralDatas.setMaterialGroup("materialGroup");
        masterGeneralDatas.setId("123");
        unitofMeaseure = Struct.create(UnitOfMeasures.class);
        unitofMeaseure.setCode(UUID.randomUUID().toString());
         unitofMeaseure.setName("name");

        when(createContextMock.getCqn()).thenReturn(cqnInsert);
        List<Map<String, Object>> entries = new ArrayList<>();
        when(cqnInsert.entries()).thenReturn(entries);
    }


    @Test
    public void testUpdateMaterialMasterGeneralDataNull() {
        MaterialMasterGeneralDatas masterGeneralDatas1 = Struct.create(MaterialMasterGeneralDatas.class);
        when(materialMasterGeneralDataService.fetchMaterialMasterGeneralDataBasedOnCode("test123")).thenReturn(masterGeneralDatas1);
        handler.onMaterialMasterGeneralDataCreate(createContextMock, masterGeneralDatas1);
    }

    @Test(expected = NullPointerException.class)
    public void testOnMaterialMasterGeneralDataCreate(){
        Optional<MaterialMasterGeneralDatas> opt = Optional.of(masterGeneralDatas);

        when(mockDb.run(any(CqnUpdate.class))).thenReturn(result);
        when(result.first(MaterialMasterGeneralDatas.class)).thenReturn(opt);
        when(materialMasterGeneralDataService.fetchMaterialMasterGeneralDataBasedOnCode("test123")).thenReturn(masterGeneralDatas);
        handler.onMaterialMasterGeneralDataCreate(createContextMock,masterGeneralDatas);
    }

    @Test
    public void testBeforeMaterialMasterGeneralDataCreate(){
        cds.gen.masterdataservice.UnitOfMeasures unit = Struct.create(cds.gen.masterdataservice.UnitOfMeasures.class);
        unit.setCode(UUID.randomUUID().toString());
        when(unitOfMeasureService.getUnitOfMeasureDetails(masterGeneralDatas.getBaseUnitOfMeasureCode())).thenReturn(unit);
        handler.beforeMaterialMasterGeneralDataCreate(createContextMock,masterGeneralDatas);
    }

    @Test
    public void testBeforeUpdateMaterialMasterGeneralData(){

        cds.gen.masterdataservice.UnitOfMeasures unit = Struct.create(cds.gen.masterdataservice.UnitOfMeasures.class);
        unit.setCode(UUID.randomUUID().toString());
        when(unitOfMeasureService.getUnitOfMeasureDetails(masterGeneralDatas.getBaseUnitOfMeasureCode())).thenReturn(unit);
        handler.beforeUpdateMaterialMasterGeneralData(cdsUpdateEventContext,masterGeneralDatas);
    }

}