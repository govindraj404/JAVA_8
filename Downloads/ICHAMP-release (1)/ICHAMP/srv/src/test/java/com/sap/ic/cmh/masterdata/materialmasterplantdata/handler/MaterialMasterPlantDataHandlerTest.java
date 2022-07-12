package com.sap.ic.cmh.masterdata.materialmasterplantdata.handler;

import cds.gen.com.sap.ic.cmh.unitofmeasure.UnitOfMeasures;
import cds.gen.masterdataservice.MaterialMasterGeneralDatas;
import cds.gen.masterdataservice.MaterialMasterPlantDatas;
import cds.gen.masterdataservice.MaterialMasterPlantDatas_;
import cds.gen.masterdataservice.Plants;
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
import com.sap.cloud.sdk.s4hana.datamodel.odata.namespaces.trialbalance.Material;
import com.sap.ic.cmh.masterdata.materialmastergeneraldata.service.MaterialMasterGeneralDataService;
import com.sap.ic.cmh.masterdata.materialmasterplantdata.service.MaterialMasterPlantDataService;
import com.sap.ic.cmh.masterdata.materialmasterplantdata.validation.MaterialMasterPlantDataValidator;
import com.sap.ic.cmh.masterdata.plant.service.PlantService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.*;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class MaterialMasterPlantDataHandlerTest {

    @InjectMocks
    @Autowired
    private MaterialMasterPlantDataHandler handler;
    @Mock
    protected PersistenceService mockDb;
    @Mock
    PlantService plantService;
    @Mock
    private MaterialMasterPlantDataValidator validator;
    @Mock
    MaterialMasterGeneralDataService materialMasterGeneralDataService;
    @Mock
    Messages messages;

    @Mock
    CdsService cdsServ;
    @Mock
    Result result;
    @Mock
    Row row;
    @Mock
    CdsCreateEventContext createContextMock;
    @Mock
    CdsUpdateEventContext updateEventContext;
    @Mock
    CqnInsert cqnInsert;

    @Mock
    Message message;

    @Mock
    MaterialMasterPlantDataService materialMasterPlantDataService;

    private MaterialMasterPlantDatas masterGeneralData;

    private Plants plants;
    private MaterialMasterGeneralDatas masterGeneralDatas;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        plants = Struct.create(Plants.class);
        plants.setId(UUID.randomUUID().toString());
        plants.setPlant("name");
        plants.setAddress("sap");
        plants.setPlantName("sap");

        masterGeneralData = Struct.create(MaterialMasterPlantDatas.class);
        masterGeneralData.setMaterialCode("1213");
        masterGeneralData.setPlant(plants.getPlant());
        masterGeneralData.setId("4534");

        masterGeneralDatas = Struct.create(MaterialMasterGeneralDatas.class);
        masterGeneralDatas.setMaterialCode("123");
        masterGeneralDatas.setMaterialGroup("grp");
        masterGeneralDatas.setId("1234");



        when(createContextMock.getCqn()).thenReturn(cqnInsert);
        List<Map<String, Object>> entries = new ArrayList<>();
        when(cqnInsert.entries()).thenReturn(entries);
    }


    @Test(expected = NullPointerException.class)
    public void testCreateMaterialMasterPlantData() {


        when(materialMasterPlantDataService.fetchMaterialMasterPlantDatas(masterGeneralData.getMaterialCode(),masterGeneralData.getPlant())).thenReturn(masterGeneralData);

        Optional<Plants> emptyOpt = Optional.of(plants);
        when(mockDb.run(any(CqnUpdate.class))).thenReturn(result);
        when(result.first(Plants.class)).thenReturn(emptyOpt);

        Optional<MaterialMasterPlantDatas> opt = Optional.of(masterGeneralData);
        when(mockDb.run(any(CqnUpdate.class))).thenReturn(result);
        when(result.first(MaterialMasterPlantDatas.class)).thenReturn(opt);


        handler.onMaterialMasterPlantDataCreate(createContextMock, masterGeneralData);
    }

    @Test
    public void testBeforeMaterialMasterPlantDataCreate(){
        when(plantService.fetchPlant(masterGeneralData.getPlant(),"message", MaterialMasterPlantDatas_.class, MaterialMasterPlantDatas_::plant)).thenReturn(plants);
        when(materialMasterGeneralDataService.fetchMaterialMasterGeneralData(masterGeneralData.getMaterialCode(),"message",MaterialMasterPlantDatas_.class, MaterialMasterPlantDatas_::materialCode)).thenReturn(masterGeneralDatas);
        handler.beforeMaterialMasterPlantDataCreate(createContextMock,masterGeneralData);

    }

    @Test
    public void testBeforeUpdateMaterialMasterGeneralData(){
        when(plantService.fetchPlant(masterGeneralData.getPlant(),"message", MaterialMasterPlantDatas_.class, MaterialMasterPlantDatas_::plant)).thenReturn(plants);
        when(materialMasterGeneralDataService.fetchMaterialMasterGeneralData(masterGeneralData.getMaterialCode(),"message",MaterialMasterPlantDatas_.class, MaterialMasterPlantDatas_::materialCode)).thenReturn(masterGeneralDatas);
        handler.beforeUpdateMaterialMasterGeneralData(updateEventContext,masterGeneralData);
    }

}
