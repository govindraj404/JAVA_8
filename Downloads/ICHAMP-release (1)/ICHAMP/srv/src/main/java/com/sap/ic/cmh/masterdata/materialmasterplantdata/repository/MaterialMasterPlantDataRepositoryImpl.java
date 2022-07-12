package com.sap.ic.cmh.masterdata.materialmasterplantdata.repository;

import cds.gen.masterdataservice.MaterialMasterGeneralDatas;
import cds.gen.masterdataservice.MaterialMasterGeneralDatas_;
import cds.gen.masterdataservice.MaterialMasterPlantDatas;
import cds.gen.masterdataservice.MaterialMasterPlantDatas_;
import com.sap.cds.Result;
import com.sap.cds.ql.Delete;
import com.sap.cds.ql.Select;
import com.sap.cds.ql.cqn.CqnDelete;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;


@Component
public class MaterialMasterPlantDataRepositoryImpl implements MaterialMasterPlantDataRepository {
    public static final Logger logger = LoggerHelper.getLogger(MaterialMasterPlantDataRepositoryImpl.class);

    @Autowired
    private PersistenceService db;

    @Override
    public Map<String, String> getMaterialMasterPlantDataMap(List<String> materialCodeList, List<String> plantList) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "getMaterialMasterPlantDataMap");
        CqnSelect materialMasterPlantDataSelect = Select.from(MaterialMasterPlantDatas_.class).columns("ID", "materialCode")
                .where(b -> b.materialCode().in(materialCodeList).and(b.plant().in(plantList)));
        List<MaterialMasterPlantDatas> materialMasterPlantDataList = db.run(materialMasterPlantDataSelect).listOf(MaterialMasterPlantDatas.class);
        logger.info("Master Plant Data List ", materialMasterPlantDataList.size());
        return materialMasterPlantDataList.stream().collect(Collectors.toMap
                (MaterialMasterPlantDatas::getMaterialCode, MaterialMasterPlantDatas::getId));
    }

    @Override
    public List<MaterialMasterGeneralDatas> getMaterialMasterGeneralDataList(List<String> materialCodeList) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "getMaterialMasterPlantDataMap");
        CqnSelect materialMasterGeneralDataSelect = Select.from(MaterialMasterGeneralDatas_.class).columns("ID", "materialCode")
                .where(b -> b.materialCode().in(materialCodeList));
        return db.run(materialMasterGeneralDataSelect).listOf(MaterialMasterGeneralDatas.class);
    }

    @Override
    public void deleteMaterialMasterPlantDataList(List<String> recordsToBeDeleted) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "deleteInactiveMaterialMasterPlantData");
        CqnDelete delete = Delete.from(MaterialMasterPlantDatas_.class).where(cc -> cc.ID().in(recordsToBeDeleted));
        long deleteCount = db.run(delete).rowCount();
        logger.info("Records Deleted count: ", deleteCount);
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "deleteInactiveMaterialMasterPlantData");
    }

    @Override
    public Result fetchMaterialMasterPlantDatas(String material, String plant) {
        return db.run(Select.from(MaterialMasterPlantDatas_.class)
        .where(b -> b.materialCode().eq(material)
                .and(b.plant().eq(plant))));
    }
}