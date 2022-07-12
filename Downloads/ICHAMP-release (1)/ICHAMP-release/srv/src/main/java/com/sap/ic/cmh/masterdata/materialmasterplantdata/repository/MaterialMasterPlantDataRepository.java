package com.sap.ic.cmh.masterdata.materialmasterplantdata.repository;

import cds.gen.masterdataservice.MaterialMasterGeneralDatas;

import java.util.List;
import java.util.Map;
import com.sap.cds.Result;

public interface MaterialMasterPlantDataRepository {

    Map<String, String> getMaterialMasterPlantDataMap(List<String> materialMasterPlantDatas, List<String> plantList);

    List<MaterialMasterGeneralDatas> getMaterialMasterGeneralDataList(List<String> materialCodeList);

    void deleteMaterialMasterPlantDataList(List<String> recordsToBeDeleted);

    Result fetchMaterialMasterPlantDatas(String material, String plant);
}