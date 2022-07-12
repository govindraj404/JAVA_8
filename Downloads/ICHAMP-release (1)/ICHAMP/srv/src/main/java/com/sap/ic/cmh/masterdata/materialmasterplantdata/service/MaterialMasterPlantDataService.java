package com.sap.ic.cmh.masterdata.materialmasterplantdata.service;

import java.util.List;
import com.sap.ic.cmh.masterdata.materialmasterplantdata.model.MaterialMasterPlantDataRequest;
import com.sap.ic.cmh.masterdata.materialmasterplantdata.model.MaterialMasterPlantDataResponse;
import cds.gen.masterdataservice.MaterialMasterPlantDatas;

public interface MaterialMasterPlantDataService {

    List<MaterialMasterPlantDataResponse> deleteMaterialMasterPlantDataList(List<MaterialMasterPlantDataRequest> materialMasterPlantDataRequest);

    MaterialMasterPlantDatas fetchMaterialMasterPlantDatas(String material, String plant);
}