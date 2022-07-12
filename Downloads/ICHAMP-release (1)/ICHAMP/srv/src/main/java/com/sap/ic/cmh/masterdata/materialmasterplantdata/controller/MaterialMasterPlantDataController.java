package com.sap.ic.cmh.masterdata.materialmasterplantdata.controller;

import com.sap.ic.cmh.masterdata.materialmasterplantdata.model.MaterialMasterPlantDataRequest;
import com.sap.ic.cmh.masterdata.materialmasterplantdata.model.MaterialMasterPlantDataResponse;
import com.sap.ic.cmh.masterdata.materialmasterplantdata.service.MaterialMasterPlantDataService;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import java.util.List;

@RestController
@RequestMapping ("/api/MasterDataService")
public class MaterialMasterPlantDataController {

    public static final Logger logger = LoggerHelper.getLogger(MaterialMasterPlantDataController.class);

    @Autowired
    private MaterialMasterPlantDataService materialMasterPlantDataService;

    /**
     * Method takes as materialMasterPlantDatasRequest request as an input for deleting the master material plant data
     *
     * @param materialMasterPlantDatasRequest
     * @return List of material master plant data responses which are nor deleted
     */
    @DeleteMapping ("/MaterialMasterPlantDatas")
    public ResponseEntity<List<MaterialMasterPlantDataResponse>> deleteMaterialMasterPlantDataLists(@RequestBody List<MaterialMasterPlantDataRequest> materialMasterPlantDatasRequest) {
        LoggerHelper.logMethodEntry(logger, "MaterialMasterPlantDataController", "deleteMaterialMasterPlantDataLists");
        final List<MaterialMasterPlantDataResponse> materialMasterPlantDataResponse = materialMasterPlantDataService.deleteMaterialMasterPlantDataList(materialMasterPlantDatasRequest);
        LoggerHelper.logMethodExit(logger, "MaterialMasterPlantDataController", "deleteMaterialMasterPlantDataLists");
        return new ResponseEntity<>(materialMasterPlantDataResponse, HttpStatus.ACCEPTED);
    }
}