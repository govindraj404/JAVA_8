package com.sap.ic.cmh.masterdata.materialmastergeneraldata.controller;

import com.sap.ic.cmh.masterdata.materialmastergeneraldata.model.MaterialMasterGeneralDataRequest;
import com.sap.ic.cmh.masterdata.materialmastergeneraldata.model.MaterialMasterGeneralDataResponse;
import com.sap.ic.cmh.masterdata.materialmastergeneraldata.service.MaterialMasterGeneralDataService;
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
public class MaterialMasterGeneralDataController {

    public static final Logger logger = LoggerHelper.getLogger(MaterialMasterGeneralDataController.class);

    @Autowired
    private MaterialMasterGeneralDataService materialMasterGeneralDataService;

    /**
     * Method takes as masterGeneralDataRequests request as an input for deleting the master material general data
     *
     * @param masterGeneralDataRequests
     * @return List of masterial master general data responses which are nor deleted
     */
    @DeleteMapping ("/MaterialMasterGeneralDatas")
    public ResponseEntity<List<MaterialMasterGeneralDataResponse>> deleteMaterialMasterGeneralDataLists(@RequestBody List<MaterialMasterGeneralDataRequest> masterGeneralDataRequests) {
        LoggerHelper.logMethodEntry(logger, "MaterialMasterGeneralDataController", "deleteMaterialMasterGeneralDataLists");
        final List<MaterialMasterGeneralDataResponse> materialMasterGeneralDataResponse = materialMasterGeneralDataService.deleteMaterialMasterGeneralDataList(masterGeneralDataRequests);
        LoggerHelper.logMethodExit(logger, "MaterialMasterGeneralDataController", "deleteMaterialMasterGeneralDataLists");
        return new ResponseEntity(materialMasterGeneralDataResponse, HttpStatus.ACCEPTED);
    }

}