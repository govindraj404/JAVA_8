package com.sap.ic.cmh.masterdata.plant.controller;

import com.sap.ic.cmh.masterdata.plant.model.PlantRequest;
import com.sap.ic.cmh.masterdata.plant.model.PlantResponse;
import com.sap.ic.cmh.masterdata.plant.service.PlantService;
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
public class PlantController {

    public static final Logger logger = LoggerHelper.getLogger(PlantController.class);

    @Autowired
    private PlantService plantService;

    /**
     * Method takes as plant request as an input to delete
     *
     * @param plantListRequest
     * @return List of Plant responses which are nor deleted
     */
    @DeleteMapping ("/Plants")
    public ResponseEntity<List<PlantResponse>> deletePlants(@RequestBody List<PlantRequest> plantListRequest) {
        LoggerHelper.logMethodEntry(logger, "PlantController", "deletePlants");
        final List<PlantResponse> plantErrorListResponse = plantService.deletePlantList(plantListRequest);
        LoggerHelper.logMethodExit(logger, "PlantController", "deletePlants");
        return new ResponseEntity<>(plantErrorListResponse, HttpStatus.ACCEPTED);
    }
}