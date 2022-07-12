package com.sap.ic.cmh.masterdata.division.controller;


import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.sap.ic.cmh.masterdata.division.model.DivisionRequest;
import com.sap.ic.cmh.masterdata.division.model.DivisionResponse;
import com.sap.ic.cmh.masterdata.division.service.DivisionService;
import com.sap.ic.cmh.utils.LoggerHelper;

import java.util.List;

/**
 * This class used to receive division details for delete
 */
@RestController
@RequestMapping ("/api/MasterDataService")
public class DivisionController {

    public static final Logger logger = LoggerHelper.getLogger(DivisionController.class);

    @Autowired
    private DivisionService divisionService;

    /**
     * Method takes as division request as an input for deleting the division
     *
     * @param divisionsRequest
     * @return List of division responses which are nor deleted
     */
    @DeleteMapping ("/Divisions")
    public ResponseEntity<List<DivisionResponse>> deleteDivisionLists(@RequestBody List<DivisionRequest> divisionsRequest) {
        LoggerHelper.logMethodEntry(logger, "DivisionController", "deleteDivisionLists");
        final List<DivisionResponse> divisionResponse = divisionService.deleteDivisionList(divisionsRequest);
        LoggerHelper.logMethodExit(logger, "DivisionController", "deleteDivisionLists");
        return new ResponseEntity<>(divisionResponse, HttpStatus.ACCEPTED);
    }

}