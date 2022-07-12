package com.sap.ic.cmh.masterdata.salesorganization.controller;


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.sap.ic.cmh.masterdata.salesorganization.model.SalesOrganizationRequest;
import com.sap.ic.cmh.masterdata.salesorganization.model.SalesOrganizationResponse;
import com.sap.ic.cmh.masterdata.salesorganization.service.SalesOrganizationService;
import com.sap.ic.cmh.utils.LoggerHelper;

import java.util.List;

/**
 * This class used to receive sales organization details for delete
 */

@RestController
@RequestMapping ("/api/MasterDataService")
public class SalesOrganizationController {

    public static final Logger logger = LoggerFactory.getLogger(SalesOrganizationController.class);

    @Autowired
    private SalesOrganizationService salesOrganizationService;

    /**
     * Method takes sales organization request as an input to delete
     *
     * @param salesOrganizationServicesRequest
     * @return List of sales organization responses which are nor deleted
     */
    @DeleteMapping ("/SalesOrganizations")
    public ResponseEntity<List<SalesOrganizationResponse>> deleteSalesOrganizationLists(@RequestBody List<SalesOrganizationRequest> salesOrganizationServicesRequest) {
        LoggerHelper.logMethodEntry(logger, "SalesOrganizationController", "deleteSalesOrganizationLists");
        final List<SalesOrganizationResponse> salesOrganizationResponse = salesOrganizationService.deleteSalesOrganizationList(salesOrganizationServicesRequest);
        LoggerHelper.logMethodExit(logger, "SalesOrganizationController", "deleteSalesOrganizationLists");
        return new ResponseEntity<>(salesOrganizationResponse, HttpStatus.ACCEPTED);
    }
}