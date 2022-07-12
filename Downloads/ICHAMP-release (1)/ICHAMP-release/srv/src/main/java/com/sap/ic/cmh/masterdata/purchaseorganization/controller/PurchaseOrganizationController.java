package com.sap.ic.cmh.masterdata.purchaseorganization.controller;

import com.sap.ic.cmh.masterdata.purchaseorganization.model.PurchaseOrganizationRequest;
import com.sap.ic.cmh.masterdata.purchaseorganization.model.PurchaseOrganizationResponse;
import com.sap.ic.cmh.masterdata.purchaseorganization.service.PurchaseOrganizationService;
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
@RequestMapping("/api/MasterDataService")
public class PurchaseOrganizationController {

    public static final Logger logger = LoggerHelper.getLogger(PurchaseOrganizationController.class);

    @Autowired
    private PurchaseOrganizationService purchaseOrganizationService;

    /**
     * Method takes Purchase Organization request as an input to delete
     *
     * @param purchaseOrganizationRequest
     * @return List of purchase organization responses which are nor deleted
     */
    @DeleteMapping("/PurchaseOrganizations")
    public ResponseEntity<List<PurchaseOrganizationResponse>> deletePurchaseOrganization(@RequestBody List<PurchaseOrganizationRequest> purchaseOrganizationRequest) {
        LoggerHelper.logMethodEntry(logger, "PurchaseOrganizationController", "deletePurchaseOrganization");
        final List<PurchaseOrganizationResponse> purchaseOrganizationResponseList = purchaseOrganizationService.
                deletePurchaseOrganizationList(purchaseOrganizationRequest);
        LoggerHelper.logMethodExit(logger, "PurchaseOrganizationController", "deletePurchaseOrganization");
        return new ResponseEntity<>(purchaseOrganizationResponseList, HttpStatus.ACCEPTED);
    }
}
