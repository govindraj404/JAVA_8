package com.sap.ic.cmh.masterdata.businesspartner.controller;

import com.sap.ic.cmh.masterdata.businesspartner.model.BusinessPartnerRequest;
import com.sap.ic.cmh.masterdata.businesspartner.model.BusinessPartnerResponse;
import com.sap.ic.cmh.masterdata.businesspartner.service.BusinessPartnerService;
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
public class BusinessPartnerController {
    public static final Logger logger = LoggerHelper.getLogger(BusinessPartnerController.class);

    @Autowired
    private BusinessPartnerService businessPartnerService;

    /**
     * Method takes as business partner request as an input for deleting the business partner
     *
     * @param businessPartnersRequest
     * @return List of Business Partner responses which are nor deleted
     */
    @DeleteMapping ("/BusinessPartners")
    public ResponseEntity<List<BusinessPartnerResponse>> deleteBusinessPartnerLists(@RequestBody List<BusinessPartnerRequest> businessPartnersRequest) {
        LoggerHelper.logMethodEntry(logger, "BusinessPartnerController", "deleteBusinessPartnerLists");
        final List<BusinessPartnerResponse> businessPartnersResponse = businessPartnerService.deleteBusinessPartners(businessPartnersRequest);
        LoggerHelper.logMethodExit(logger, "BusinessPartnerController", "deleteBusinessPartnerLists");
        return new ResponseEntity<>(businessPartnersResponse, HttpStatus.ACCEPTED);
    }
}
