package com.sap.ic.cmh.masterdata.businesspartner.controller;


import com.sap.ic.cmh.masterdata.businesspartner.model.BusinessPartnerRequest;
import com.sap.ic.cmh.masterdata.businesspartner.model.BusinessPartnerResponse;
import com.sap.ic.cmh.masterdata.businesspartner.model.PdmBusinessPartnerRequest;
import com.sap.ic.cmh.masterdata.businesspartner.service.BusinessPartnerService;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.ArrayList;
import java.util.List;


@RestController
@RequestMapping("/api/MasterDataService")
public class PdmBusinessPartnerController {
    public static final Logger logger = LoggerHelper.getLogger(PdmBusinessPartnerController.class);

    @Autowired
    private BusinessPartnerService businessPartnerService;

    @PostMapping("/BusinessPartners")
    public ResponseEntity<List<BusinessPartnerResponse>> deleteBusinessPartner(@RequestBody PdmBusinessPartnerRequest businessPartners ) {

        BusinessPartnerRequest listBusinessPartnerRequest = new BusinessPartnerRequest();
        listBusinessPartnerRequest.setBusinessPartner(businessPartners.getBusinessPartner());
        List<BusinessPartnerRequest> businessPartnerList = new ArrayList<>();
        businessPartnerList.add(listBusinessPartnerRequest);
        final List<BusinessPartnerResponse> businessPartnersResponse = businessPartnerService.deleteBusinessPartners(businessPartnerList);
        logger.info("*******************************************DELETED ********************" );
        return new ResponseEntity<>(businessPartnersResponse, HttpStatus.OK);
    }

}
