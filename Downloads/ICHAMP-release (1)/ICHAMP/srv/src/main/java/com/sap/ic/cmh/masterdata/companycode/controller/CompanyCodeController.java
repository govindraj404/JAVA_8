package com.sap.ic.cmh.masterdata.companycode.controller;

import com.sap.ic.cmh.masterdata.companycode.model.CompanyCodeRequest;
import com.sap.ic.cmh.masterdata.companycode.model.CompanyCodeResponse;
import com.sap.ic.cmh.masterdata.companycode.service.CompanyCodeService;
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
public class CompanyCodeController {
    public static final Logger logger = LoggerHelper.getLogger(CompanyCodeController.class);
    @Autowired
    private CompanyCodeService companyCodeService;

    /**
     * Method takes as company code request as an input for deleting the company code
     *
     * @param companyCodeLists
     * @return List of Company code responses which are nor deleted
     */
    @DeleteMapping ("/CompanyCodes")
    public ResponseEntity<List<CompanyCodeResponse>> deleteCompanyCodeLists(@RequestBody List<CompanyCodeRequest> companyCodeLists) {
        LoggerHelper.logMethodEntry(logger, "CompanyCodeController", "deleteCompanyCodeLists");
        final List<CompanyCodeResponse> companyCodeResponse = companyCodeService.deleteCompanyCodeList(companyCodeLists);
        LoggerHelper.logMethodExit(logger, "CompanyCodeController", "deleteCompanyCodeLists");
        return new ResponseEntity<>(companyCodeResponse, HttpStatus.ACCEPTED);
    }
}