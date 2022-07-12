package com.sap.ic.cmh.masterdata.address.controller;

import com.sap.ic.cmh.masterdata.address.model.AddressRequest;
import com.sap.ic.cmh.masterdata.address.model.AddressResponse;
import com.sap.ic.cmh.masterdata.address.service.AddressService;
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
public class AddressController {
    public static final Logger logger = LoggerHelper.getLogger(AddressController.class);
    @Autowired
    private AddressService addressService;

    /**
     * Method takes as address request as an input for deleting the address
     *
     * @param address
     * @return List of address responses which are nor deleted
     */
    @DeleteMapping ("/Addresses")
    public ResponseEntity<List<AddressResponse>> deleteAddressLists(@RequestBody List<AddressRequest> address) {
        LoggerHelper.logMethodEntry(logger, "AddressController", "deleteAddressLists");
        final List<AddressResponse> addressResponse = addressService.deleteAddressList(address);
        LoggerHelper.logMethodExit(logger, "AddressController", "deleteAddressLists");
        return new ResponseEntity<>(addressResponse, HttpStatus.ACCEPTED);
    }
}