package com.sap.ic.cmh.metering.controller;

import com.sap.ic.cmh.metering.service.MeteringService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/meter")
public class MeteringController {

    @Autowired
    MeteringService meteringService;

    @PutMapping("/metering")
    public ResponseEntity<String> sendMeteringData() {
        meteringService.send();
        return null;
    }
}