package com.sap.ic.cmh.qualitynotification.model;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;

public class SupplierPersonDetailsTest {
    @InjectMocks
    @Autowired
    private SupplierPersonDetails handler;
    private QualityNotificationDTO qualityNotificationDTO;


    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        handler=new SupplierPersonDetails();
        qualityNotificationDTO=new QualityNotificationDTO();
    }
    @Test
    public void setMethodTest(){
        handler.setCode("100");
        handler.setRole("admin");
        qualityNotificationDTO.setPersonDetails(handler);
        qualityNotificationDTO.setSupplierDetails(handler);
    }
    @Test
    public void getMethodTest(){
        handler.getCode();
        handler.getRole();
        qualityNotificationDTO.getPersonDetails();
        qualityNotificationDTO.getSupplierDetails();
    }
} 
