package com.sap.ic.cmh.qualitynotification.model;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;

public class QualityNotificationDTOTest {
    @InjectMocks
    @Autowired
    private QualityNotificationDTO dto;
    @Mock
    private SupplierPersonDetails supplierDetails;
    @Mock
    private SupplierPersonDetails personDetails;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        dto=new QualityNotificationDTO();
    }
    @Test
    public void testSetMethod(){
    	Map<String,Object> qualityMap = new HashMap<>();
    	qualityMap.put("ID", "1");
    	dto.setQualityNotifications(qualityMap);
        dto.setSupplierDetails(supplierDetails);
        dto.setPersonDetails(personDetails);
    }

    @Test
    public void testGetMethod(){
        dto.getSupplierDetails();
        dto.getPersonDetails();
        dto.getQualityNotifications();
    }
}
