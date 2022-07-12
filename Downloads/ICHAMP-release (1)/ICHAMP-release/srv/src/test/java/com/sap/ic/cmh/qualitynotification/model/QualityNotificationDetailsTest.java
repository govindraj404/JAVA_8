package com.sap.ic.cmh.qualitynotification.model;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;

public class QualityNotificationDetailsTest {
    @InjectMocks
    @Autowired
    private QualityNotificationDetails qualityNotificationDetails;
    @Mock
    private QualityNotificationDTO qualityNotificationDTO;
    private List<SystemStatus> list;
    private List<QualityNotificaitonItemDetails> itemDetails;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        qualityNotificationDTO=new QualityNotificationDTO();
        qualityNotificationDetails=new QualityNotificationDetails();
    }

    @Test
    public void testSetMethod(){
        qualityNotificationDetails.setNotificationType("");
        qualityNotificationDetails.setMaterial("F01");
        qualityNotificationDetails.setPlant("PLAMT");
        qualityNotificationDetails.setPurchasingOrg("ORG");
        qualityNotificationDetails.setQuantity("10");
        qualityNotificationDetails.setUnitCode("CODE");
        qualityNotificationDetails.setSupplier("SUPP01");
        qualityNotificationDetails.setPersonResponsible("RESP");
        qualityNotificationDetails.setPurchaseOrderNumber("0001");
        qualityNotificationDetails.setPurchaseOrderItem("ITEM11");
        qualityNotificationDetails.setSystemStatus(list);
        qualityNotificationDetails.setItemDetails(itemDetails);
        qualityNotificationDetails.setNotificationNumber("100");

    }

    @Test
    public void testgetMethod(){
        qualityNotificationDetails.getNotificationType();
        qualityNotificationDetails.getMaterial();
        qualityNotificationDetails.getPlant();
        qualityNotificationDetails.getPurchasingOrg();
        qualityNotificationDetails.getQuantity();
        qualityNotificationDetails.getUnitCode();
        qualityNotificationDetails.getSupplier();
        qualityNotificationDetails.getPersonResponsible();
        qualityNotificationDetails.getPurchaseOrderNumber();
        qualityNotificationDetails.getPurchaseOrderItem();
        qualityNotificationDetails.getSystemStatus();
        qualityNotificationDetails.getItemDetails();
        qualityNotificationDetails.getNotificationNumber();
    }

    @Test
    public void testToString(){
        qualityNotificationDetails.toString();
    }
}
