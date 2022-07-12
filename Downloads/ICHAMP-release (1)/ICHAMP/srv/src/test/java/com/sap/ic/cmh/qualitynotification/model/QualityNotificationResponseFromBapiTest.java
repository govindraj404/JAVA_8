package com.sap.ic.cmh.qualitynotification.model;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

public class QualityNotificationResponseFromBapiTest {
    @InjectMocks
    @Autowired
    private QualityNotificationResponseFromBapi qualityNotificationResponseFromBapi;
    @Mock
    private QualityNotificationDTO qualityNotificationDTO;
    @Mock
    private QualityNotificationDetails qualityNotificationDetails;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        qualityNotificationDTO=new QualityNotificationDTO();
        qualityNotificationResponseFromBapi=new QualityNotificationResponseFromBapi();
    }

    @Test
    public void testSetMethod(){
        qualityNotificationResponseFromBapi.setErrorMessage("errr");
        qualityNotificationResponseFromBapi.setQualityNotificationDetails(qualityNotificationDetails);
    }

    @Test
    public void testGetMethod(){
        qualityNotificationResponseFromBapi.getErrorMessage();
        qualityNotificationResponseFromBapi.getQualityNotificationDetails();
    }

}
