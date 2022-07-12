package com.sap.ic.cmh.qualitynotification.model;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

public class QualityNotificaitonItemDetailsTest {
    @InjectMocks
    @Autowired
    private QualityNotificaitonItemDetails qualityNotificaitonItemDetails;
    @Mock
    private QualityNotificationDTO qualityNotificationDTO;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        qualityNotificationDTO=new QualityNotificationDTO();
        qualityNotificaitonItemDetails=new QualityNotificaitonItemDetails();
    }

    @Test
    public void testSetMethod(){
        qualityNotificaitonItemDetails.setItemKey("KEY001");
        qualityNotificaitonItemDetails.setDefectGroup("GRP001");
        qualityNotificaitonItemDetails.setDefectCode("DEF002");
        qualityNotificaitonItemDetails.setDefectDescription("Desc");
    }

    @Test
    public void testGetMethod(){
        qualityNotificaitonItemDetails.getItemKey();
        qualityNotificaitonItemDetails.getDefectGroup();
        qualityNotificaitonItemDetails.getDefectCode();
        qualityNotificaitonItemDetails.getDefectDescription();
    }

    @Test
    public void testToString(){
        qualityNotificaitonItemDetails.toString();
    }
}
