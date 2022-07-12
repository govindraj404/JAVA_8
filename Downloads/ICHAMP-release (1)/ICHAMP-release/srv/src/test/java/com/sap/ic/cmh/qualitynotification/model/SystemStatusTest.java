package com.sap.ic.cmh.qualitynotification.model;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

public class SystemStatusTest {
    @InjectMocks
    @Autowired
    private SystemStatus systemStatus;
    @Mock
    private QualityNotificationDTO qualityNotificationDTO;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        systemStatus=new SystemStatus();
    }

    @Test
    public void testSetMethod(){
    systemStatus.setActiveIndicator("Active");
    systemStatus.setCode("0101");
    systemStatus.setDescription("Desc");
    }

    @Test
    public void testGetMethod(){
    systemStatus.getActiveIndicator();
    systemStatus.getCode();
    systemStatus.getDescription();
    }

    @Test
    public void testToString(){
        systemStatus.toString();
    }
}
