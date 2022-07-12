package com.sap.ic.cmh.metering.controller;

import com.sap.ic.cmh.metering.service.MeteringService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class MeteringControllerTest {

    @InjectMocks
    MeteringController client;
    @Mock
    MeteringService meteringService;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void sendMeteringDataTest() {
        client.sendMeteringData();
    }
}
