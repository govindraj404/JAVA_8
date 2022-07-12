package com.sap.ic.cmh.metering.model;

import com.sap.ic.cmh.utils.Constants;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

public class UsageDocumentTest {
    @InjectMocks
    UsageDocument usageDocument;

    Measure measure = new Measure();
    Consumer consumer = new Consumer();
    Service service = new Service();

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void setMethodTest() {
        List<Measure> measures = new ArrayList<>();

        measure.setId(Constants.MEASURE_ID);
        measure.setValue(100L);
        measures.add(measure);

        consumer.setEnvironment(Constants.CONSUMER_ENVIRONMENT);
        consumer.setEnvironment("e");
        consumer.setRegion("");
        consumer.setSubAccount("");
        consumer.setIdentityZone("");

        service.setId(Constants.SERVICE_ID);
        service.setPlan(Constants.SERVICE_PLAN);

        usageDocument.setTimestamp(new SimpleDateFormat(Constants.METERING_TIMESTAMP).format(new Date()));
        usageDocument.setId("12");
        usageDocument.setConsumer(consumer);
        usageDocument.setService(service);
        usageDocument.setMeasures(measures);
    }

    @Test
    public void getMethodTest() {
        measure.getId();
        measure.getValue();

        consumer.getEnvironment();
        consumer.getRegion();
        consumer.getSubAccount();
        consumer.getIdentityZone();

        service.getId();
        service.getPlan();

        usageDocument.getTimestamp();
        usageDocument.getConsumer();
        usageDocument.getService();
        usageDocument.getId();
        usageDocument.getMeasures();
    }
}
