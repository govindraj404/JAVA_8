package com.sap.ic.cmh.configuration.persistency;

import cds.gen.configurationservice.BusinessObjectConfigurations;
import cds.gen.configurationservice.BusinessObjectConfigurations_;
import cds.gen.qualitynotificationservice.QualityNotifications;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnDelete;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.persistence.PersistenceService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class BusinessObjectConfigurationDaoTest {
    @InjectMocks
    BusinessObjectConfigurationDao dao;
    @Mock
    PersistenceService db;
    @Mock
    Result result;
    @Mock
    Runnable run;
    @Mock
    CdsCreateEventContext createContextMock;
    @Mock
    CqnInsert cqnInsert;
    BusinessObjectConfigurations businessObjectConfigurations;

    private List<BusinessObjectConfigurations_> boList = new ArrayList<>();
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        businessObjectConfigurations =  Struct.create(BusinessObjectConfigurations.class);
        businessObjectConfigurations.setBusinessObjectAttributeCode("100");
        businessObjectConfigurations.setDestination("100");
        businessObjectConfigurations.setBusinessObjectTypeCode("120");
        businessObjectConfigurations.setComplaintTypeCode("1");
        businessObjectConfigurations.setBusinessObjectValue("2");
        businessObjectConfigurations.setId("100");
        when(db.run(any(CqnDelete.class))).thenReturn(result);
        when(createContextMock.getCqn()).thenReturn(cqnInsert);
        List<Map<String, Object>> entries = new ArrayList<>();
        when(cqnInsert.entries()).thenReturn(entries);
    }

    @Test
    public void getBusinessObjectConfigurationsTest(){
        dao.getBusinessObjectConfigurations();
    }

    @Test
    public void getBusinessObjectConfigBasedOnDestinationAndBOTest(){
        dao.getBusinessObjectConfigBasedOnDestinationAndBO(businessObjectConfigurations.getComplaintTypeCode(),businessObjectConfigurations
                .getDestination(),businessObjectConfigurations.getBusinessObjectAttributeCode(),businessObjectConfigurations.getBusinessObjectTypeCode());
    }
    @Test
    public void getBusinessObjectConfigBasedOnDestinationAndBOAndDestTest(){
        dao.getBusinessObjectConfigBasedOnDestinationAndBOAndDest(businessObjectConfigurations.getComplaintTypeCode(),businessObjectConfigurations
                .getDestination(),businessObjectConfigurations.getBusinessObjectTypeCode());
    }
    @Test
    public void testGetBusinessObjectConfigurationsDetails() {
        Mockito.when(db.run(any(CqnSelect.class))).thenReturn(result);
        Mockito.when(result.listOf(BusinessObjectConfigurations_.class)).thenReturn(boList);
        dao.getBusinessObjectConfigurationsDetails(businessObjectConfigurations.getId());
    }
}
