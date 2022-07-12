package com.sap.ic.cmh.configuration.action.handler;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.ArrayList;

import cds.gen.configurationservice.ComplaintTypeToItemCategoryMappings;
import com.sap.cds.services.messages.Message;
import org.junit.Test;

import java.util.List;
import java.util.stream.Stream;

import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.sap.cds.Result;
import org.junit.Before;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.draft.DraftService;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.configuration.persistency.ComplaintTypeToSalesAreaMappingDao;

import cds.gen.configurationservice.ComplaintTypeConfigurations;
import cds.gen.configurationservice.CopyComplaintTypeConfigurationsContext;
import cds.gen.configurationservice.ComplaintTypeToSalesAreaMappings;

public class CopyComplaintTypeConfigurationHandlerTest {

    @InjectMocks
    CopyComplaintTypeConfigurationHandler handler;

    @Mock
    ComplaintTypeToSalesAreaMappingDao salesAreaDestinationSystemMapsDao;

    @Mock
    CopyComplaintTypeConfigurationsContext context;

    @Mock
    CdsService cdsService;
    @Mock
    CqnSelect cqnSelect;
    @Mock
    Result result;
    @Mock
    DraftService draftService;

    @Mock
    Message message;
    @Mock
    Messages messages;

    private ComplaintTypeConfigurations complaintType;
    private ComplaintTypeToSalesAreaMappings salesAreaDestinationSystemMaps;
    private ComplaintTypeToItemCategoryMappings salesAreaItemCategoryMaps;
    List<ComplaintTypeToSalesAreaMappings> salesAreaList = new ArrayList<>();
    List<ComplaintTypeToItemCategoryMappings> salesAreaItemCategoryMapsList = new ArrayList<>();
    Stream<ComplaintTypeToSalesAreaMappings> salesAreaStream;

    @Before
    public void setBefore() {
        MockitoAnnotations.openMocks(this);
        complaintType = Struct.create(ComplaintTypeConfigurations.class);
        salesAreaDestinationSystemMaps = Struct.create(ComplaintTypeToSalesAreaMappings.class);
        salesAreaItemCategoryMaps = Struct.create(ComplaintTypeToItemCategoryMappings.class);
        complaintType.setCode("abc");
        complaintType.setDescription("test");
        complaintType.setId("123");
        complaintType.setIndividualComplaintType(true);
        complaintType.setItemCategoryId("456");
        salesAreaItemCategoryMaps.setItemCategoryId("123456");
        salesAreaItemCategoryMaps.setComplaintTypeId("4567");
        salesAreaItemCategoryMapsList.add(salesAreaItemCategoryMaps);
        salesAreaDestinationSystemMaps.setId("789");
        salesAreaList.add(salesAreaDestinationSystemMaps);
        complaintType.setComplaintTypeToSalesAreaMappings(salesAreaList);
        salesAreaStream = salesAreaList.stream();
    }

    @Test
    public void testCopyComplaintTypeConfiguration() {

        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(ComplaintTypeConfigurations.class)).thenReturn(complaintType);
        when(salesAreaDestinationSystemMapsDao.
                getSalesAreaMappingBasedOnComplaintTypeConfig("123")).thenReturn(result);
        when(result.rowCount()).thenReturn((long) 1);
        when(messages.success(any(String.class))).thenReturn(message);
        when(result.streamOf(ComplaintTypeToSalesAreaMappings.class)).thenReturn(salesAreaStream);
        when(result.rowCount()).thenReturn((long) 1);
        when(draftService.newDraft(any(CqnInsert.class))).thenReturn(result);
        handler.copyComplaintTypeConfiguration(context);
    }

}
