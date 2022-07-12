package com.sap.ic.cmh.configuration.action.handler;

import cds.gen.configurationservice.ComplaintTypeToItemCategoryMappings;
import cds.gen.configurationservice.ComplaintTypeToSalesAreaMappings;
import cds.gen.configurationservice.CopyComplaintTypeToItemCategoryMappingsContext;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.draft.DraftService;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.configuration.persistency.ComplaintTypeToItemCatMappingDao;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class CopyComplaintTypeToItemCatMappingHandlerTest {

    @InjectMocks
    CopyComplaintTypeToItemCatMappingHandler handler;

    @Mock
    CopyComplaintTypeToItemCategoryMappingsContext context;

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

    @Mock
    ComplaintTypeToItemCatMappingDao db;
    private ComplaintTypeToItemCategoryMappings complaintType;
    private ComplaintTypeToSalesAreaMappings salesAreaDestinationSystemMaps;
    private ComplaintTypeToItemCategoryMappings salesAreaItemCategoryMaps;
    List<ComplaintTypeToSalesAreaMappings> salesAreaList = new ArrayList<>();
    List<ComplaintTypeToItemCategoryMappings> salesAreaItemCategoryMapsList = new ArrayList<>();
    Stream<ComplaintTypeToSalesAreaMappings> salesAreaStream;

    @Before
    public void setBefore() {
        MockitoAnnotations.openMocks(this);
        complaintType = Struct.create(ComplaintTypeToItemCategoryMappings.class);
        salesAreaDestinationSystemMaps = Struct.create(ComplaintTypeToSalesAreaMappings.class);
        salesAreaItemCategoryMaps = Struct.create(ComplaintTypeToItemCategoryMappings.class);
        complaintType.setComplaintTypeId("1234");
        complaintType.setItemCategoryId("1245");
        complaintType.setId("1245");
        complaintType.setDivisionId("127");
        salesAreaItemCategoryMaps.setItemCategoryId("123456");
        salesAreaItemCategoryMaps.setComplaintTypeId("4567");
        salesAreaItemCategoryMapsList.add(salesAreaItemCategoryMaps);
        salesAreaDestinationSystemMaps.setId("789");
        salesAreaList.add(salesAreaDestinationSystemMaps);
        salesAreaStream = salesAreaList.stream();
    }

    @Test
    public void copyComplaintTypeToItemCategoryMappingTest(){
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(ComplaintTypeToItemCategoryMappings.class)).thenReturn(complaintType);
        when(db. getComplaintTypeToItemCatMappingBasedOnUniqueFields(complaintType)).thenReturn(result);
        when(result.rowCount()).thenReturn((long) 1);
        when(messages.success(any(String.class))).thenReturn(message);
        when(result.streamOf(ComplaintTypeToSalesAreaMappings.class)).thenReturn(salesAreaStream);
        when(result.rowCount()).thenReturn((long) 1);
        when(draftService.newDraft(any(CqnInsert.class))).thenReturn(result);
        handler.copyComplaintTypeToItemCategoryMapping(context);
    }

}
