package com.sap.ic.cmh.configuration.service;

import cds.gen.configurationservice.ComplaintReasonMappings;

import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;

import com.sap.ic.cmh.configuration.persistency.ComplaintReasonMappingsDao;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class ComplaintReasonMappingsServiceImplTest {

    @InjectMocks
    ComplaintReasonMappingsServiceImpl complaintReasonMappingsService;

    @Mock
    ComplaintReasonMappingsDao complaintReasonMappingsDao;

    @Mock
    Result result;
    @Mock
    public Row row;
    private ComplaintReasonMappings complaintReasonMappings;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        complaintReasonMappings = Struct.create(ComplaintReasonMappings.class);
        complaintReasonMappings.setSalesOrganizationId("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        complaintReasonMappings.setDistributionChannelId("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        complaintReasonMappings.setDivisionId("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        complaintReasonMappings.setItemCategoryId("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        complaintReasonMappings.setComplaintReasonId("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
    }

    @Test
    public void getComplaintReasonMapTest() {
        complaintReasonMappingsService.getComplaintReasonMapIdentifier();
    }
    @Test
    public void getComplaintReasonMappingsDetailsNullTest(){
        when(complaintReasonMappingsDao.getComplaintReasonMapDetailsBasedOnId(
                any(String.class))).thenReturn(result);
        Optional<Row> opt = Optional.empty();
        when(result.first()).thenReturn(opt);
        complaintReasonMappingsService.getComplaintReasonMappingsDetails("ID");
    }
    @Test
    public void getComplaintReasonMappingsDetailsTest(){
        when(complaintReasonMappingsDao.getComplaintReasonMapDetailsBasedOnId(any(String.class))).thenReturn(result);
        row.put("id", "ID");
        Optional<Row> op = Optional.of(row);
        when(result.first()).thenReturn(op);
        ComplaintReasonMappings comp = Struct.create(ComplaintReasonMappings.class);
        comp.setId("ID");
        List<ComplaintReasonMappings> compList = new ArrayList<>();
        compList.add(comp);
        when(result.listOf(ComplaintReasonMappings.class)).thenReturn(compList);
        complaintReasonMappingsService.getComplaintReasonMappingsDetails("ID");
    }
}
