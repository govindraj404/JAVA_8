package com.sap.ic.cmh.masterdata.purchasinggroup.service;

import cds.gen.masterdataservice.PurchasingGroups;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.masterdata.purchasinggroup.repository.PurchasingGroupRepository;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.*;

import static org.mockito.Mockito.when;

public class purchasingGroupServiceTest {

    @InjectMocks
    PurchasingGroupServiceImpl purchasingGroupService;

    @Mock
     PurchasingGroupService purchasingGroupServices;
    @Mock
    PersistenceService db;
    @Mock
    Result result;
    @Mock
    PurchasingGroupRepository purchasingGroupRepository;

    private Row row;
    private Optional<Row> opt;


    private List<PurchasingGroups> purchasingGroupsList = new ArrayList<>();
    private PurchasingGroups purchasingGroups;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        purchasingGroups = Struct.create(PurchasingGroups.class);
        purchasingGroups.setCode("code");
        purchasingGroups.setDescription("Description");
    }


    @Test
    public void testFetchPurchasingGroupDetails() {
        Map<String, String> purchasingGroupMap = new HashMap<>();
        purchasingGroupMap.put("200001", "Australia");
        when(result.listOf(PurchasingGroups.class)).thenReturn(purchasingGroupsList);
        when(purchasingGroupRepository.fetchPurchasingGroupDetails(purchasingGroups.getCode())).thenReturn(result);
        purchasingGroupService.fetchPurchasingGroupDetails(purchasingGroups.getCode());

    }

    @Test
    public void testFetchPurchasingGroupDetailsNull() {
        when(purchasingGroupRepository.fetchPurchasingGroupDetails(purchasingGroups.getCode())).thenReturn(result);
        purchasingGroupService.fetchPurchasingGroupDetails(purchasingGroups.getCode());	}
}
