package com.sap.ic.cmh.masterdata.purchaseorganization.service;

import cds.gen.masterdataservice.PurchaseOrganizations;
import cds.gen.masterdataservice.PurchasingGroups;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.ic.cmh.masterdata.purchaseorganization.model.PurchaseOrganizationRequest;
import com.sap.ic.cmh.masterdata.purchaseorganization.model.PurchaseOrganizationResponse;
import com.sap.ic.cmh.masterdata.purchaseorganization.repository.PurchaseOrganizationRepository;
import com.sap.ic.cmh.utils.LocaleMessageHelper;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.*;
import java.util.stream.Collectors;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

public class PurchaseOrganizationServiceImplTest {

    @InjectMocks
    PurchaseOrganizationServiceImpl purchaseOrganizationServiceImpl;

    @Mock
    LocaleMessageHelper messageHelper;
    @Mock
    Result result;

    @Mock
    private PurchaseOrganizationRepository purchaseOrganizationRepository;
    private PurchaseOrganizations purchaseOrganizations;
    private List<PurchaseOrganizations> purchaseOrganizationLists;
    private List<PurchaseOrganizationRequest> purchaseOrganizationDataRequestList;
    private List<String> purchaseOrganizationList;

    @Before
    public void beforeClass() throws JsonProcessingException {
        MockitoAnnotations.openMocks(this);
        String json = "[{\"purchaseOrganization\":\"101\"},{\"purchaseOrganization\":\"02\"}]";
        purchaseOrganizationDataRequestList =  new ObjectMapper().readValue(json, new TypeReference<List<PurchaseOrganizationRequest>>() {
        });
        purchaseOrganizationList = purchaseOrganizationDataRequestList.stream().map(PurchaseOrganizationRequest::getPurchaseOrganization).collect(Collectors.toList());
        purchaseOrganizations = Struct.create(PurchaseOrganizations.class);

        purchaseOrganizations.setPurchaseOrganization("SO10");
        purchaseOrganizations.setPurchaseOrganizationName("Capgemini Auto sap");
        purchaseOrganizations.setCompanyCode("BP01");


    }

    @Test
    public void testDeletePurchaseOrganizationList() {
        Map<String, String> purchaseOrganizationWithIdMap = new LinkedHashMap<>();
        purchaseOrganizationWithIdMap.put("01", "4052c63c-6351-4ea5-8192-4af6686bb526");
        purchaseOrganizationWithIdMap.put("21", "4052c63c-6351-4ea5-8192-4af6686bb527");
        Mockito.when(purchaseOrganizationRepository.getPurchaseOrganizationMap(purchaseOrganizationList)).thenReturn(purchaseOrganizationWithIdMap);
        final List<PurchaseOrganizationResponse> purchaseOrganizationResponses = purchaseOrganizationServiceImpl.deletePurchaseOrganizationList(purchaseOrganizationDataRequestList);
        Assert.assertEquals("101", purchaseOrganizationResponses.get(0).getPurchaseOrganization());
        Assert.assertNull(purchaseOrganizationResponses.get(0).getStatus());
        Assert.assertEquals("1", purchaseOrganizationResponses.get(0).getRecordNo());
        Assert.assertNull( purchaseOrganizationResponses.get(0).getMessage());

    }

    @Test
    public void testDeletePurchaseOrganizationListElse() {
        Map<String, String> purchaseOrganizationWithIdMap = new LinkedHashMap<>();
        purchaseOrganizationWithIdMap.put("01", "4052c63c-6351-4ea5-8192-4af6686bb526");
        purchaseOrganizationWithIdMap.put("21", "4052c63c-6351-4ea5-8192-4af6686bb527");
        Mockito.when(purchaseOrganizationRepository.getPurchaseOrganizationMap(purchaseOrganizationList)).thenReturn(purchaseOrganizationWithIdMap);
        List<String> list=new ArrayList<>();
        list.add("01");
        when(purchaseOrganizationRepository.getActiveComplaintsInPurchaseOrganization(any(List.class))).thenReturn(list);
         purchaseOrganizationServiceImpl.deletePurchaseOrganizationList(purchaseOrganizationDataRequestList);


    }

    @Test
    public void testFetchSubItemTypesNull() {
        when(purchaseOrganizationRepository.fetchPurchaseOrganization(purchaseOrganizations.getPurchaseOrganization())).thenReturn(result);
        purchaseOrganizationServiceImpl.fetchPurchaseOrganization(purchaseOrganizations.getPurchaseOrganization());
    }


}
