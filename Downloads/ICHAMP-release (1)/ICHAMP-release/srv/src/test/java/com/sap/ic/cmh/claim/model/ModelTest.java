package com.sap.ic.cmh.claim.model;

import com.sap.cds.Struct;
import com.sap.ic.cmh.claim.model.binary_relation.BinaryRelationDataModel;
import com.sap.ic.cmh.claim.model.binary_relation.BinaryRelationObject;

import cds.gen.costcollectorservice.CostCollectors;

import org.junit.Before;
import org.junit.Test;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ModelTest {


    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void testSetterMethod () {
        ClaimDetails claimDetails = new ClaimDetails();
        claimDetails.setClaimNumber("1344");
        claimDetails.setClaimStatus("NEW");
        ClaimItem claimItem = new ClaimItem();
        claimItem.setMaterial("material");
        claimItem.setDescription("description");
        claimItem.setItemKey("Key");
        claimItem.setAdditionalProperty("", new Object());
        claimItem.setItemType("ItemKey");
        claimItem.setHandle("handle");
        claimItem.setHandleVersion("HandleVersion");
        claimItem.setUnit("unit");
        claimItem.setQuantity("quantity");
        claimItem.setSubItemType("SUnItemType");

        claimItem.getMaterial();
        claimItem.getDescription();
        claimItem.getItemKey();
        claimItem.getAdditionalProperties();
        claimItem.getItemType();
        claimItem.getHandle();
        claimItem.getHandleVersion();
        claimItem.getUnit();
        claimItem.getQuantity();
        claimItem.getSubItemType();
        claimItem.getPartCauseDamage();

        claimDetails .getClaimNumber();
        claimDetails.getClaimStatus();
        List<ClaimItem> claimItemList = new ArrayList<>();
        claimItemList.add(claimItem);

        ClaimPricing claimPricing = new ClaimPricing();
        claimPricing.setCondType("condType");
        claimPricing.setAdditionalProperty("", new Object());
        claimPricing.setTotalCost("1000");
        claimPricing.setCurrency("INR");
        claimPricing.setRefhandle("refhandle");

        claimPricing.getAdditionalProperties();
        claimPricing.getCondType();
        claimPricing.getCurrency();
        claimPricing.getTotalCost();
        claimPricing.getRefhandle();

        List<ClaimPricing> claimPricingList = new ArrayList<>();
        claimPricingList.add(claimPricing);
        
        Map<String,Object> claimMap = new HashMap<>();
        claimMap.put("ID", 1);
        claimMap.put("ID1",2);
        claimMap.put("ID2",3);


        ClaimDTO dto = new ClaimDTO();
        dto.setClaimItem(claimItemList);
        dto.setClaimPricing(claimPricingList);
        dto.setClaims(claimMap);
        dto.getClaimPricing();
        dto.getBinaryRelationDataModel();
        dto.getClaims();
        dto.getClaimItem();

        ResponseModel model = new ResponseModel();
        model.getResult();
        model.getErrorMessage();

        BinaryRelationObject object = new BinaryRelationObject();
        object.setObjectKey("key");
        object.setLogSys("sys");
        object.setObjectType("type");

        object.getObjectKey();
        object.getLogSys();
        object.getObjectType();

        BinaryRelationDataModel bmodel = new BinaryRelationDataModel();
        bmodel.setObjectA(object);
        bmodel.setObjectB(object);
        bmodel.setRelationType("relationType");
        bmodel.getObjectA();
        bmodel.getObjectB();
        bmodel.getRelationType();

    }

    }