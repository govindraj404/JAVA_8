package com.sap.ic.cmh.returnpo.model;

import java.util.HashMap;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import com.sap.ic.cmh.claim.model.binary_relation.BinaryRelationDataModel;
import com.sap.ic.cmh.claim.model.binary_relation.BinaryRelationObject;
import com.sap.ic.cmh.utils.Constants;

import cds.gen.returnpurchaseorderservice.ReturnPurchaseOrders;

public class ReturnOrderTest {

    @InjectMocks
    ReturnOrder returnOrder;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }
    @Test
    public void setMethodTest(){
    	Map<String, Object> returnOrdersMap = new HashMap<>();
        returnOrdersMap.put(ReturnPurchaseOrders.COMPANY_ID, "1000");
        returnOrdersMap.put(ReturnPurchaseOrders.PURCHASING_GROUP_CODE,"001");
        returnOrder.setReturnOrders(returnOrdersMap);
        BinaryRelationDataModel binaryRelationDataModel = new BinaryRelationDataModel();
		binaryRelationDataModel.setObjectA(new BinaryRelationObject());
		binaryRelationDataModel.setObjectB(new BinaryRelationObject());
		binaryRelationDataModel.setRelationType(Constants.BINARY_RELATIONSHIP_TYPE);
		returnOrder.setBinaryRelationDataModel(binaryRelationDataModel);
       
    }

    @Test
    public void getMethodTest(){
        returnOrder.getReturnOrders();
        returnOrder.getBinaryRelationDataModel();
    }
}
