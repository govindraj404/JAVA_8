package com.sap.ic.cmh.claim.model.binary_relation;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class DocumentFlowTest {
    @InjectMocks
    DocumentFlow documentFlow;
    @Mock
    BinaryRelationObject binaryRelationObject;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);}

    @Test
    public void setMethodTest(){
        documentFlow.setObjectA(binaryRelationObject);
        documentFlow.setObjectANumber("test");
        documentFlow.setObjectB(binaryRelationObject);
        documentFlow.setObjectBNumber("test");
        documentFlow.setObjectAType("test");
        documentFlow.setSourceLogicalSystem("test");
        documentFlow.setObjectBType("test");
        documentFlow.setTargetLogicalSystem("test");

    }
    @Test
    public void getMethodTest(){
        documentFlow.getObjectA();
        documentFlow.getObjectANumber();
        documentFlow.getObjectB();
        documentFlow.getObjectBNumber();
        documentFlow.getObjectAType();
        documentFlow.getSourceLogicalSystem();
        documentFlow.getObjectBType();
        documentFlow.getTargetLogicalSystem();
    }

    }


