package com.sap.ic.cmh.businessobjects.service;

import cds.gen.complaintservice.BusinessObjects;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.businessobjects.persistency.BusinessObjectDao;
import com.sap.ic.cmh.utils.CommonFunctions;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.*;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class BusinessObjectServiceImplTest {
    @InjectMocks
    @Autowired
    public BusinessObjectServiceImpl impl;
    @Mock
    CommonFunctions commonFunctions;
    @Mock
    BusinessObjectDao businessObjectDao;
    @Mock
    Result result;
    @Mock
    CdsCreateEventContext createContextMock;
    @Mock
    CqnInsert cqnInsert;
    @Mock
    public PersistenceService db;
    private Row row;
    private Optional<Row> opt;
    private BusinessObjects objects;
    private List<BusinessObjects> boList= new ArrayList<>();

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        row = Struct.create(Row.class);

        objects =  Struct.create(BusinessObjects.class);
        objects.setId("1234");

        boList.add(objects);
    }

    @Test
    public void testUpdateBusinessObjects() {
        Map<String, Object> map=new HashMap<>();
        map.put("ID","12");
        map.put("complaint_ID","12");
        when(commonFunctions.convertObjectToMap(any())).thenReturn(map);
        impl.updateBusinessObjects("test","test");
    }
    @Test
    public void testGetBusinessObjectIdBasedOnTypeAndComplaint() {
        when(businessObjectDao.getBusinessObjectIdBasedOnTypeAndComplaint(any(String.class), any(String.class))).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("businessObjectID_ID", "12");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.first()).thenReturn(opt);
        when(result.list()).thenReturn(rowvalues);
        impl.getBusinessObjectIdBasedOnTypeAndComplaint("","qn");
    }
    @Test
    public void testUpdateBusinessObject() {
        Map<String, Object> map=new HashMap<>();
        map.put("ID","12");
        map.put("complaint_ID","12");
        impl.updateBusinessObject("test",map);
    }

    @Test
    public void testSetBusinessObjectStatusCLM() {
        when(businessObjectDao.findClaimStatusMappingbyCode(any(String.class))).thenReturn(result);
        impl.setBusinessObjectStatus("CLM","200","20",true);
    }
    @Test
    public void testSetBusinessObjectStatusCLMResultNotNull() {
        when(businessObjectDao.findClaimStatusMappingbyCode(any(String.class))).thenReturn(result);
        when(businessObjectDao.checkIfCreatedStatusExistsActive("CLMCRTD", "20")).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("status_code", "D4556");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.first()).thenReturn(opt);
        when(result.list()).thenReturn(rowvalues);
        impl.setBusinessObjectStatus("CLM","200","20",true);
    }
    @Test
    public void testSetBusinessObjectStatusQN() {
        when(businessObjectDao.findQNStatusMappingbyCode(any(String.class))).thenReturn(result);
        impl.setBusinessObjectStatus("QN","200","20",true);
    }

    @Test
    public void testSetBusinessObjectStatusQNResultNotNull() {
        when(businessObjectDao.findQNStatusMappingbyCode(any(String.class))).thenReturn(result);
        when(businessObjectDao.checkIfCreatedStatusExistsActive("QNCRTD", "20")).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("status_code", "D4556");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.first()).thenReturn(opt);
        when(result.list()).thenReturn(rowvalues);
        impl.setBusinessObjectStatus("QN","200","20",true);
    }

    @Test
    public void testSetBusinessObjectStatusS8D() {
        when(businessObjectDao.findEightDStatusMappingbyCode("200","LC")).thenReturn(result);
        impl.setBusinessObjectStatus("S8D","200","20",true);
    }

    @Test
    public void testSetBusinessObjectStatusRPO() {
        when(businessObjectDao.findReturnPOStatusMappingbyCode(any(String.class))).thenReturn(result);
        impl.setBusinessObjectStatus("RPO","200","20",true);
    }

    @Test
    public void testSetBusinessObjectStatusS8DResultNotNull() {
        when(businessObjectDao.findEightDStatusMappingbyCode("200","LC")).thenReturn(result);
        when(businessObjectDao.checkIfCreatedStatusExistsActive("S8DCRTD", "20")).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("status_code", "D4556");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.first()).thenReturn(opt);
        when(result.list()).thenReturn(rowvalues);
        impl.setBusinessObjectStatus("S8D","200","20",true);
    }

    @Test
    public void testSetBusinessObjectStatusRPOResultNotNull() {
        when(businessObjectDao.findReturnPOStatusMappingbyCode(any(String.class))).thenReturn(result);
        when(businessObjectDao.checkIfCreatedStatusExistsActive("RPOCRTD", "20")).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("status_code", "200");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.first()).thenReturn(opt);
        when(result.list()).thenReturn(rowvalues);
        impl.setBusinessObjectStatus("RPO","200","20",true);
    }

    @Test
    public void testInsertBusinessObjectStatus() {
        when(businessObjectDao.findReturnPOStatusMappingbyCode(any(String.class))).thenReturn(result);
        impl.insertBusinessObjectStatus("RPO","200","20",true);
    }

    @Test
    public void testInsertBusinessObjectStatusNull() {
        when(businessObjectDao.findReturnPOStatusMappingbyCode(any(String.class))).thenReturn(result);
        impl.insertBusinessObjectStatus("RPO","200","",true);
    }

    @Test
    public void testGetBusinessObjectsBasedOnBusinessObjectId() {
        when(businessObjectDao.getBusinessObjectsBasedOnBusinessObjectId(any(String.class))).thenReturn(result);
        impl.getBusinessObjectsBasedOnBusinessObjectId("7");
    }

    @Test
    public void testGetBusinessObjectsBasedOnBusinessObjectIdResultNotNull() {
        when(businessObjectDao.getBusinessObjectsBasedOnBusinessObjectId(any(String.class))).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("businessObjectStatus_code", "D4556");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.first()).thenReturn(opt);
        when(result.listOf(BusinessObjects.class)).thenReturn(boList);
        impl.getBusinessObjectsBasedOnBusinessObjectId("7");
    }

    @Test
    public void testGetCurrentBOStatus() {
        when(businessObjectDao.getCurrentBOStatus(any(String.class))).thenReturn(result);
        impl.getCurrentBOStatus("7");
    }

    @Test
    public void testGetCurrentBOStatusResultNotNull() {
        when(businessObjectDao.getCurrentBOStatus(any(String.class))).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("businessObjectStatus_code", "D4556");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.first()).thenReturn(opt);
        when(result.list()).thenReturn(rowvalues);
        impl.getCurrentBOStatus("7");
    }

    @Test
    public void testFetchStreamTypeByBOType() {
        when(businessObjectDao.fetchStreamTypeByBOType(any(String.class))).thenReturn(result);
        impl.fetchStreamTypeByBOType("7");
    }

    @Test
    public void testFetchStreamTypeByBOTypeResultNotNull() {
        when(businessObjectDao.fetchStreamTypeByBOType(any(String.class))).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("streamType_code", "D4556");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.first()).thenReturn(opt);
        when(result.list()).thenReturn(rowvalues);
        impl.fetchStreamTypeByBOType("7");
    }

    @Test
    public void testGetBusinessObjectStatusBasedOnType() {
        when(businessObjectDao.getBusinessObjectStatusBasedOnType(any(String.class),any(String.class))).thenReturn(result);
        impl.getBusinessObjectStatusBasedOnType("7","CO");
    }
    @Test
    public void testGetBusinessObjectStatusBasedOnTypeResultNotNull() {
        when(businessObjectDao.getBusinessObjectStatusBasedOnType(any(String.class),any(String.class))).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("businessObjectStatus_code", "D4556");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.first()).thenReturn(opt);
        when(result.list()).thenReturn(rowvalues);
        impl.getBusinessObjectStatusBasedOnType("7","CO");
    }

    @Test
    public void testInsertBusinessObjectRelations() {
        when(businessObjectDao.getBusinessObjectStatusBasedOnType(any(String.class),any(String.class))).thenReturn(result);
        impl.insertBusinessObjectRelations("7","CO","test","3");
    }

    @Test
    public void testGetCurrentActiveBOStatus() {
        when(businessObjectDao.getCurrentActiveBOStatus(any(String.class))).thenReturn(result);
        impl.getCurrentActiveBOStatus("7");
    }

    @Test
    public void testGetCurrentActiveBOStatusResultNotNull() {
        when(businessObjectDao.getCurrentActiveBOStatus(any(String.class))).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("businessObjectStatus_code", "D4556");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.first()).thenReturn(opt);
        when(result.list()).thenReturn(rowvalues);
        impl.getCurrentActiveBOStatus("7");
    }

    @Test
    public void testCheckIfCreatedStatusExistsTrue() {
        when(businessObjectDao.checkIfCreatedStatusExistsActive("200", "12")).thenReturn(result);
        impl.checkIfCreatedStatusExists("200","boType", "12",true);
    }

    @Test
    public void testCheckIfCreatedStatusExists1False() {
        when(businessObjectDao.checkIfCreatedStatusExists("200", "12")).thenReturn(result);
        impl.checkIfCreatedStatusExists("200","boType", "12",false);
    }

    @Test
    public void testCheckIfBOIsRelevant() {
        when(businessObjectDao.checkIfBOIsRelevant("1234","boType")).thenReturn(result);
        impl.checkIfBOIsRelevant("1234", "boType");
    }

    @Test
    public void testCheckIfBOIsRelevantResultNotNull() {
        when(businessObjectDao.checkIfBOIsRelevant("1234","boType")).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("isRelevant", true);
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.first()).thenReturn(opt);
        when(result.list()).thenReturn(rowvalues);
        impl.checkIfBOIsRelevant("1234", "boType");
    }

    @Test
    public void testCheckIfBOStatusExists() {
        when(businessObjectDao.findBOStatusCode("1234", "NEW")).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("isRelevant", true);
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.first()).thenReturn(opt);
        when(result.list()).thenReturn(rowvalues);
        impl.checkIfBOStatusExists("1234", "NEW");
    }
    
    @Test
    public void testGetCurrentActiveBOStatusSupplier8D() {
        when(businessObjectDao.getCurrentActiveBOStatusSupplier8D(any(String.class))).thenReturn(result);
        impl.getCurrentActiveBOStatusSupplier8D("7");
    }

    @Test
    public void testGetCurrentActiveBOStatusSupplier8DResultNotNull() {
        when(businessObjectDao.getCurrentActiveBOStatusSupplier8D(any(String.class))).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("businessObjectStatus_code", "S8DINPR");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.first()).thenReturn(opt);
        when(result.list()).thenReturn(rowvalues);
        impl.getCurrentActiveBOStatusSupplier8D("7");
    }
    
    @Test
    public void testGetCurrentBOStatusSupplier8D() {
        when(businessObjectDao.getCurrentBOStatusSupplier8D(any(String.class))).thenReturn(result);
        impl.getCurrentBOStatusSupplier8D("7");
    }

    @Test
    public void testGetCurrentBOStatusSupplier8DResultNotNull() {
        when(businessObjectDao.getCurrentBOStatusSupplier8D(any(String.class))).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("businessObjectStatus_code", "S8DINPR");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.first()).thenReturn(opt);
        when(result.list()).thenReturn(rowvalues);
        impl.getCurrentBOStatusSupplier8D("7");
    }
    @Test
    public void validateIfStatusIsCreatedTestNull(){
        impl.validateIfStatusIsCreated("","23",true,"test","test");

    }

    @Test
    public void testGetBusinessObjectStatusBasedOnTypeElse() {
        when(businessObjectDao.getBusinessObjectStatusBasedOnType(any(String.class),any(String.class))).thenReturn(result);
        impl.getBusinessObjectStatusBasedOnType("7","CO");
    }
}
