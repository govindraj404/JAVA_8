package com.sap.ic.cmh.businessobjects.persistency;

import cds.gen.com.sap.ic.cmh.qualitynotificationstatusmapping.QualityNotificationStatusMappings;
import cds.gen.com.sap.ic.cmh.supplierissueprocessstatusmapping.SupplierIssueProcessStatusMappings;
import cds.gen.complaintservice.BusinessObjectStatuses;
import cds.gen.complaintservice.BusinessObjects;
import cds.gen.complaintservice.CommonBusinessObjects;
import cds.gen.qualitynotificationservice.BusinessObjectRelations;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.persistence.PersistenceService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class BusinessObjectDaoTest {
    @InjectMocks
    @Autowired
    public BusinessObjectDao dao;
    @Mock
    public PersistenceService db;
    @Mock
    Result result;
    @Mock
    CdsCreateEventContext createContextMock;
    @Mock
    CqnInsert cqnInsert;
    private Row row;
    private Optional<Row> opt;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        row = Struct.create(Row.class);

    }

    @Test
    public void testUpdateBusinessObjects() {
        BusinessObjects businessObjects = Struct.create(BusinessObjects.class);
        dao.updateBusinessObject(businessObjects, "test", "test");
    }

    @Test
    public void testFindClaimStatusMappingbyCode() {
        dao.findClaimStatusMappingbyCode("test");
    }

    @Test
    public void testFindQNStatusMappingbyCode() {
        dao.findQNStatusMappingbyCode("test");
    }

    @Test
    public void testFindEightDStatusMappingbyCode() {
        dao.findEightDStatusMappingbyCode("test","CONF");
    }

    @Test
    public void testFindReturnPOStatusMappingbyCode() {
        dao.findReturnPOStatusMappingbyCode("test");
    }

    @Test
    public void testInsertBusinessObjectStatuses() {
        BusinessObjectStatuses businessObjects = Struct.create(BusinessObjectStatuses.class);
        dao.insertBusinessObjectStatuses(businessObjects);
    }
    @Test
    public void testFindBOStatusBasedOnBOId() {
        dao.findBOStatusBasedOnBOId("test");
    }
    @Test
    public void testInsertBusinessObjectStatusesActive() {
        cds.gen.managecomplaintservice.BusinessObjectStatuses businessObjects = Struct.create(cds.gen.managecomplaintservice.BusinessObjectStatuses.class);
        dao.insertBusinessObjectStatusesActive(businessObjects);
    }

    @Test
    public void testGetBusinessObjectsBasedOnBusinessObjectId() {
        BusinessObjectStatuses businessObjects = Struct.create(BusinessObjectStatuses.class);
        dao.getBusinessObjectsBasedOnBusinessObjectId("businessObjects");
    }

    @Test
    public void testGetCurrentBOStatus() {
        BusinessObjectStatuses businessObjects = Struct.create(BusinessObjectStatuses.class);
        dao.getCurrentBOStatus("businessObjects");
    }

    @Test
    public void testGetCurrentActiveBOStatus() {
        BusinessObjectStatuses businessObjects = Struct.create(BusinessObjectStatuses.class);
        dao.getCurrentActiveBOStatus("businessObjects");
    }

    @Test
    public void testGetBusinessObjectStatusBasedOnType() {
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("businessObjectID_ID", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        dao.getBusinessObjectStatusBasedOnType("businessObjects", "49");
    }

    @Test
    public void testGetBusinessObjectStatusBasedOnTypeForNullValues() {
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("businessObjectID_ID", null);
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        dao.getBusinessObjectStatusBasedOnType("businessObjects", "49");
    }


    @Test
    public void testGetActiveBusinessObjectStatusBasedOnType() {
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("businessObjectID_ID", "1234");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        dao.getActiveBusinessObjectStatusBasedOnType("businessObjects", "49");
    }

    @Test
    public void testGetActiveBusinessObjectStatusBasedOnTypeNull() {
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("businessObjectID_ID", null);
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        dao.getActiveBusinessObjectStatusBasedOnType("businessObjects", "49");
    }

    @Test
    public void testFetchStreamTypeByBOType() {
        BusinessObjectStatuses businessObjects = Struct.create(BusinessObjectStatuses.class);
        dao.fetchStreamTypeByBOType("businessObjects");
    }

    @Test
    public void testInsertBusinessObjectRelations() {
        BusinessObjectRelations businessObjects = Struct.create(BusinessObjectRelations.class);
        dao.insertBusinessObjectRelations(businessObjects);
    }

    @Test
    public void testCheckIfCreatedStatusExistsActive() {
        cds.gen.managecomplaintservice.BusinessObjectStatuses businessObjects = Struct.create(cds.gen.managecomplaintservice.BusinessObjectStatuses.class);
        Optional<cds.gen.managecomplaintservice.BusinessObjectStatuses> emptyOpt = Optional.of(businessObjects);
        when(db.run(any(CqnSelect.class))).thenReturn(result);

    }

    @Test
    public void testCheckIfCreatedStatusExists() {
        BusinessObjectStatuses businessObjects = Struct.create(BusinessObjectStatuses.class);
        Optional<BusinessObjectStatuses> emptyOpt = Optional.of(businessObjects);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(BusinessObjectStatuses.class)).thenReturn(emptyOpt);
        dao.checkIfCreatedStatusExistsActive("QNCRTD", "1111");
    }

    @Test
    public void testCheckIfBOIsRelevant() {
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        dao.checkIfBOIsRelevant("1234", "boType");
    }

    @Test
    public void testCheckIfCreatedStatusExists1() {
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        dao.checkIfCreatedStatusExists("code", "1234");
    }

    @Test
    public void testGetAllReturnPOStatusMappings() {
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        dao.getAllReturnPOStatusMappings();
    }

    @Test
    public void testFindBOStatusCode() {
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        dao.findBOStatusCode("1234","F767");
    }
    
    @Test
    public void testGetCurrentBOStatusSupplier8D() {
       
        dao.getCurrentBOStatusSupplier8D("businessObjects");
    }

    @Test
    public void testGetCurrentActiveBOStatusSupplier8D() {
        
        dao.getCurrentActiveBOStatusSupplier8D("businessObjects");
    }
    @Test
    public void getAllSupplierIssueProcessMappingsTest() {
        SupplierIssueProcessStatusMappings businessObjects = Struct.create(SupplierIssueProcessStatusMappings.class);
        Optional<SupplierIssueProcessStatusMappings> emptyOpt = Optional.of(businessObjects);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(SupplierIssueProcessStatusMappings.class)).thenReturn(emptyOpt);
        dao.getAllSupplierIssueProcessMappings();
    }
    @Test
    public void getCommonBusinessObjectsBasedOnComplaintTest() {
        CommonBusinessObjects businessObjects = Struct.create(CommonBusinessObjects.class);
        Optional<CommonBusinessObjects> emptyOpt = Optional.of(businessObjects);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(CommonBusinessObjects.class)).thenReturn(emptyOpt);
        dao.getCommonBusinessObjectsBasedOnComplaint("12");
    }
    @Test
    public void getCommonBusinessObjectsBasedOnBusinessPartnerTest() {
        CommonBusinessObjects businessObjects = Struct.create(CommonBusinessObjects.class);
        Optional<CommonBusinessObjects> emptyOpt = Optional.of(businessObjects);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(CommonBusinessObjects.class)).thenReturn(emptyOpt);
        dao.getCommonBusinessObjectsBasedOnBusinessPartner("213");

    }

    @Test
    public void testGetBusinessObjectStatusBasedOnTypeExp() {
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        //row.put("businessObjectID_ID", null);
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        dao.getBusinessObjectStatusBasedOnType("businessObjects", "49");
    }
    @Test(expected = Exception.class)
    public void testGetBusinessObjectStatusBasedOnTypeNullExp() {
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        //row.put("businessObjectID_ID", null);
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(null);
        dao.getBusinessObjectStatusBasedOnType("businessObjects", "49");
    }

    @Test
    public void testGetActiveBusinessObjectStatusBasedOnTypeEmpty() {
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        //row.put("businessObjectID_ID", "1234");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        dao.getActiveBusinessObjectStatusBasedOnType("businessObjects", "49");
    }

    @Test(expected = Exception.class)
    public void testGetActiveBusinessObjectStatusBasedOnTypeEMptyExp() {
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
          opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(null);
        dao.getActiveBusinessObjectStatusBasedOnType("businessObjects", "49");
    }
    
    @Test
    public void getAllQualityNotificationStatusMappingsTest() {
    	QualityNotificationStatusMappings businessObjects = Struct.create(QualityNotificationStatusMappings.class);
        Optional<QualityNotificationStatusMappings> emptyOpt = Optional.of(businessObjects);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(QualityNotificationStatusMappings.class)).thenReturn(emptyOpt);
        dao.getAllQualityNotificationStatusMappings();
    }

    @Test
    public void testGetBusinessObjectIdBasedOnTypeAndComplaint() {
    	BusinessObjects bo = Struct.create(BusinessObjects.class);
    	bo.setComplaint("1");
    	bo.setBusinessObjectTypeCode("QN");
    	bo.setBusinessObjectIDId("2");
    	dao.getBusinessObjectIdBasedOnTypeAndComplaint("1","QN");
    }
}
