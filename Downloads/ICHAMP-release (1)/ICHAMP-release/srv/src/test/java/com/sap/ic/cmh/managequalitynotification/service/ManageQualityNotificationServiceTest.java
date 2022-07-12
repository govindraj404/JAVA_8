package com.sap.ic.cmh.managequalitynotification.service;


import cds.gen.claimservice.Claims;
import cds.gen.com.sap.ic.cmh.qualitynotificationstatusmapping.QualityNotificationStatusMappings;
import cds.gen.complaintservice.BusinessObjectStatuses_;
import cds.gen.complaintservice.Complaints;
import cds.gen.managequalitynotificationservice.BusinessObjectStatuses;
import cds.gen.managequalitynotificationservice.Defects;
import cds.gen.managequalitynotificationservice.QualityNotifications;
import cds.gen.masterdataservice.*;
import cds.gen.returnpurchaseorderservice.ReturnPurchaseOrders;
import cds.gen.supplierissueprocessservice.Supplier8DProcesses;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;


import com.sap.ic.cmh.businessobjects.persistency.BusinessObjectDao;
import com.sap.ic.cmh.businessobjects.service.BusinessObjectService;
import com.sap.ic.cmh.claim.service.ClaimService;
import com.sap.ic.cmh.complaint.service.ComplaintService;

import com.sap.ic.cmh.configuration.model.MasterData;
import com.sap.ic.cmh.configuration.service.ConfigurationService;
import com.sap.ic.cmh.managecomplaint.service.ManageComplaintService;
import com.sap.ic.cmh.qualitynotification.model.QualityNotificaitonItemDetails;
import com.sap.ic.cmh.qualitynotification.model.QualityNotificationDetails;
import com.sap.ic.cmh.qualitynotification.model.SystemStatus;
import com.sap.ic.cmh.qualitynotification.persistency.QualityNotificationDao;
import com.sap.ic.cmh.returnpo.service.ReturnPurchaseOrderService;
import com.sap.ic.cmh.supplierissueprocess.service.EightDService;
import com.sap.ic.cmh.utils.CommonFunctions;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;


import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;

import static org.mockito.Mockito.when;

public class ManageQualityNotificationServiceTest {

    @InjectMocks
    @Autowired
    ManageQualityNotificationServiceImpl manageQualityNotificationServiceImpl;

    @Mock
    ConfigurationService configService;

    @Mock
    BusinessObjectDao businessObjectDao;

    @Mock
    ConfigurationService configurationService;

    @Mock
    ClaimService claimService;

    @Mock
    ReturnPurchaseOrderService returnPurchaseOrderService;

    @Mock
    ManageComplaintService manageComplaintService;


    @Mock
    ManageQualityNotificationService manageQualityNotificationService;

    @Mock
    QualityNotificationDao qualityNotificationDao;

    @Mock
    CommonFunctions commonFunctions;

    @Mock
    EightDService eightDService;

    @Mock
    ComplaintService complaintService;

    @Mock
    BusinessObjectService businessObjectService;

    @Mock
    Result result;

    @Mock
    Result result1;

    private QualityNotifications qn;
    private BusinessObjectStatuses businessObjectStatuses;
    private QualityNotificationStatusMappings qualityNotificationStatusMappings;

    private Claims claims;
    private ReturnPurchaseOrders rpo;
    private Supplier8DProcesses supplier8DProcesses;
    private Complaints complaints;
    private MasterData masterData;

    List<BusinessObjectStatuses> boList = new ArrayList<>();
    List<QualityNotificationStatusMappings> qnStatusList = new ArrayList<>();
    List<QualityNotifications> qnlist = new ArrayList<>();


    private Row row;

    private Optional<Row> opt;

    private BusinessPartners subItemTypes;
    private MaterialMasterGeneralDatas material;
    private Plants plants;
    private PurchaseOrganizations purchaseOrganizations;
    private QualityNotificationDetails qualityNotificationDetails;
    private Defects defects;
    QualityNotificaitonItemDetails details2 = new QualityNotificaitonItemDetails();
    QualityNotificaitonItemDetails details = new QualityNotificaitonItemDetails();

    @Before
    public void beforeClass() {

        MockitoAnnotations.openMocks(this);
        row = Struct.create(Row.class);

        qn = Struct.create(QualityNotifications.class);
        qn.setMaterialId("1234");
        qn.setSupplierId("1234");
        qn.setPlantId("1234");
        qn.setPurchasingOrganizationId("3456");
        qn.setQuantity(BigDecimal.TEN);
        qn.setComplaintId("1234");
        qn.setId("123");
        qn.setMaterialCode("111");
        qn.setPlantCode("111");
        qn.setSupplierCode("111");
        qn.setPersonResponsibleCode("111");
        qn.setPurchaseOrganisationCode("111");
        qn.setDefect(defects);


        qn.setReferenceNumber("1111");

        qnlist.add(qn);



        businessObjectStatuses = Struct.create(BusinessObjectStatuses.class);
        businessObjectStatuses.setBusinessObjectStatusCode("QNCRTD");
        businessObjectStatuses.setBusinessObjectType("QN");
        businessObjectStatuses.setParent("124");
        businessObjectStatuses.setBusinessObjectStatus(qn);
        boList.add(businessObjectStatuses);

        qualityNotificationStatusMappings = Struct.create(QualityNotificationStatusMappings.class);
        qualityNotificationStatusMappings.setStatusCode("QNCRTD");
        qualityNotificationStatusMappings.setCode("QNCRTD");
        qualityNotificationStatusMappings.setStatus(businessObjectStatuses);

        qnStatusList.add(qualityNotificationStatusMappings);


        claims = Struct.create(Claims.class);
        claims.setComplaintId("123");
        claims.setId("123");

        rpo = Struct.create(ReturnPurchaseOrders.class);
        rpo.setComplaintId("123");
        rpo.setId("123");

        supplier8DProcesses = Struct.create(Supplier8DProcesses.class);
        supplier8DProcesses.setComplaintId("123");
        supplier8DProcesses.setSupplierId("123");
        supplier8DProcesses.setDefect(defects);


        complaints = Struct.create(Complaints.class);
        complaints.setCreationType("auto");
        complaints.setId("123");
        complaints.setSupplierId("123");




        material = Struct.create(MaterialMasterGeneralDatas.class);
        material.setMaterialCode("1234");
        material.setBaseUnitOfMeasureCode("test");

        plants = Struct.create(Plants.class);
        plants.setPlant("1234");

        purchaseOrganizations = Struct.create(PurchaseOrganizations.class);
        purchaseOrganizations.setPurchaseOrganization("ABC");
        purchaseOrganizations.setId("3456");

        subItemTypes = Struct.create(BusinessPartners.class);
        subItemTypes.setId("123");

        masterData = new MasterData();
        masterData.setMaterial(material);
        masterData.setPlants(plants);
        masterData.setPurchaseOrg(purchaseOrganizations);
        masterData.setSupplier(subItemTypes);

        details.setDefectCode("DefectCode");
        details.setDefectGroup("DefectGroup");
        details.setItemKey("ItemKey");

        details2.setDefectCode("DefectCode");
        details2.setDefectGroup("DefectGroup");
        details2.setItemKey("ItemKey");


        SystemStatus status = new SystemStatus();
        status.setCode("code");
        status.setActiveIndicator("ActiveIndicator");
        status.setDescription("description");
        List<SystemStatus> statusList = new ArrayList<>();
        statusList.add(status);
        qualityNotificationDetails = new QualityNotificationDetails();
        qualityNotificationDetails.setNotificationType("NEW");
        qualityNotificationDetails.setMaterial("material");
        qualityNotificationDetails.setPlant("plant");
        qualityNotificationDetails.setSupplier("supplier");
        qualityNotificationDetails.setPersonResponsible("person");
        //qualityNotificationDetails.setItemDetails(list);
        qualityNotificationDetails.setSystemStatus(statusList);

        defects = Struct.create(Defects.class);
        defects.setId("123");

    }


    @Test
    public void testGetMasterDataIdsBasedOnDetails(){
            manageQualityNotificationServiceImpl.getMasterDataIdsBasedOnDetails(qn);
    }

    @Test
    public void testMapQNStatus(){
        List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "123");
        opt = Optional.of(row);
        rowvalues.add(row);

        when(businessObjectDao.getAllQualityNotificationStatusMappings()).thenReturn(qnStatusList);
        when(businessObjectDao.findBOStatusCode(qn.getId(),qualityNotificationStatusMappings.getStatusCode())).thenReturn(result);
        when(result.listOf(BusinessObjectStatuses.class)).thenReturn(boList);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        manageQualityNotificationServiceImpl.mapQNStatus(qn, boList);

    }

    @Test
    public void testMapQNStatus_nullId(){
        List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "123");
        opt = Optional.of(row);
        rowvalues.add(row);

        when(businessObjectDao.getAllQualityNotificationStatusMappings()).thenReturn(qnStatusList);
        when(businessObjectDao.findBOStatusCode(qn.getId(),qualityNotificationStatusMappings.getStatusCode())).thenReturn(result);
        when(result.listOf(BusinessObjectStatuses.class)).thenReturn(boList);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        qn.setId(null);
        manageQualityNotificationServiceImpl.mapQNStatus(qn, boList);

    }




    @Test
    public void testSetBusinessObjectStatusForQNUpdate() {

        when(businessObjectDao.getAllQualityNotificationStatusMappings()).thenReturn(qnStatusList);
        List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "123");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(businessObjectDao.findBOStatusCode(qn.getId(), qualityNotificationStatusMappings.getStatusCode())).thenReturn(result);
        when(result.listOf(BusinessObjectStatuses.class)).thenReturn(boList);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        manageQualityNotificationServiceImpl.setBusinessObjectStatusForQNUpdate(qn, boList, businessObjectStatuses, qualityNotificationStatusMappings);
    }

    @Test
    public void testSetBusinessObjectStatusForQNUpdate_EmptyBoList() {

        when(businessObjectDao.getAllQualityNotificationStatusMappings()).thenReturn(qnStatusList);
        List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "");
        opt = Optional.of(row);
        rowvalues.add(row);
        List<BusinessObjectStatuses> list = new ArrayList<>();
        when(businessObjectDao.findBOStatusCode(qn.getId(), qualityNotificationStatusMappings.getStatusCode())).thenReturn(result);
        when(result.listOf(BusinessObjectStatuses.class)).thenReturn(Collections.emptyList());
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(businessObjectDao.findBOStatusBasedOnBOId(qn.getId())).thenReturn(result);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        manageQualityNotificationServiceImpl.setBusinessObjectStatusForQNUpdate(qn, boList, businessObjectStatuses, qualityNotificationStatusMappings);
    }



    @Test
    public void testFindExistingBusinessObjectStatusList(){

        List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "123");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(businessObjectDao.findBOStatusBasedOnBOId(qn.getId())).thenReturn(result);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        manageQualityNotificationServiceImpl.findExistingBusinessObjectStatusList(qn.getId());
    }

    @Test
    public void testSetBusinessObjectStatuses(){
        manageQualityNotificationServiceImpl.setBusinessObjectStatuses(businessObjectStatuses,businessObjectStatuses.getBusinessObjectStatusCode());
    }

    @Test
    public void testGetStatus(){

        when(businessObjectService.determineQualityNotificationStatus("abc")).thenReturn("abc");
        manageQualityNotificationServiceImpl.getStatus("abc");
    }


    @Test
    public void testUpdateQualityNotification(){
        List<Defects> defectsList = new ArrayList<>();
        defectsList.add(defects);
        List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "123");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(qualityNotificationDao.getQualityNotificationDetailsFromActive(qn.getId())).thenReturn(result);
        when(result.listOf(QualityNotifications.class)).thenReturn(qnlist);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(qualityNotificationDao.getDefectBasedOnQNFromActive(qn.getId())).thenReturn(result1);
        when(result1.listOf(Defects.class)).thenReturn(defectsList);
        when(result1.list()).thenReturn(rowvalues);
        when(result1.first()).thenReturn(opt);
        when(claimService.getClaim(qn.getComplaintId())).thenReturn(claims);
        when(returnPurchaseOrderService.getReturnPurchaseOrderDetailsBasedOnComplaintId("123")).thenReturn(null);
        when(eightDService.getEightDDetailsBasedOnComplaintId(complaints.getId())).thenReturn(supplier8DProcesses);
        when(complaintService.getComplaintCreationTypeAndCompanyCode("123")).thenReturn(complaints);

        when(configService.getMasterDataDetails(qn.getMaterialId(),qn.getPlantId(),qn.getSupplierId(),qn.getPurchasingOrganizationId())).thenReturn(masterData);
        when(configService.getPurchaseOrganisation(purchaseOrganizations.getId())).thenReturn(purchaseOrganizations);
        when(configService.getMasterDataDetailsByCodes(any(),any(),any(),any(),any())).thenReturn(masterData);

        manageQualityNotificationServiceImpl.updateQualityNotification(qn);
    }

    @Test
    public void testUpdateQNWhenAllBOsAreNull() {

        when(configService.getMasterDataDetailsByCodes(qn.getMaterialCode(),qn.getPlantCode(),qn.getSupplierCode(),qn.getPersonResponsibleCode(),qn.getPurchaseOrganisationCode())).thenReturn(masterData);

        // Run the test
        manageQualityNotificationServiceImpl.updateQNWhenAllBOsAreNull(qn, qn, complaints);


    }

    @Test
    public void testUpdateQNWhenAllBOsAreNull_nullValues() {
        qn.setMaterialCode(null);
        qn.setPlantCode(null);
        qn.setSupplierCode(null);
        qn.setPersonResponsibleCode(null);
        qn.setPurchaseOrganisationCode(null);
        Complaints comp = Struct.create(Complaints.class);
        MasterData data = new MasterData();
        when(configService.getMasterDataDetailsByCodes(qn.getMaterialCode(),qn.getPlantCode(),qn.getSupplierCode(),qn.getPersonResponsibleCode(),qn.getPurchaseOrganisationCode())).thenReturn(data);

        // Run the test
        manageQualityNotificationServiceImpl.updateQNWhenAllBOsAreNull(qn, qn, comp);


    }


    @Test
    public void testUpdatePurchaseOrgWhenBOsAreNull(){
        manageQualityNotificationServiceImpl.updatePurchaseOrgWhenBOsAreNull(qn,complaints,masterData);
    }

    @Test
    public void testUpdatePurchaseOrgWhenBOsAreNull_purchaseOrgNull(){

        MasterData data  = new MasterData();
        complaints.setCreationType("Manual");
        manageQualityNotificationServiceImpl.updatePurchaseOrgWhenBOsAreNull(qn,complaints,data);
    }

    @Test
    public void testUpdatePurchaseOrgWhenBOsAreNull_complaintDetailNull(){
        masterData.setPurchaseOrg(null);
        complaints.setCreationType(null);
        manageQualityNotificationServiceImpl.updatePurchaseOrgWhenBOsAreNull(qn,complaints,masterData);
    }

    @Test
    public void testCheckIfActiveQNExistsBasedOnNumber(){

        List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "092");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(qualityNotificationDao.checkIfActiveQNExistsBasedOnNumber("1234")).thenReturn(result);
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(opt);
        manageQualityNotificationServiceImpl.checkIfActiveQNExistsBasedOnNumber("1234");
    }

    @Test
    public void testGetMasterDataCodeByIds() {
        when(configService.getMasterDataDetails(qn.getMaterialId(),qn.getPlantId(),qn.getSupplierId(),qn.getPurchasingOrganizationId())).thenReturn(masterData);
        manageQualityNotificationServiceImpl.getMasterDataCodeByIds(qn);
    }

    @Test
    public void testGetMasterDataCodeByIds_masterDataNull() {

        MasterData data = new MasterData();
        when(configService.getMasterDataDetails(qn.getMaterialId(),qn.getPlantId(),qn.getSupplierId(),qn.getPurchasingOrganizationId())).thenReturn(data);
        manageQualityNotificationServiceImpl.getMasterDataCodeByIds(qn);
    }

    @Test
    public void testGetQualityNotificationDetailsFromActive() {
    	List<Row> rowvalues = new ArrayList<>();
        row.put("destination", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(opt);
        Mockito.when(result.listOf(QualityNotifications.class)).thenReturn(qnlist);
    	when(qualityNotificationDao.getQualityNotificationDetailsFromActive(qn.getId())).thenReturn(result);
    	manageQualityNotificationServiceImpl.getQualityNotificationDetailsFromActive(qn.getId());
    	
    }
}


