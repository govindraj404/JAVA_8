package com.sap.ic.cmh.qualitynotification.service;

import cds.gen.complaintservice.Complaints;
import cds.gen.configurationservice.BusinessObjectConfigurations;
import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.masterdataservice.MaterialMasterGeneralDatas;
import cds.gen.masterdataservice.Plants;
import cds.gen.masterdataservice.PurchaseOrganizations;
import cds.gen.masterdataservice.UnitOfMeasures;
import cds.gen.qualitynotificationservice.Defects;
import cds.gen.qualitynotificationservice.QualityNotifications;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.claim.model.ResponseModel;
import com.sap.ic.cmh.complaint.service.ComplaintService;
import com.sap.ic.cmh.configuration.model.MasterData;
import com.sap.ic.cmh.configuration.persistency.BusinessObjectConfigurationDao;
import com.sap.ic.cmh.configuration.persistency.DestinationConfigurationDao;
import com.sap.ic.cmh.configuration.service.ConfigurationService;
import com.sap.ic.cmh.masterdata.unitofmeasure.service.UnitOfMeasureService;
import com.sap.ic.cmh.network.service.HttpService;
import com.sap.ic.cmh.qualitynotification.model.QualityNotificationDTO;
import com.sap.ic.cmh.qualitynotification.persistency.QualityNotificationDao;
import com.sap.ic.cmh.qualitynotification.validations.QualityNotificationValidation;
import com.sap.ic.cmh.utils.CommonFunctions;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.QnValidation;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.IOException;
import java.math.BigDecimal;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class QualityNotificationServiceImplTest {

    @InjectMocks
    @Autowired
    QualityNotificationServiceImpl qualityNotificationService;

    @Mock
    CommonFunctions commonFunctions;
    @Mock
    DestinationConfigurationDao destinationConfigDao;

    @Mock
    QualityNotificationDao qualityNotificationDao;
 @Mock
 ComplaintService complaintService;
    @Mock
    Result result;

    @Mock
    ConfigurationService configService;

    @Mock
    UnitOfMeasureService unitOfMeasureService;

    @Mock
    HttpService httpService;

    @Mock
    BusinessObjectConfigurationDao businessObjectConfigurationDao;

    @Mock
    protected PersistenceService mockDb;

    @Mock
    QualityNotificationValidation qualityNotificationValidation;

    @Mock
    Messages messages;

    @Mock
    QnValidation qnValidation;

    private QualityNotifications qn;

    private QualityNotificationDTO qnRequest;

    private Map<String, Object> qnRequestMap = new HashMap<>();

    private MasterData masterData;
    private BusinessPartners partner;
    UnitOfMeasures unitOfMeasures;

    private Row row;
    private Optional<Row> opt;

    private List<BusinessObjectConfigurations> bosList;
    private List<QualityNotifications> qnList;

    private Complaints complaints;


    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        qn = Struct.create(QualityNotifications.class);
        qn.setComplaintId("F677");
        qn.setId("345");
        qn.setQuantity(BigDecimal.TEN);
        qn.setUnit("GTY");
        qn.setPersonResponsibleId("w34");
        qn.setCompanyId("companyId");
        qn.setInspectionResult("CMH_QN");
        qn.setStatusCode("QNCRTD");
        Defects defect = Struct.create(Defects.class);
        defect.setDescription("description");
        qnRequestMap.put("defect",defect);
        qn.setDefect(qnRequestMap);

        complaints = Struct.create(Complaints.class);
        complaints.setId("F677");
        complaints.setReferenceNumber("CMH_REF");

        MaterialMasterGeneralDatas material = Struct.create(MaterialMasterGeneralDatas.class);
        material.setMaterialCode("2344");

        PurchaseOrganizations orgs = Struct.create(PurchaseOrganizations.class);
        orgs.setPurchaseOrganization("FGR");

        partner = Struct.create(BusinessPartners.class);
        partner.setBusinessPartnerNumber("1234");

        Plants plant = Struct.create(Plants.class);
        plant.setPlant("plat");
        plant.setId("67");
        masterData = new MasterData();
        masterData.setPlants(plant);
        masterData.setMaterial(material);
        masterData.setPurchaseOrg(orgs);
        masterData.setSupplier(partner);

        row = Struct.create(Row.class);
        bosList = new ArrayList<>();

        BusinessObjectConfigurations bos = Struct.create(BusinessObjectConfigurations.class);
        bos.setBusinessObjectValue("ruyyy");
        bos.setBusinessObjectAttributeCode("F988");

        bosList.add(bos);

        qnList = new ArrayList<>();
        qnList.add(qn);
        
        unitOfMeasures = Struct.create(UnitOfMeasures.class);
		unitOfMeasures.setCode("GTY");
		unitOfMeasures.setISOCode("PCE");
    }


    @Test
    public void testCreateQualityNotification() throws IOException {
        ResponseModel responseModel=new ResponseModel();
        responseModel.setResult("9999");
        responseModel.setErrorMessage("");
        responseModel.setStatusCode("I0068");
        when(configService.getSupplier(qn.getPersonResponsibleId())).thenReturn(partner);
        when(configService.getMasterDataDetails(qn.getMaterialId(), qn.getPlantId(),
                qn.getSupplierId(), qn.getPurchasingOrganizationId())).thenReturn(masterData);
        when(commonFunctions.convertObjectToMap(any(Object.class))).thenReturn(qnRequestMap);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(qn.getCompanyId(),
                Constants.QUALITYNOTIFICATION_CODE)).thenReturn(result);
        when(unitOfMeasureService.getUnitOfMeasureDetails(qn.getUnit())).thenReturn(unitOfMeasures);
        when(complaintService.getComplaintDetails(qn.getComplaintId())).thenReturn(complaints);
        when(httpService.callCpiFlow(any(String.class), any(Map.class), any(String.class))).thenReturn(responseModel);
        ConcurrentMap<String,String> qnNumberMap = new ConcurrentHashMap<>();
        qnNumberMap.put("Identifier","Identifier");
        when(qnValidation.getQnMap()).thenReturn(qnNumberMap);
        qualityNotificationService.createQualityNotification(qn);
    }

    @Test
    public void testCreateQualityNotificationDestNotNull() throws IOException {

        ResponseModel responseModel=new ResponseModel();
        responseModel.setResult("9999");
        responseModel.setErrorMessage("");
        responseModel.setStatusCode("I0068");
        qn.setUnit(null);
        when(configService.getSupplier(qn.getPersonResponsibleId())).thenReturn(partner);
        when(configService.getMasterDataDetails(qn.getMaterialId(), qn.getPlantId(),
                qn.getSupplierId(), qn.getPurchasingOrganizationId())).thenReturn(masterData);
        when(commonFunctions.convertObjectToMap(any(Object.class))).thenReturn(qnRequestMap);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(qn.getCompanyId(),
                Constants.QUALITYNOTIFICATION_CODE)).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("destination", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(opt);
        when(unitOfMeasureService.getUnitOfMeasureDetails(qn.getUnit())).thenReturn(unitOfMeasures);
         when(complaintService.getComplaintDetails(qn.getComplaintId())).thenReturn(complaints);
        when(httpService.callCpiFlow(any(String.class), any(Map.class), any(String.class))).thenReturn(responseModel);
        ConcurrentMap<String,String> qnNumberMap = new ConcurrentHashMap<>();
        qnNumberMap.put("Identifier","Identifier");
        when(qnValidation.getQnMap()).thenReturn(qnNumberMap);
        qualityNotificationService.createQualityNotification(qn);
    }

    @Test
    public void testCreateQualityNotificationStatusCodeNull() throws IOException {

        ResponseModel responseModel=new ResponseModel();
        responseModel.setResult("9999");
        responseModel.setErrorMessage("");
        responseModel.setStatusCode(null);
        qn.setUnit(null);
        when(configService.getSupplier(qn.getPersonResponsibleId())).thenReturn(partner);
        when(configService.getMasterDataDetails(qn.getMaterialId(), qn.getPlantId(),
                qn.getSupplierId(), qn.getPurchasingOrganizationId())).thenReturn(masterData);
        when(commonFunctions.convertObjectToMap(any(Object.class))).thenReturn(qnRequestMap);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(qn.getCompanyId(),
                Constants.QUALITYNOTIFICATION_CODE)).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("destination", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(opt);
        when(unitOfMeasureService.getUnitOfMeasureDetails(qn.getUnit())).thenReturn(unitOfMeasures);
        when(complaintService.getComplaintDetails(qn.getComplaintId())).thenReturn(complaints);
        when(httpService.callCpiFlow(any(String.class), any(Map.class), any(String.class))).thenReturn(responseModel);
        ConcurrentMap<String,String> qnNumberMap = new ConcurrentHashMap<>();
        qnNumberMap.put("Identifier","Identifier");
        when(qnValidation.getQnMap()).thenReturn(qnNumberMap);
        qualityNotificationService.createQualityNotification(qn);
    }

    @Test
    public void testCreateQualityNotificationMasterDataNull() throws IOException {
        masterData.setMaterial(null);
        masterData.setPurchaseOrg(null);
        qn.setQuantity(null);
        qn.setPurchaseOrderNumber("344");
        qn.setPurchaseOrderItem("Car");
        ResponseModel responseModel=new ResponseModel();
        responseModel.setResult("9999");
        responseModel.setErrorMessage("");
        responseModel.setStatusCode("df");
        qn.setUnit(null);
        when(configService.getSupplier(qn.getPersonResponsibleId())).thenReturn(partner);
        when(configService.getMasterDataDetails(qn.getMaterialId(), qn.getPlantId(),
                qn.getSupplierId(), qn.getPurchasingOrganizationId())).thenReturn(masterData);
        when(commonFunctions.convertObjectToMap(any(Object.class))).thenReturn(qnRequestMap);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(qn.getCompanyId(),
                Constants.QUALITYNOTIFICATION_CODE)).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("destination", "ComplaintID");
        row.put("ISOCode", "IsoCode");
        opt = Optional.of(row);
        rowvalues.add(row);
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(opt);
        when(unitOfMeasureService.getUnitOfMeasureDetails(qn.getUnit())).thenReturn(unitOfMeasures);
        when(complaintService.getComplaintDetails(qn.getComplaintId())).thenReturn(complaints);
        when(httpService.callCpiFlow(any(String.class), any(Map.class), any(String.class))).thenReturn(responseModel);
        ConcurrentMap<String,String> qnNumberMap = new ConcurrentHashMap<>();
        qnNumberMap.put("Identifier","Identifier");
        when(qnValidation.getQnMap()).thenReturn(qnNumberMap);
        qualityNotificationService.createQualityNotification(qn);
    }

    @Test
    public void testCreateQualityNotificationNull() throws IOException {
        when(configService.getSupplier(qn.getPersonResponsibleId())).thenReturn(partner);
        when(configService.getMasterDataDetails(qn.getMaterialId(), qn.getPlantId(),
                qn.getSupplierId(), qn.getPurchasingOrganizationId())).thenReturn(masterData);
        when(commonFunctions.convertObjectToMap(any(Object.class))).thenReturn(qnRequestMap);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(qn.getCompanyId(),
                Constants.QUALITYNOTIFICATION_CODE)).thenReturn(result);
        when(unitOfMeasureService.getUnitOfMeasureDetails(qn.getUnit())).thenReturn(unitOfMeasures);
        when(complaintService.getComplaintDetails(qn.getComplaintId())).thenReturn(complaints);
        when(httpService.callCpiFlow(any(String.class), any(Map.class), any(String.class))).thenReturn(null);
        qualityNotificationService.createQualityNotification(qn);
    }

    @Test
    public void testCreateQualityNotificationException() throws IOException {
        when(configService.getSupplier(qn.getPersonResponsibleId())).thenReturn(partner);
        when(configService.getMasterDataDetails(qn.getMaterialId(), qn.getPlantId(),
                qn.getSupplierId(), qn.getPurchasingOrganizationId())).thenReturn(masterData);
        when(commonFunctions.convertObjectToMap(any(Object.class))).thenReturn(qnRequestMap);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(qn.getCompanyId(),
                Constants.QUALITYNOTIFICATION_CODE)).thenReturn(result);
        when(unitOfMeasureService.getUnitOfMeasureDetails(qn.getUnit())).thenReturn(unitOfMeasures);
        when(complaintService.getComplaintDetails(qn.getComplaintId())).thenReturn(complaints);
        when(httpService.callCpiFlow(any(String.class), any(Map.class), any(String.class))).thenThrow(IOException.class);
        qualityNotificationService.createQualityNotification(qn);
    }


    @Test
    public void testGetQualityNotificationDetailsNull() {
        when(qualityNotificationDao.getQualityNotificationDetails("1234")).thenReturn(result);
        qualityNotificationService.getQualityNotificationDetails("1234");
    }

    @Test
    public void testGetQualityNotificationDetails() {
        List<Row> rowvalues = new ArrayList<>();
        row.put("destination", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(opt);
        Mockito.when(result.listOf(QualityNotifications.class)).thenReturn(qnList);
        when(qualityNotificationDao.getQualityNotificationDetails("1234")).thenReturn(result);
        qualityNotificationService.getQualityNotificationDetails("1234");
    }

    @Test
    public void testGetQualityNotificationDetailsByComplaintIdNull() {
        when(qualityNotificationDao.getQualityNotificationDetailsByComplaintId("1234")).thenReturn(result);
        qualityNotificationService.getQualityNotificationDetailsByComplaintId("1234");
    }

    @Test
    public void testSetConfiguredValues() {

        List<Row> rowvalues = new ArrayList<>();
        row.put("destination", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        Mockito.when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(opt);
        Mockito.when(result.listOf(BusinessObjectConfigurations.class)).thenReturn(bosList);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(qn.getCompanyId(),
                Constants.QUALITYNOTIFICATION_CODE)).thenReturn(result);
        when(businessObjectConfigurationDao
                .getBusinessObjectConfigBasedOnDestinationAndBOAndDest(any(String.class), any(String.class), any(String.class))).thenReturn(result);
        qualityNotificationService.setConfiguredValues(qn,"boType", "F7756");

    }

    @Test
    public void testSetConfiguredValuesQnType() {
        BusinessObjectConfigurations bos = Struct.create(BusinessObjectConfigurations.class);
        bos.setBusinessObjectValue("ruyyy");
        bos.setBusinessObjectAttributeCode("qnType");
        bosList.add(bos);
        List<Row> rowvalues = new ArrayList<>();
        row.put("destination", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        Mockito.when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(opt);
        Mockito.when(result.listOf(BusinessObjectConfigurations.class)).thenReturn(bosList);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(qn.getCompanyId(),
                Constants.QUALITYNOTIFICATION_CODE)).thenReturn(result);
        when(businessObjectConfigurationDao
                .getBusinessObjectConfigBasedOnDestinationAndBOAndDest(any(String.class), any(String.class), any(String.class))).thenReturn(result);
        qualityNotificationService.setConfiguredValues(qn,"boType", "F7756");

    }

    @Test
    public void testSetConfiguredValuesSupplierRole() {
        BusinessObjectConfigurations bos = Struct.create(BusinessObjectConfigurations.class);
        bos.setBusinessObjectValue("ruyyy");
        bos.setBusinessObjectAttributeCode("supplierRole");
        bosList.add(bos);
        List<Row> rowvalues = new ArrayList<>();
        row.put("destination", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        Mockito.when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(opt);
        Mockito.when(result.listOf(BusinessObjectConfigurations.class)).thenReturn(bosList);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(qn.getCompanyId(),
                Constants.QUALITYNOTIFICATION_CODE)).thenReturn(result);
        when(businessObjectConfigurationDao
                .getBusinessObjectConfigBasedOnDestinationAndBOAndDest(any(String.class), any(String.class), any(String.class))).thenReturn(result);
        qualityNotificationService.setConfiguredValues(qn,"boType", "F7756");

    }


    @Test
    public void testSetConfiguredValuesPersonResponsibleRole() {
        BusinessObjectConfigurations bos = Struct.create(BusinessObjectConfigurations.class);
        bos.setBusinessObjectValue("ruyyy");
        bos.setBusinessObjectAttributeCode("personResponsibleRole");
        bosList.add(bos);
        List<Row> rowvalues = new ArrayList<>();
        row.put("destination", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        Mockito.when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(opt);
        Mockito.when(result.listOf(BusinessObjectConfigurations.class)).thenReturn(bosList);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(qn.getCompanyId(),
                Constants.QUALITYNOTIFICATION_CODE)).thenReturn(result);
        when(businessObjectConfigurationDao
                .getBusinessObjectConfigBasedOnDestinationAndBOAndDest(any(String.class), any(String.class), any(String.class))).thenReturn(result);
        qualityNotificationService.setConfiguredValues(qn,"boType", "F7756");

    }


    @Test
    public void testSetConfiguredValuesResultNull() {
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(any(),any())).thenReturn(result);
        when(businessObjectConfigurationDao
                .getBusinessObjectConfigBasedOnDestinationAndBOAndDest(any(String.class), any(String.class), any(String.class))).thenReturn(result);
        qualityNotificationService.setConfiguredValues(qn,"boType", "F7756");

    }


    @Test
    public void testUpdateQualityNotification() {

        qualityNotificationService.updateQualityNotification(qn);
    }

    @Test
    public void testGetQualityNotificationDetailsByComplaintId() {
        List<Row> rowvalues = new ArrayList<>();
        row.put("destination", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(opt);
        Mockito.when(result.listOf(QualityNotifications.class)).thenReturn(qnList);

        when(qualityNotificationDao
                .getQualityNotificationDetailsByComplaintId("1234")).thenReturn(result);
        qualityNotificationService.getQualityNotificationDetailsByComplaintId("1234");
    }

    @Test
    public void testValidateQNDetails() {
        qualityNotificationService.validateQNDetails(qn);

    }

    @Test
    public void testValidateQNExistsAndFieldControl() {
        qualityNotificationService.validateQNExistsAndFieldControl(qn);
    }
    @Test
    public void testValidateIfQNExistsForComplaint() {
        qualityNotificationService.validateIfQNExistsForComplaint("1234");
    }

    @Test
    public void testGetDefectBasedOnQN() {
        when(qualityNotificationDao.getDefectBasedOnQN(any())).thenReturn(result);
        qualityNotificationService.getDefectBasedOnQN("123");
    }

    @Test
    public void testGetDefectBasedOnQNResultNotNull() {
        cds.gen.qualitynotificationservice.Defects defect = Struct.create(cds.gen.qualitynotificationservice.Defects.class);
        List<cds.gen.qualitynotificationservice.Defects> defectList = new ArrayList<>();
        defectList.add(defect);
        defect.setDefectGroupCode("fgh");
        when(qualityNotificationDao.getDefectBasedOnQN(any())).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        Optional<Row> opt = Optional.of(row);
        row.put("ID", "111");
        rowvalues.add(row);
        Mockito.when(result.first()).thenReturn(opt);
        Mockito.when(result.listOf(cds.gen.qualitynotificationservice.Defects.class)).thenReturn(defectList);
        qualityNotificationService.getDefectBasedOnQN("123");
    }

    @Test
    public void checkIfDuplicateQNExistsAndDeleteTest(){
        QualityNotifications qualityNotifications=Struct.create(QualityNotifications.class);
        qualityNotifications.setId("202");
        qualityNotifications.setIdentifier("203");
        qualityNotifications.setSupplierId("309");
        qualityNotifications.setComplaintId("232");
        QualityNotifications qualityNotifications1=Struct.create(QualityNotifications.class);
        qualityNotifications1.setId("202");
        qualityNotifications1.setIdentifier("203");
        qualityNotifications1.setSupplierId("309");
        qualityNotifications1.setComplaintId("232");
        List<QualityNotifications> list=new ArrayList<>();
        list.add(qualityNotifications);
        list.add(qualityNotifications1);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(QualityNotifications.class)).thenReturn(list);
        List<Row> rowValues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "201");
        row.put("itemType_code", "202");
        row.put("complaint_ID", "232");
        row.put("identifier", "203");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        when(qualityNotificationDao.checkIfQNExistsBasedOnNumber(any())).thenReturn(result);
        qualityNotificationService.checkIfDuplicateQNExistsAndDelete("201");
    }

    @Test
    public void checkIfDuplicateQNExistsAndDeletecompliteTest(){
        QualityNotifications qualityNotifications=Struct.create(QualityNotifications.class);
        qualityNotifications.setId("202");
        qualityNotifications.setIdentifier("203");
        qualityNotifications.setSupplierId("309");
        qualityNotifications.setComplaintId("232");
        QualityNotifications qualityNotifications1=Struct.create(QualityNotifications.class);
        qualityNotifications1.setId("202");
        qualityNotifications1.setIdentifier("203");
        qualityNotifications1.setSupplierId("309");
        qualityNotifications1.setComplaintId("232");
        List<QualityNotifications> list=new ArrayList<>();
        list.add(qualityNotifications);
        list.add(qualityNotifications1);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(QualityNotifications.class)).thenReturn(list);
        List<Row> rowValues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "201");
        row.put("itemType_code", "202");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
         Complaints complaints=Struct.create(Complaints.class);
         complaints.setCreationType("Automatic");
         
        when(complaintService.getComplaintCreationTypeAndCompanyCode(any())).thenReturn(complaints);
                when(qualityNotificationDao.checkIfQNExistsBasedOnNumber(any())).thenReturn(result);
        qualityNotificationService.checkIfDuplicateQNExistsAndDelete("201");
    }

    @Test
    public void checkIfDuplicateQNExistsAndDeletecompliteElseTest(){
        QualityNotifications qualityNotifications=Struct.create(QualityNotifications.class);
        qualityNotifications.setId("202");
        qualityNotifications.setIdentifier("203");
        qualityNotifications.setSupplierId("309");
        qualityNotifications.setComplaintId("232");
        QualityNotifications qualityNotifications1=Struct.create(QualityNotifications.class);
        qualityNotifications1.setId("202");
        qualityNotifications1.setIdentifier("203");
        qualityNotifications1.setSupplierId("309");
        qualityNotifications1.setComplaintId("232");
        List<QualityNotifications> list=new ArrayList<>();
        list.add(qualityNotifications);
        list.add(qualityNotifications1);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(QualityNotifications.class)).thenReturn(list);
        List<Row> rowValues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "201");
        row.put("itemType_code", "202");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        Complaints complaints=Struct.create(Complaints.class);
        complaints.setCreationType("test");
        when(complaintService.getComplaintCreationTypeAndCompanyCode(any())).thenReturn(complaints);
        when(qualityNotificationDao.checkIfQNExistsBasedOnNumber(any())).thenReturn(result);
        qualityNotificationService.checkIfDuplicateQNExistsAndDelete("201");
    }
    @Test
    public void checkIfDuplicateQNExistsAndDeleteNullTest(){
        QualityNotifications qualityNotifications=Struct.create(QualityNotifications.class);
        QualityNotifications qualityNotifications1=Struct.create(QualityNotifications.class);
        List<QualityNotifications> list=new ArrayList<>();
        list.add(qualityNotifications);
        list.add(qualityNotifications1);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(QualityNotifications.class)).thenReturn(list);
        List<Row> rowValues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "201");
        row.put("itemType_code", "202");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        Complaints complaints=Struct.create(Complaints.class);
        complaints.setCreationType("Automatic");
        when(complaintService.getComplaintCreationTypeAndCompanyCode(any())).thenReturn(complaints);
        when(qualityNotificationDao.checkIfQNExistsBasedOnNumber(any())).thenReturn(result);
        qualityNotificationService.checkIfDuplicateQNExistsAndDelete("201");
    }

    @Test
    public void testGetStatusAndCompanyCode() {
        List<Row> rowvalues = new ArrayList<>();
        row.put("destination", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(opt);
        Mockito.when(result.listOf(QualityNotifications.class)).thenReturn(qnList);
        when(qualityNotificationDao.getStatusAndCompanyCode("1234")).thenReturn(result);
        qualityNotificationService.getStatusAndCompanyCode("1234");
    }
    
    @Test
    public void testCheckIfQNExistsBasedOnNumber() {
        List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "092");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(qualityNotificationDao.checkIfQNExistsBasedOnNumber("1234")).thenReturn(result);
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(opt);
        qualityNotificationService.checkIfQNExistsBasedOnNumber("1234");
    }
    @Test
    public void testCheckIfQNExistsBasedOnNumberNull() {
        when(qualityNotificationDao.checkIfQNExistsBasedOnNumber("1234")).thenReturn(result);
        qualityNotificationService.checkIfQNExistsBasedOnNumber("1234");
    }

}