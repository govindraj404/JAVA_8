package com.sap.ic.cmh.supplierissueprocess.service;

import cds.gen.com.sap.ic.cmh.supplierissueprocessstatusmapping.SupplierIssueProcessStatusMappings;
import cds.gen.complaintservice.BusinessObjectStatuses;
import cds.gen.configurationservice.BusinessObjectConfigurations;
import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.masterdataservice.MaterialMasterGeneralDatas;
import cds.gen.masterdataservice.Plants;
import cds.gen.qualitynotificationservice.QualityNotifications;
import cds.gen.supplierissueprocessservice.Defects;
import cds.gen.supplierissueprocessservice.Supplier8DProcesses;
import com.google.gson.Gson;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.messages.Messages;
import com.sap.cloud.sdk.cloudplatform.connectivity.HttpDestination;
import com.sap.cloud.sdk.cloudplatform.connectivity.ScpCfDestinationLoader;
import com.sap.cloud.sdk.datamodel.odata.client.exception.ODataException;
import com.sap.cloud.sdk.datamodel.odata.client.exception.ODataServiceError;
import com.sap.cloud.sdk.datamodel.odata.client.exception.ODataServiceErrorDetails;
import com.sap.cloud.sdk.datamodel.odata.client.exception.ODataServiceErrorException;
import com.sap.cloud.sdk.datamodel.odata.helper.ModificationResponse;
import com.sap.ic.cmh.businessobjects.persistency.BusinessObjectDao;
import com.sap.ic.cmh.businessobjects.service.BusinessObjectService;
import com.sap.ic.cmh.claim.validations.ClaimValidation;
import com.sap.ic.cmh.configuration.model.MasterData;
import com.sap.ic.cmh.configuration.persistency.BusinessObjectConfigurationDao;
import com.sap.ic.cmh.configuration.persistency.DestinationConfigurationDao;
import com.sap.ic.cmh.configuration.service.ConfigurationService;
import com.sap.ic.cmh.network.service.DestinationService;
import com.sap.ic.cmh.network.service.HttpService;
import com.sap.ic.cmh.qualitynotification.persistency.QualityNotificationDao;
import com.sap.ic.cmh.supplierissueprocess.persistency.EightDDao;
import com.sap.ic.cmh.supplierissueprocess.validations.EightDValidation;
import com.sap.ic.cmh.utils.Constants;
import org.junit.Before;
import org.junit.Test;
import org.mockito.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import vdm.namespaces.qmsipsrv.SupplierIssueProcesses;
import vdm.namespaces.qmsipsrv.SupplierIssueProcessesByKeyFluentHelper;
import vdm.namespaces.qmsipsrv.SupplierIssueProcessesCreateFluentHelper;
import vdm.services.APIQMSIPSRVService;
import vdm.services.DefaultAPIQMSIPSRVService;

import java.time.LocalDate;
import java.util.*;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class EightDServiceImplTest {

    @InjectMocks
    @Autowired
    EightDServiceImpl service;

    @Mock
    EightDDao eightDDao;

    @Mock
    EightDValidation eightDValidator;

    @Mock
    ConfigurationService configurationService;

    @Mock
    DestinationConfigurationDao destinationConfigDao;

    @Mock
    BusinessObjectConfigurationDao businessObjectConfigurationDao;

    @Mock
    QualityNotificationDao qnDao;

    @Mock
    DestinationService destinationService;

    @Mock
    BusinessObjectService businessObjectService;

    @Mock
    HttpService httpService;

    @Mock
    Messages messages;
    @Mock
    Messages message1;

    @Mock
    Result result;

    @Mock
    CdsService cdsService;

    @Mock
    private HttpDestination destination;
    @Mock
    private ClaimValidation claimValidation;
    @Spy
    private APIQMSIPSRVService apiqmsipsrvService;
    @Mock
    private SupplierIssueProcesses supplierIssueProcesses;
    @Mock
    private Gson gsonObj;
    @Mock
    private ODataException oDataException;
    @Mock
    private ODataServiceError oDataServiceError;
    @Mock
    private ODataServiceErrorDetails oDataServiceErrorDetails;
    @Mock
    BusinessObjectDao businessObjectDao;
    @Mock
    QualityNotifications qualityNotification;

    @Mock
    SupplierIssueProcessesByKeyFluentHelper supplierIssueProcessesByKeyFluentHelper;

    @Mock
    SupplierIssueProcessesCreateFluentHelper supplierIssueProcessesFluent;

    @Mock
    ModificationResponse<SupplierIssueProcesses> createdSupplierIssueProcesses;

    private Supplier8DProcesses eightD;

    private Row row;
    private Optional<Row> opt;

    private MasterData masterData;
    private List<BusinessObjectConfigurations> businessObjectConfigurations;
    private static final Logger logger = LoggerFactory.getLogger(EightDServiceImpl.class);


    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);

        destination = mock(HttpDestination.class);
        eightD = Struct.create(Supplier8DProcesses.class);
        eightD.setComplaintId("Id");
        eightD.setRequestStartDate(LocalDate.now());
        eightD.setRequestEndDate(LocalDate.now());

        row = Struct.create(Row.class);
        destination = Struct.create(HttpDestination.class);


        Plants plants = Struct.create(Plants.class);
        plants.setPlant("plant");

        MaterialMasterGeneralDatas material = Struct.create(MaterialMasterGeneralDatas.class);
        material.setMaterialCode("codeM");

        BusinessPartners partner = Struct.create(BusinessPartners.class);
        partner.setBusinessPartnerNumber("P567");

        masterData = new MasterData();
        masterData.setPlants(plants);
        masterData.setMaterial(material);
        masterData.setSupplier(partner);

        Defects defects = Struct.create(Defects.class);
        defects.setDefectCodeCode("236");
        defects.setIdentifier("45267");
        defects.setDefectGroupCode("F566");

        Map<String, Object> map = new HashMap<>();
        map.put("defects", defects);
        qualityNotification = Struct.create(QualityNotifications.class);
        qualityNotification.setIdentifier("identifier");
        qualityNotification.setPurchaseOrderNumber("12345");
        qualityNotification.setPurchaseOrderItem("Car");
        qualityNotification.setDefect(map);

        BusinessObjectConfigurations boc = Struct.create(BusinessObjectConfigurations.class);
        boc.setBusinessObjectAttributeCode("code");
        boc.setBusinessObjectValue("56gvb");
        boc.setBusinessObjectAttributeCode("supplierIssueProcessesType");
        businessObjectConfigurations = new ArrayList<>();
        businessObjectConfigurations.add(boc);
    }

    @Test
    public void testCreate8D() {
        List<Row> rowvalues = new ArrayList<>();
        row.put("destination", "destinationId");
        row.put("identifier", "destinationId");
        row.put("ID", "destinationId");

        opt = Optional.of(row);
        rowvalues.add(row);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(
                eightD.getCompanyId(), Constants.SUPPLIER_EIGHTD_CODE)).thenReturn(result);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(
                eightD.getCompanyId(), Constants.QUALITYNOTIFICATION_CODE)).thenReturn(result);
        Mockito.when(result.first()).thenReturn(opt);
        when(destinationService.getHttpDestination((any(ScpCfDestinationLoader.class)),
                any(String.class))).thenReturn(destination);
        when(configurationService.getMasterDataDetails(eightD.getMaterialId(),
                eightD.getPlantId(), eightD.getSupplierId(), eightD.getPurchasingOrganizationId())).thenReturn(masterData);
        when(qnDao.getQualityNotificationDetailsByComplaintId(eightD.getComplaintId())).thenReturn(result);
        when(qnDao.fetchQNForSupplier8d(eightD.getComplaintId())).thenReturn(qualityNotification);

        try (MockedConstruction<DefaultAPIQMSIPSRVService> mocked = Mockito.mockConstruction(DefaultAPIQMSIPSRVService.class,
                (mock, context) -> {
                    SupplierIssueProcesses modifiedSupplierIssueProcesses = new SupplierIssueProcesses();
                    modifiedSupplierIssueProcesses.setSupplier("2342");
                    modifiedSupplierIssueProcesses.setSuplrIssProcId("35");
                    modifiedSupplierIssueProcesses.setProcessTypeId("23");
                    modifiedSupplierIssueProcesses.setProcessStatusId("23");
                    modifiedSupplierIssueProcesses.setConfirmationStatusId("466");
                    ModificationResponse<SupplierIssueProcesses> createdSupplierIssueProcesses = mock(ModificationResponse.class);
                    when(createdSupplierIssueProcesses.getResponseStatusCode()).thenReturn(200);
                    when(createdSupplierIssueProcesses.getModifiedEntity()).thenReturn(modifiedSupplierIssueProcesses);
                    SupplierIssueProcessesCreateFluentHelper helper = mock(SupplierIssueProcessesCreateFluentHelper.class);
                    when(mock.createSupplierIssueProcesses(any())).thenReturn(helper);
                    when(helper.executeRequest(any())).thenReturn(createdSupplierIssueProcesses);
                })) {

            service.create8D(eightD, "user");

        }
    }

    @Test
    public void testCreate8DException() {
        List<Row> rowvalues = new ArrayList<>();
        row.put("destination", "destinationId");
        row.put("identifier", "destinationId");
        row.put("ID", "destinationId");

        opt = Optional.of(row);
        rowvalues.add(row);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(
                eightD.getCompanyId(), Constants.SUPPLIER_EIGHTD_CODE)).thenReturn(result);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(
                eightD.getCompanyId(), Constants.QUALITYNOTIFICATION_CODE)).thenReturn(result);
        Mockito.when(result.first()).thenReturn(opt);
        when(destinationService.getHttpDestination((any(ScpCfDestinationLoader.class)),
                any(String.class))).thenReturn(destination);
        when(configurationService.getMasterDataDetails(eightD.getMaterialId(),
                eightD.getPlantId(), eightD.getSupplierId(), eightD.getPurchasingOrganizationId())).thenReturn(masterData);
        when(qnDao.getQualityNotificationDetailsByComplaintId(eightD.getComplaintId())).thenReturn(result);
        when(qnDao.fetchQNForSupplier8d(eightD.getComplaintId())).thenReturn(qualityNotification);

        try (MockedConstruction<DefaultAPIQMSIPSRVService> mocked = Mockito.mockConstruction(DefaultAPIQMSIPSRVService.class,
                (mock, context) -> {
                    SupplierIssueProcesses modifiedSupplierIssueProcesses = new SupplierIssueProcesses();
                    modifiedSupplierIssueProcesses.setSupplier("2342");
                    modifiedSupplierIssueProcesses.setSuplrIssProcId("35");
                    modifiedSupplierIssueProcesses.setProcessTypeId("23");
                    modifiedSupplierIssueProcesses.setProcessStatusId("23");
                    modifiedSupplierIssueProcesses.setConfirmationStatusId("466");
                    ModificationResponse<SupplierIssueProcesses> createdSupplierIssueProcesses = mock(ModificationResponse.class);
                    when(createdSupplierIssueProcesses.getResponseStatusCode()).thenReturn(200);
                    when(createdSupplierIssueProcesses.getModifiedEntity()).thenReturn(modifiedSupplierIssueProcesses);
                    SupplierIssueProcessesCreateFluentHelper helper = mock(SupplierIssueProcessesCreateFluentHelper.class);
                    when(mock.createSupplierIssueProcesses(any())).thenReturn(helper);
                    Map<String, Object> map = new HashMap<>();
                    map.put("ID",modifiedSupplierIssueProcesses);
                    ODataServiceErrorException exception = mock(ODataServiceErrorException.class);
                    ODataServiceError oDataServiceError = mock(ODataServiceError.class);
                    when(oDataServiceError.getODataCode()).thenReturn("435");
                    when(oDataServiceError.getInnerError()).thenReturn(map);
                    when(oDataServiceError.getODataMessage()).thenReturn("message");
                    when(exception.getOdataError()).thenReturn(oDataServiceError);
                    when(helper.executeRequest(any())).thenThrow(exception);
                })) {

            service.create8D(eightD, "user");

        }
    }

    @Test
    public void testGetEightDDetails() {
        List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);

        when(eightDDao.getEightDBasedOnId("234")).thenReturn(result);
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(opt);
        Mockito.when(result.single(Supplier8DProcesses.class)).thenReturn(eightD);
        service.getEightDDetails("234");
    }


    @Test
    public void testGetEightDDetailsBasedOnComplaintId() {
        List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(eightDDao.getEightDDetailsBasedOnComplaintId("1234")).thenReturn(result);
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(opt);
        service.getEightDDetailsBasedOnComplaintId("1234");
    }

    @Test
    public void testValidate8DDetails() {
        service.validate8DDetails(eightD);
    }

    @Test
    public void testSetConfiguredValues() {

        List<Row> rowvalues = new ArrayList<>();
        row.put("destination", "destinationID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(eightD.getCompanyId(),
                Constants.SUPPLIER_EIGHTD_CODE)).thenReturn(result);
        when(businessObjectConfigurationDao
                .getBusinessObjectConfigBasedOnDestinationAndBOAndDest(any(String.class), any(String.class), any(String.class))).thenReturn(result);
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(opt);
        Mockito.when(result.listOf(BusinessObjectConfigurations.class)).thenReturn(businessObjectConfigurations);
        service.setConfiguredValues(eightD, "BoType", "F009");
    }

    @Test
    public void testValidateIf8DExists() {
        service.validateIf8DExists(eightD);
    }

    @Test
    public void testMapEightDStatus() {
        SupplierIssueProcessStatusMappings supplierIssueProcessStatusMappings = Struct.create(SupplierIssueProcessStatusMappings.class);
        supplierIssueProcessStatusMappings.setStatusCode("F543");
        List<SupplierIssueProcessStatusMappings> mappingList = new ArrayList<>();
        mappingList.add(supplierIssueProcessStatusMappings);
        List<cds.gen.supplierissueprocessinternalservice.Supplier8DProcesses> supplier8DProcessesList = new ArrayList<>();
        cds.gen.supplierissueprocessinternalservice.Supplier8DProcesses supplier8DProcesses = Struct.create(cds.gen.supplierissueprocessinternalservice.Supplier8DProcesses.class);
        supplier8DProcesses.setIdentifier("123");
        supplier8DProcesses.setCompanyId("123");
        supplier8DProcessesList.add(supplier8DProcesses);

        row.put("destination", "destinationID");
        opt = Optional.of(row);
        Mockito.when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(
                supplier8DProcesses.getCompanyId(), Constants.SUPPLIER_EIGHTD_CODE)).thenReturn(result);
        Mockito.when(result.first()).thenReturn(opt);
        Mockito.when(destinationService.getHttpDestination(any(), any())).thenReturn(destination);
        Mockito.when(eightDDao.getActiveEightD()).thenReturn(supplier8DProcessesList);
        Mockito.when(businessObjectDao.getAllSupplierIssueProcessMappings()).thenReturn(mappingList);

        try (MockedConstruction<DefaultAPIQMSIPSRVService> mocked = Mockito.mockConstruction(DefaultAPIQMSIPSRVService.class,
                (mock, context) -> {
                    SupplierIssueProcesses modifiedSupplierIssueProcesses = new SupplierIssueProcesses();
                    modifiedSupplierIssueProcesses.setSupplier("2342");
                    modifiedSupplierIssueProcesses.setSuplrIssProcId("35");
                    modifiedSupplierIssueProcesses.setProcessTypeId("23");
                    modifiedSupplierIssueProcesses.setProcessStatusId("23");
                    modifiedSupplierIssueProcesses.setConfirmationStatusId("466");

                    SupplierIssueProcessesByKeyFluentHelper helper = mock(SupplierIssueProcessesByKeyFluentHelper.class);
                    when(mock.getSupplierIssueProcessesByKey(any())).thenReturn(helper);
                    when(helper.executeRequest(any())).thenReturn(modifiedSupplierIssueProcesses);
                })) {

            service.mapEightDStatus();

        }


    }
    
    @Test
    public void testMapEightDStatusException() {
        SupplierIssueProcessStatusMappings supplierIssueProcessStatusMappings = Struct.create(SupplierIssueProcessStatusMappings.class);
        supplierIssueProcessStatusMappings.setStatusCode("F543");
        List<SupplierIssueProcessStatusMappings> mappingList = new ArrayList<>();
        mappingList.add(supplierIssueProcessStatusMappings);
        List<cds.gen.supplierissueprocessinternalservice.Supplier8DProcesses> supplier8DProcessesList = new ArrayList<>();
        cds.gen.supplierissueprocessinternalservice.Supplier8DProcesses supplier8DProcesses = Struct.create(cds.gen.supplierissueprocessinternalservice.Supplier8DProcesses.class);
        supplier8DProcesses.setIdentifier("");
        supplier8DProcesses.setCompanyId("");
        cds.gen.supplierissueprocessinternalservice.Supplier8DProcesses supplier8DProcesses1 = Struct.create(cds.gen.supplierissueprocessinternalservice.Supplier8DProcesses.class);
        supplier8DProcesses1.setIdentifier("123");
        supplier8DProcesses1.setCompanyId("123");
        supplier8DProcessesList.add(supplier8DProcesses);
        supplier8DProcessesList.add(supplier8DProcesses1);

        row.put("destination", "destinationID");
        opt = Optional.of(row);
        Mockito.when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(
                supplier8DProcesses.getCompanyId(), Constants.SUPPLIER_EIGHTD_CODE)).thenReturn(result);
        Mockito.when(result.first()).thenReturn(opt);
        Mockito.when(destinationService.getHttpDestination(any(), any())).thenReturn(destination);
        Mockito.when(eightDDao.getActiveEightD()).thenReturn(supplier8DProcessesList);
        Mockito.when(businessObjectDao.getAllSupplierIssueProcessMappings()).thenReturn(mappingList);

        try (MockedConstruction<DefaultAPIQMSIPSRVService> mocked = Mockito.mockConstruction(DefaultAPIQMSIPSRVService.class,
                (mock, context) -> {
                    SupplierIssueProcesses modifiedSupplierIssueProcesses = new SupplierIssueProcesses();
                    modifiedSupplierIssueProcesses.setSupplier("2342");
                    modifiedSupplierIssueProcesses.setSuplrIssProcId("35");
                    modifiedSupplierIssueProcesses.setProcessTypeId("23");
                    modifiedSupplierIssueProcesses.setProcessStatusId("23");
                    modifiedSupplierIssueProcesses.setConfirmationStatusId("466");

                    SupplierIssueProcessesByKeyFluentHelper helper = mock(SupplierIssueProcessesByKeyFluentHelper.class);
                    when(mock.getSupplierIssueProcessesByKey(any())).thenReturn(helper);
                    when(helper.executeRequest(any())).thenReturn(modifiedSupplierIssueProcesses);
                })) {

            service.mapEightDStatus();

        }


    }

    @Test
    public void testProcessEightDStatusesAndUpdate() {
        SupplierIssueProcessStatusMappings mappings = Struct.create(SupplierIssueProcessStatusMappings.class);
        mappings.setCode("code");
        mappings.setStatusCode("Status");
        mappings.setFieldName("Status");
        SupplierIssueProcessStatusMappings mappings1 = Struct.create(SupplierIssueProcessStatusMappings.class);
        mappings1.setCode("F456");
        mappings1.setStatusCode("S3455");
        List<SupplierIssueProcessStatusMappings> allSupplierIssueProcessMappings = new ArrayList<>();
        allSupplierIssueProcessMappings.add(mappings);
        allSupplierIssueProcessMappings.add(mappings1);
        cds.gen.supplierissueprocessinternalservice.Supplier8DProcesses supplier8DProcess = Struct.create(cds.gen.supplierissueprocessinternalservice.Supplier8DProcesses.class);
        supplier8DProcess.setIdentifier("112");
        supplier8DProcess.setSupplierId("242");
        Map<String, String> statusMap = new HashMap<>();
        statusMap.put("F456", "S3455");
        statusMap.put("Status", "CODE");
        when(businessObjectDao.findBOStatusCode(any(), any())).thenReturn(result);
        when(businessObjectDao.findEightDStatusMappingbyCode(any(), any())).thenReturn(result);
        when(cdsService.run(any(CqnUpdate.class))).thenReturn(result);
        service.processEighDStatusesAndUpdate(allSupplierIssueProcessMappings, supplier8DProcess, statusMap);
    }

    @Test
    public void testProcessEightDStatusesAndUpdateBoStatusNull() {
        List<BusinessObjectStatuses> boStatuses = new ArrayList<>();
        SupplierIssueProcessStatusMappings mappings = Struct.create(SupplierIssueProcessStatusMappings.class);
        mappings.setCode("code");
        mappings.setStatusCode("Status");
        mappings.setFieldName("Status");
        SupplierIssueProcessStatusMappings mappings1 = Struct.create(SupplierIssueProcessStatusMappings.class);
        mappings1.setCode("F456");
        mappings1.setStatusCode("S3455");
        mappings.setFieldName("Status");
        List<SupplierIssueProcessStatusMappings> allSupplierIssueProcessMappings = new ArrayList<>();
        allSupplierIssueProcessMappings.add(mappings);
        allSupplierIssueProcessMappings.add(mappings1);
        cds.gen.supplierissueprocessinternalservice.Supplier8DProcesses supplier8DProcess = Struct.create(cds.gen.supplierissueprocessinternalservice.Supplier8DProcesses.class);
        supplier8DProcess.setIdentifier("112");
        supplier8DProcess.setSupplierId("242");
        Map<String, String> statusMap = new HashMap<>();
        statusMap.put("F456", "S3455");
        statusMap.put("Status", "CODE");
        when(businessObjectDao.findBOStatusCode(any(), any())).thenReturn(result);
        when(businessObjectDao.findEightDStatusMappingbyCode(any(), any())).thenReturn(result);
        when(cdsService.run(any(CqnUpdate.class))).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("status_code", "code");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.first()).thenReturn(opt);
        when(result.list()).thenReturn(rowvalues);
        when(result.listOf(BusinessObjectStatuses.class)).thenReturn(boStatuses);
        service.processEighDStatusesAndUpdate(allSupplierIssueProcessMappings, supplier8DProcess, statusMap);
    }

    @Test
    public void testProcessEightDStatusesAndUpdateResultNotNull() {
        BusinessObjectStatuses status = Struct.create(BusinessObjectStatuses.class);
        status.setId("1234");
        List<BusinessObjectStatuses> boStatuses = new ArrayList<>();
        boStatuses.add(status);
        SupplierIssueProcessStatusMappings mappings = Struct.create(SupplierIssueProcessStatusMappings.class);
        mappings.setCode("code");
        mappings.setStatusCode("Status");
        mappings.setFieldName("Status");
        SupplierIssueProcessStatusMappings mappings1 = Struct.create(SupplierIssueProcessStatusMappings.class);
        mappings1.setCode("F456");
        mappings1.setStatusCode("S3455");
        mappings.setFieldName("Status");
        List<SupplierIssueProcessStatusMappings> allSupplierIssueProcessMappings = new ArrayList<>();
        allSupplierIssueProcessMappings.add(mappings);
        allSupplierIssueProcessMappings.add(mappings1);
        cds.gen.supplierissueprocessinternalservice.Supplier8DProcesses supplier8DProcess = Struct.create(cds.gen.supplierissueprocessinternalservice.Supplier8DProcesses.class);
        supplier8DProcess.setIdentifier("112");
        supplier8DProcess.setSupplierId("242");
        Map<String, String> statusMap = new HashMap<>();
        statusMap.put("F456", "S3455");
        statusMap.put("Status", "CODE");
        when(businessObjectDao.findBOStatusCode(any(), any())).thenReturn(result);
        when(businessObjectDao.findEightDStatusMappingbyCode(any(), any())).thenReturn(result);
        when(cdsService.run(any(CqnUpdate.class))).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("status_code", "code");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.first()).thenReturn(opt);
        when(result.list()).thenReturn(rowvalues);
        when(result.listOf(BusinessObjectStatuses.class)).thenReturn(boStatuses);
        service.processEighDStatusesAndUpdate(allSupplierIssueProcessMappings, supplier8DProcess, statusMap);
    }

    @Test
    public void testValidateIf8DExistsForComplaint() {
        eightD.setComplaintId("1010");
        List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(eightDDao.getEightDDetailsBasedOnComplaintId("1010")).thenReturn(result);
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(opt);
        Mockito.when(result.single(Supplier8DProcesses.class)).thenReturn(eightD);
        claimValidation.validateIfQualityNotificationExists("1010", Constants.SUPPLIER_EIGHTD_CODE);
        eightDValidator.validateifBOIsRelevant("1010", Constants.SUPPLIER_EIGHTD_CODE);
        service.validateIf8DExists(eightD);
        service.validateIf8DExistsForComplaint("1010");
    }

    @Test
    public void testSetDetailsFromQualityNotification() {
        when(qnDao.fetchQNForSupplier8d(eightD.getComplaintId())).thenReturn(qualityNotification);
        supplierIssueProcesses.setNotification(qualityNotification.getIdentifier());
        supplierIssueProcesses.setPurchaseOrder(qualityNotification.getPurchaseOrderNumber());
        supplierIssueProcesses.setPurchaseOrderItem(qualityNotification.getPurchaseOrderItem());
        service.setDetailsFromQualityNotification(eightD, supplierIssueProcesses);
    }

/*   @Test(expected = Exception.class)
    public void testCreate8DAtDestination(){
        String eightDNumber = "";
        Mockito.when(apiqmsipsrvService.createSupplierIssueProcesses(supplierIssueProcesses).executeRequest(destination).getModifiedEntity()).thenReturn(supplierIssueProcesses);
        when(oDataException.getMessage()).thenReturn(oDataException.toString());
        when(oDataException.getLocalizedMessage()).thenReturn(oDataException.toString());
        when(oDataServiceError.getODataMessage()).thenReturn(oDataServiceError.toString());
        when(oDataServiceError.getODataCode()).thenReturn(oDataServiceError.toString());
        when(oDataServiceError.getInnerError().toString()).thenReturn(oDataServiceError.toString());
        when(oDataServiceErrorDetails.getODataCode()).thenReturn(oDataServiceErrorDetails.getODataCode());
        when(oDataServiceErrorDetails.getODataMessage()).thenReturn(oDataServiceErrorDetails.getODataCode());
        ModificationResponse<SupplierIssueProcesses> createdSupplierIssueProcesses = apiqmsipsrvService.createSupplierIssueProcesses(supplierIssueProcesses).executeRequest(destination);
        SupplierIssueProcesses modifiedSupplierIssueProcesses = createdSupplierIssueProcesses
                .getModifiedEntity();
        String modifiedSupplierIssueProcessesJsonStr = gsonObj.toJson(modifiedSupplierIssueProcesses);
        Map<String, Object> modifiedSupplierIssueProcessesMap = gsonObj
                .fromJson(modifiedSupplierIssueProcessesJsonStr, Map.class);
        eightDNumber = modifiedSupplierIssueProcessesMap.get("suplrIssProcId").toString();
        when(eightDNumber).thenReturn(eightDNumber);
        when(gsonObj.fromJson(modifiedSupplierIssueProcessesJsonStr, Map.class)).thenReturn(modifiedSupplierIssueProcessesMap);
        //service.create8DAtDestination(eightD);
    }*/
}
