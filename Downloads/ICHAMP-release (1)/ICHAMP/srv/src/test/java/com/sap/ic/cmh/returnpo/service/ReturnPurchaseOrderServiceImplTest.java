package com.sap.ic.cmh.returnpo.service;


import cds.gen.com.sap.ic.cmh.returnpurchaseorderstatusmapping.ReturnPurchaseOrderStatusMappings;
import cds.gen.com.sap.ic.cmh.supplierissueprocessstatusmapping.SupplierIssueProcessStatusMappings;
import cds.gen.configurationservice.BusinessObjectConfigurations;
import cds.gen.configurationservice.DestinationConfigurations;
import cds.gen.masterdataservice.*;
import cds.gen.qualitynotificationservice.QualityNotifications;
import cds.gen.returnpurchaseorderservice.ReturnPurchaseOrders;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.ServiceException;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.cds.CdsReadEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.draft.DraftNewEventContext;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.cds.services.request.UserInfo;
import com.sap.cloud.sdk.cloudplatform.connectivity.ScpCfDestinationLoader;
import com.sap.ic.cmh.businessobjects.persistency.BusinessObjectDao;
import com.sap.ic.cmh.businessobjects.service.BusinessObjectService;
import com.sap.ic.cmh.claim.model.ResponseModel;
import com.sap.ic.cmh.claim.model.binary_relation.BinaryRelationDataModel;
import com.sap.ic.cmh.claim.model.binary_relation.BinaryRelationObject;
import com.sap.ic.cmh.claim.validations.ClaimValidation;
import com.sap.ic.cmh.configuration.model.MasterData;
import com.sap.ic.cmh.configuration.persistency.BusinessObjectConfigurationDao;
import com.sap.ic.cmh.configuration.persistency.ConfigurationDao;
import com.sap.ic.cmh.configuration.persistency.DestinationConfigurationDao;
import com.sap.ic.cmh.configuration.service.ConfigurationService;
import com.sap.ic.cmh.masterdata.unitofmeasure.service.UnitOfMeasureService;
import com.sap.ic.cmh.network.service.HttpService;
import com.sap.ic.cmh.qualitynotification.service.QualityNotificationService;
import com.sap.ic.cmh.returnpo.model.*;
import com.sap.ic.cmh.returnpo.persistency.ReturnPurchaseOrderDao;
import com.sap.ic.cmh.returnpo.validation.ReturnPurchaseOrderValidation;
import com.sap.ic.cmh.utils.CommonFunctions;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.IOException;
import java.util.*;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

public class ReturnPurchaseOrderServiceImplTest {
    @InjectMocks
    ReturnPurchaseOrderServiceImpl returnPurchaseOrderService;
    @Mock
    CommonFunctions commonFunctions;
    @Mock
    HttpService httpService;
    @Mock
    ReturnPurchaseOrderDao returnPurchaseOrderDao;
    @Mock
    ConfigurationService configurationService;
    @Mock
    ConfigurationDao configurationDao;
    @Mock
    ReturnPurchaseOrderValidation returnPurchaseOrderValidation;
    @Mock
    DestinationConfigurationDao destinationConfigDao;
    @Autowired
    @Mock
    BusinessObjectConfigurationDao businessObjectConfigurationDao;
    @Mock
    QualityNotificationService qualityNotificationService;
    @Mock
    BusinessObjectService businessObjectService;
    @Mock
    Messages messages;
    @Mock
    LoggerHelper loggerHelper;
    @Mock
    CdsCreateEventContext createContextMock;
    @Mock
    CqnInsert cqnInsert;
    @Mock
    Result result;
    @Mock
    DraftNewEventContext context;
    @Mock
    PersistenceService mockDb;
    @Mock
    PersistenceService db;
    @Mock
    CdsReadEventContext readEventContext;
    @Mock
    UserInfo user;
    @Mock
    UnitOfMeasureService unitOfMeasureService;
    @Mock
    ClaimValidation claimValidation;
    @Mock
    BusinessObjectDao businessObjectDao;
    QualityNotifications qualityNotifications;
    List<BusinessObjectConfigurations> bocsList = new ArrayList<>();
    @Mock
    private CdsService cdsService;
    private ReturnPurchaseOrders returnPurchaseOrders;
    private CompanyCodes companycode;
    private MasterData masterData;
    private PurchaseOrganizations purchaseOrganizations;
    private BusinessPartners businessPartners;
    private BusinessObjectConfigurations businessObjectConfigurations;
    private BusinessObjectConfigurationDao businessObjectConfigurationsDao;
    private MaterialMasterGeneralDatas materialMasterGeneralDatas;
    private Plants plants;
    private Row row;
    private Optional<Row> opt;
    UnitOfMeasures unitOfMeasures;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        qualityNotifications = Struct.create(QualityNotifications.class);
        qualityNotifications.setMaterialId("test");
        qualityNotifications.setPlantId("test");
        qualityNotifications.setSupplierId("test");
        qualityNotifications.setIdentifier("Fp01");
        businessObjectConfigurations = Struct.create(BusinessObjectConfigurations.class);
        businessPartners = Struct.create(BusinessPartners.class);
        businessPartners.setBusinessPartnerNumber("202");
        businessObjectConfigurations.setBusinessObjectTypeCode("202");
        businessObjectConfigurations.setBusinessObjectAttributeCode("returnPurchaseType");
        businessObjectConfigurations.setBusinessObjectValue("itemType");
        bocsList.add(businessObjectConfigurations);
        materialMasterGeneralDatas = Struct.create(MaterialMasterGeneralDatas.class);
        materialMasterGeneralDatas.setMaterialCode("44");
        plants = Struct.create(Plants.class);
        plants.setPlant("Fp01");
        masterData = new MasterData();
        masterData.setPurchaseOrg(purchaseOrganizations);
        masterData.setSupplier(businessPartners);
        masterData.setPlants(plants);

        returnPurchaseOrders = Struct.create(ReturnPurchaseOrders.class);
        companycode = Struct.create(CompanyCodes.class);
        companycode.setCompanyCode("12");
        companycode.setId("201");
        returnPurchaseOrders.setCompanyId("201");
        returnPurchaseOrders.setComplaintId("101");
        //returnPurchaseOrders.setContactPerson("sap");
        returnPurchaseOrders.setReturnPurchaseType("PO");
        returnPurchaseOrders.setIdentifier("test");
        returnPurchaseOrders.setSupplierId("201");
        returnPurchaseOrders.setReasonCode("DRO");
        returnPurchaseOrders.setPurchasingGroupCode("001");
        when(createContextMock.getCqn()).thenReturn(cqnInsert);
        List<Map<String, Object>> entries = new ArrayList<>();
        when(cqnInsert.entries()).thenReturn(entries);

        row = Struct.create(Row.class);
        
        unitOfMeasures = Struct.create(UnitOfMeasures.class);
		unitOfMeasures.setCode("GTY");
		unitOfMeasures.setISOCode("PCE");

    }

    @Test
    public void setConfiguredValuesTest() {
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(returnPurchaseOrders.getCompanyId(),
                Constants.RETURNPO_CODE)).thenReturn(result);
        when(businessObjectConfigurationDao
                .getBusinessObjectConfigBasedOnDestinationAndBOAndDest(any(String.class), any(String.class), any(String.class))).thenReturn(result);

        List<Row> rowvalues = new ArrayList<>();
        row.put("destination", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(opt);
        when(result.listOf(BusinessObjectConfigurations.class)).thenReturn(bocsList);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(any(String.class), any(String.class))).thenReturn(result);
        returnPurchaseOrderService.setConfiguredValues(returnPurchaseOrders, "test", "Fp01");
    }

    @Test
    public void testSetConfiguredValues() {
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(returnPurchaseOrders.getCompanyId(),
                Constants.RETURNPO_CODE)).thenReturn(result);
        when(businessObjectConfigurationDao
                .getBusinessObjectConfigBasedOnDestinationAndBOAndDest(any(String.class), any(String.class), any(String.class))).thenReturn(result);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(any(String.class), any(String.class))).thenReturn(result);
        returnPurchaseOrderService.setConfiguredValues(returnPurchaseOrders, "test", "Fp01");
    }

    @Test
    public void setConfiguredValuesTestItemType() {
        businessObjectConfigurations.setBusinessObjectAttributeCode("itemNumber");
        bocsList.add(businessObjectConfigurations);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(returnPurchaseOrders.getCompanyId(),
                Constants.RETURNPO_CODE)).thenReturn(result);
        when(businessObjectConfigurationDao
                .getBusinessObjectConfigBasedOnDestinationAndBOAndDest(any(String.class), any(String.class), any(String.class))).thenReturn(result);

        List<Row> rowvalues = new ArrayList<>();
        row.put("destination", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(opt);
        when(result.listOf(BusinessObjectConfigurations.class)).thenReturn(bocsList);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(any(String.class), any(String.class))).thenReturn(result);
        returnPurchaseOrderService.setConfiguredValues(returnPurchaseOrders, "test", "Fp01");
    }

    @Test
    public void testSetConfiguredValuesDiffItemType() {
        businessObjectConfigurations.setBusinessObjectAttributeCode("PurchaseItemNumber");
        bocsList.add(businessObjectConfigurations);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(returnPurchaseOrders.getCompanyId(),
                Constants.RETURNPO_CODE)).thenReturn(result);

        when(businessObjectConfigurationDao
                .getBusinessObjectConfigBasedOnDestinationAndBOAndDest(any(String.class), any(String.class), any(String.class))).thenReturn(result);

        List<Row> rowvalues = new ArrayList<>();
        row.put("destination", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(opt);
        when(result.listOf(BusinessObjectConfigurations.class)).thenReturn(bocsList);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(any(String.class), any(String.class))).thenReturn(result);
        returnPurchaseOrderService.setConfiguredValues(returnPurchaseOrders, "test", "Fp01");
    }

    @Test
    public void testCreateReturnPurchaseOrder() {
        qualityNotifications.setIdentifier("12");
        ScpCfDestinationLoader scpCfDestinationLoader = new ScpCfDestinationLoader();
        returnPurchaseOrders.setCompanyId("3");
        returnPurchaseOrders.setMaterialId("34");
        returnPurchaseOrders.setPlantId("3");
        returnPurchaseOrders.setSupplierId("5");
        returnPurchaseOrders.setStatusCode("200");
        returnPurchaseOrders.setPurchasingOrganizationId("4");
        returnPurchaseOrders.setItemNumber("2");
        returnPurchaseOrders.setPersonResponsibleId("2");
        returnPurchaseOrders.setUnit("ST");
        PurchaseOrganizations prOrg = Struct.create(PurchaseOrganizations.class);
        prOrg.setPurchaseOrganization("test");
        MaterialMasterGeneralDatas matiral = Struct.create(MaterialMasterGeneralDatas.class);
        matiral.setMaterialCode("Gun");
        masterData.setPurchaseOrg(prOrg);
        masterData.setMaterial(matiral);
        BusinessPartners businessPartners = Struct.create(BusinessPartners.class);
        businessPartners.setBusinessPartnerNumber("2");
        when(configurationService.getCompanyCodes(returnPurchaseOrders.getCompanyId())).thenReturn(companycode);
        when(configurationService.getMasterDataDetails(returnPurchaseOrders.getMaterialId(), returnPurchaseOrders.getPlantId()
                , returnPurchaseOrders.getSupplierId(), returnPurchaseOrders.getPurchasingOrganizationId())).thenReturn(masterData);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(returnPurchaseOrders.getCompanyId(), "QN")).thenReturn(result);
        when(unitOfMeasureService.getUnitOfMeasureDetails(returnPurchaseOrders.getUnit())).thenReturn(unitOfMeasures);
        when(configurationService.getSupplier(returnPurchaseOrders.getPersonResponsibleId())).thenReturn(businessPartners);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(returnPurchaseOrders.getCompanyId(), "RPO")).thenReturn(result);

        List<Row> rowvalues = new ArrayList<>();
        row.put("ISOCode", "ComplaintID");
        row.put("destination", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);

        when(httpService.getLogicalSystem(scpCfDestinationLoader, "destination")).thenReturn("ASDCLNT100");
        when(httpService.getTargetLogicalSystem(scpCfDestinationLoader, "destination")).thenReturn("ASDCLNT100");
        when(qualityNotificationService.getQualityNotificationDetailsByComplaintId(any())).thenReturn(qualityNotifications);
        returnPurchaseOrderService.createReturPurchaseOrder(returnPurchaseOrders);

    }

    @Test
    public void testCreateReturnPurchaseOrderNull() throws IOException {
        qualityNotifications.setIdentifier("12");
        ScpCfDestinationLoader scpCfDestinationLoader = new ScpCfDestinationLoader();
        returnPurchaseOrders.setCompanyId("3");
        returnPurchaseOrders.setMaterialId("34");
        returnPurchaseOrders.setPlantId("3");
        returnPurchaseOrders.setSupplierId("5");
        returnPurchaseOrders.setStatusCode("200");
        returnPurchaseOrders.setPurchasingOrganizationId("4");
        returnPurchaseOrders.setItemNumber("2");
        returnPurchaseOrders.setPersonResponsibleId("2");
        returnPurchaseOrders.setUnit("ST");
        PurchaseOrganizations prOrg = Struct.create(PurchaseOrganizations.class);
        prOrg.setPurchaseOrganization("test");
        MaterialMasterGeneralDatas matiral = Struct.create(MaterialMasterGeneralDatas.class);
        matiral.setMaterialCode("Gun");
        masterData.setPurchaseOrg(prOrg);
        masterData.setMaterial(matiral);
        BusinessPartners businessPartners = Struct.create(BusinessPartners.class);
        businessPartners.setBusinessPartnerNumber("2");
        when(configurationService.getCompanyCodes(returnPurchaseOrders.getCompanyId())).thenReturn(companycode);
        when(configurationService.getMasterDataDetails(returnPurchaseOrders.getMaterialId(), returnPurchaseOrders.getPlantId()
                , returnPurchaseOrders.getSupplierId(), returnPurchaseOrders.getPurchasingOrganizationId())).thenReturn(masterData);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(returnPurchaseOrders.getCompanyId(), "QN")).thenReturn(result);
        when(unitOfMeasureService.getUnitOfMeasureDetails(returnPurchaseOrders.getUnit())).thenReturn(unitOfMeasures);
        when(configurationService.getSupplier(returnPurchaseOrders.getPersonResponsibleId())).thenReturn(businessPartners);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(returnPurchaseOrders.getCompanyId(), "RPO")).thenReturn(result);

        List<Row> rowvalues = new ArrayList<>();
        row.put("ISOCode", "ComplaintID");
        row.put("destination", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        ResponseModel responseModel = new ResponseModel();
        responseModel.setStatusCode("F456");
        responseModel.setResult("result");
        responseModel.setErrorMessage("Error");
        when(httpService.callCpiFlow(any(), any(),any())).thenReturn(responseModel);
        when(httpService.getLogicalSystem(scpCfDestinationLoader, "destination")).thenReturn("ASDCLNT100");
        when(httpService.getTargetLogicalSystem(scpCfDestinationLoader, "destination")).thenReturn("ASDCLNT100");
        when(qualityNotificationService.getQualityNotificationDetailsByComplaintId(any())).thenReturn(qualityNotifications);
        returnPurchaseOrderService.createReturPurchaseOrder(returnPurchaseOrders);

    }

    @Test(expected = ServiceException.class)
    public void testCreateReturnPurchaseOrderIOException() throws IOException {
        qualityNotifications.setIdentifier("12");
        ScpCfDestinationLoader scpCfDestinationLoader = new ScpCfDestinationLoader();
        returnPurchaseOrders.setCompanyId("3");
        returnPurchaseOrders.setMaterialId("34");
        returnPurchaseOrders.setPlantId("3");
        returnPurchaseOrders.setSupplierId("5");
        returnPurchaseOrders.setStatusCode("200");
        returnPurchaseOrders.setPurchasingOrganizationId("4");
        returnPurchaseOrders.setItemNumber("2");
        returnPurchaseOrders.setPersonResponsibleId("2");
        returnPurchaseOrders.setUnit("ST");
        PurchaseOrganizations prOrg = Struct.create(PurchaseOrganizations.class);
        prOrg.setPurchaseOrganization("test");
        MaterialMasterGeneralDatas matiral = Struct.create(MaterialMasterGeneralDatas.class);
        matiral.setMaterialCode("Gun");
        masterData.setPurchaseOrg(prOrg);
        masterData.setMaterial(matiral);
        BusinessPartners businessPartners = Struct.create(BusinessPartners.class);
        businessPartners.setBusinessPartnerNumber("2");
        when(configurationService.getCompanyCodes(returnPurchaseOrders.getCompanyId())).thenReturn(companycode);
        when(configurationService.getMasterDataDetails(returnPurchaseOrders.getMaterialId(), returnPurchaseOrders.getPlantId()
                , returnPurchaseOrders.getSupplierId(), returnPurchaseOrders.getPurchasingOrganizationId())).thenReturn(masterData);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(returnPurchaseOrders.getCompanyId(), "QN")).thenReturn(result);
        when(unitOfMeasureService.getUnitOfMeasureDetails(returnPurchaseOrders.getUnit())).thenReturn(unitOfMeasures);
        when(configurationService.getSupplier(returnPurchaseOrders.getPersonResponsibleId())).thenReturn(businessPartners);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(returnPurchaseOrders.getCompanyId(), "RPO")).thenReturn(result);

        List<Row> rowvalues = new ArrayList<>();
        row.put("ISOCode", "ComplaintID");
        row.put("destination", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);

        when(httpService.getLogicalSystem(scpCfDestinationLoader, "destination")).thenReturn("ASDCLNT100");
        when(httpService.getTargetLogicalSystem(scpCfDestinationLoader, "destination")).thenReturn("ASDCLNT100");
        when(qualityNotificationService.getQualityNotificationDetailsByComplaintId(any())).thenReturn(qualityNotifications);
        when(httpService.callCpiFlow(any(), any(),any())).thenThrow(IOException.class);
        returnPurchaseOrderService.createReturPurchaseOrder(returnPurchaseOrders);

    }

    @Test
    public void createReturnPORequestTest() {
        returnPurchaseOrders.setCompanyId("3");
        returnPurchaseOrders.setMaterialId("34");
        returnPurchaseOrders.setPlantId("3");
        returnPurchaseOrders.setSupplierId("5");
        returnPurchaseOrders.setStatusCode("200");
        returnPurchaseOrders.setPurchasingOrganizationId("4");
        returnPurchaseOrders.setItemNumber("2");
        returnPurchaseOrders.setPersonResponsibleId("2");
        returnPurchaseOrders.setUnit("ST");
        PurchaseOrganizations prOrg = Struct.create(PurchaseOrganizations.class);
        prOrg.setPurchaseOrganization("test");
        MaterialMasterGeneralDatas matiral = Struct.create(MaterialMasterGeneralDatas.class);
        matiral.setMaterialCode("Gun");
        masterData.setPurchaseOrg(prOrg);
        masterData.setMaterial(matiral);
        BusinessPartners businessPartners = Struct.create(BusinessPartners.class);
        businessPartners.setBusinessPartnerNumber("2");
        when(configurationService.getCompanyCodes(returnPurchaseOrders.getCompanyId())).thenReturn(companycode);
        when(configurationService.getMasterDataDetails(returnPurchaseOrders.getMaterialId(), returnPurchaseOrders.getPlantId()
                , returnPurchaseOrders.getSupplierId(), returnPurchaseOrders.getPurchasingOrganizationId())).thenReturn(masterData);
        when(configurationService.getSupplier(returnPurchaseOrders.getPersonResponsibleId())).thenReturn(businessPartners);
        when(unitOfMeasureService.getUnitOfMeasureDetails(returnPurchaseOrders.getUnit())).thenReturn(unitOfMeasures);
        returnPurchaseOrderService.createReturnPORequest(returnPurchaseOrders);
    }

    @Test
    public void getReturnPurchaseOrderDetailsTest() {
        Optional<ReturnPurchaseOrders> emptyOpt = Optional.of(returnPurchaseOrders);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(ReturnPurchaseOrders.class)).thenReturn(emptyOpt);
        Result result1 = Mockito.mock(Result.class);
        when(returnPurchaseOrderDao.getReturnPurchaseOrderDetails("202")).thenReturn(result1);
        returnPurchaseOrderService.getReturnPurchaseOrderDetails("202");
    }

    @Test
    public void getReturnPurchaseOrderBasedOnIdTest() {
        Optional<ReturnPurchaseOrders> emptyOpt = Optional.of(returnPurchaseOrders);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(ReturnPurchaseOrders.class)).thenReturn(emptyOpt);
        Result result1 = Mockito.mock(Result.class);
        when(returnPurchaseOrderDao.getReturnPurchaseOrderBasedOnId("202")).thenReturn(result1);
        returnPurchaseOrderService.getReturnPurchaseOrderBasedOnId("202");
    }


    @Test
    public void validateReturnPurchaseOrderFieldsTest() {
        Optional<ReturnPurchaseOrders> emptyOpt = Optional.of(returnPurchaseOrders);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(ReturnPurchaseOrders.class)).thenReturn(emptyOpt);
        returnPurchaseOrderService.validateReturnPurchaseOrderFields(returnPurchaseOrders);
    }

    @Test
    public void checkIfReturnPOExistsBasedOnNumberTest() {
        Optional<ReturnPurchaseOrders> emptyOpt = Optional.of(returnPurchaseOrders);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(ReturnPurchaseOrders.class)).thenReturn(emptyOpt);
        Result result1 = Mockito.mock(Result.class);
        when(returnPurchaseOrderDao.checkIfReturnPOExistsBasedOnNumber(any(String.class))).thenReturn(result1);
        returnPurchaseOrderService.checkIfReturnPOExistsBasedOnNumber("returnPurchaseOrders");
    }

    @Test
    public void getReturnPurchaseOrderDetailsBasedOnComplaintIdTest() {
        Optional<ReturnPurchaseOrders> emptyOpt = Optional.of(returnPurchaseOrders);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(ReturnPurchaseOrders.class)).thenReturn(emptyOpt);
        Result result1 = Mockito.mock(Result.class);
        when(returnPurchaseOrderDao.getReturnPurchaseOrderDetailsBasedOnComplaintId(any(String.class))).thenReturn(result1);
        returnPurchaseOrderService.getReturnPurchaseOrderDetailsBasedOnComplaintId("returnPurchaseOrders");
    }

    @Test
    public void createDocumentFlowTest() {
        ReturnOrder returnOrder = new ReturnOrder();
        qualityNotifications.setIdentifier("123");
        Optional<ReturnPurchaseOrders> emptyOpt = Optional.of(returnPurchaseOrders);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(ReturnPurchaseOrders.class)).thenReturn(emptyOpt);
        when(qualityNotificationService.getQualityNotificationDetailsByComplaintId(any(String.class))).thenReturn(qualityNotifications);
        Result result1 = Mockito.mock(Result.class);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(any(String.class), any(String.class))).thenReturn(result1);
        returnPurchaseOrderService.setRequestForDocFlow("tes", qualityNotifications, returnOrder, "test");
    }

    @Test
    public void setReturnPurchaseOrderDetailsTest() {
        ResponseModel responseModel = new ResponseModel();
        responseModel.setResult("9999");
        responseModel.setErrorMessage("");
        responseModel.setStatusCode("I0068");
        returnPurchaseOrderService.setReturnPurchaseOrderDetails(responseModel, returnPurchaseOrders);
    }

    @Test
    public void validateIfReturnPurchaseOrderExistsTest() {

        returnPurchaseOrderService.validateIfReturnPurchaseOrderExists(returnPurchaseOrders);
    }

    @Test
    public void createReturPurchaseOrderTest() {
        ScpCfDestinationLoader scpCfDestinationLoader = new ScpCfDestinationLoader();
        returnPurchaseOrders.setCompanyId("3");
        returnPurchaseOrders.setMaterialId("34");
        returnPurchaseOrders.setPlantId("3");
        returnPurchaseOrders.setSupplierId("5");
        returnPurchaseOrders.setStatusCode("200");
        returnPurchaseOrders.setPurchasingOrganizationId("4");
        returnPurchaseOrders.setItemNumber("2");
        returnPurchaseOrders.setPersonResponsibleId("2");returnPurchaseOrders.setUnit("ST");
        PurchaseOrganizations prOrg=Struct.create(PurchaseOrganizations.class);
        prOrg.setPurchaseOrganization("test");
        MaterialMasterGeneralDatas matiral=Struct.create(MaterialMasterGeneralDatas.class);
        matiral.setMaterialCode("Gun");
        masterData.setPurchaseOrg(prOrg);
        masterData.setMaterial(matiral);
        BusinessPartners businessPartners=Struct.create(BusinessPartners.class);
        businessPartners.setBusinessPartnerNumber("2");

        when(configurationService.getCompanyCodes(returnPurchaseOrders.getCompanyId())).thenReturn(companycode);
        when(configurationService.getMasterDataDetails(returnPurchaseOrders.getMaterialId(), returnPurchaseOrders.getPlantId()
                , returnPurchaseOrders.getSupplierId(), returnPurchaseOrders.getPurchasingOrganizationId())).thenReturn(masterData);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(returnPurchaseOrders.getCompanyId(), "QN")).thenReturn(result);
        when(unitOfMeasureService.getUnitOfMeasureDetails(returnPurchaseOrders.getUnit())).thenReturn(unitOfMeasures);
        when(configurationService.getSupplier(returnPurchaseOrders.getPersonResponsibleId())).thenReturn(businessPartners);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(returnPurchaseOrders.getCompanyId(), "RPO")).thenReturn(result);
        when(httpService.getLogicalSystem(scpCfDestinationLoader, "destination")).thenReturn("ASDCLNT100");
        when(httpService.getTargetLogicalSystem(scpCfDestinationLoader, "destination")).thenReturn("ASDCLNT100");
        when(commonFunctions.setBinaryRelationDataModel("1222", "QN", "RPO", "ASD", "ASD")).thenReturn(setRequest());
        returnPurchaseOrderService.createReturPurchaseOrder(returnPurchaseOrders);
    }

    @Test
    public void testValidateIfReturnPurchaseOrderExistsForComplaint() {
        returnPurchaseOrderService.validateIfReturnPurchaseOrderExistsForComplaint("1234");
    }

    @Test
    public void getReturnPurchaseOrderDetailsBasedOnNumberTest() {
        Optional<ReturnPurchaseOrders> emptyOpt = Optional.of(returnPurchaseOrders);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(ReturnPurchaseOrders.class)).thenReturn(emptyOpt);
        returnPurchaseOrders.setStatusCode("200");
        Mockito.when(returnPurchaseOrderDao.checkIfReturnPOExistsBasedOnNumber(any(String.class))).thenReturn(result);
        returnPurchaseOrderService.getReturnPurchaseOrderDetailsBasedOnNumber("1234");
    }

    @Test
    public void getReturnPurchaseOrderDetailsBasedOnNumberNullTest() {
        Optional<ReturnPurchaseOrders> emptyOpt = Optional.of(returnPurchaseOrders);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(ReturnPurchaseOrders.class)).thenReturn(emptyOpt);
        returnPurchaseOrders.setStatusCode("200");
        returnPurchaseOrders.setIdentifier("30");
        Mockito.when(returnPurchaseOrderDao.checkIfReturnPOExistsBasedOnNumber(any(String.class))).thenReturn(result);
        List<ReturnPurchaseOrders> businessPartnersList = new ArrayList<>();
        businessPartnersList.add(returnPurchaseOrders);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        Row row = Struct.create(Row.class);
        row.put("identifier", "30");
        when(result.listOf(ReturnPurchaseOrders.class)).thenReturn(businessPartnersList);
        when(result.first(ReturnPurchaseOrders.class)).thenReturn(emptyOpt);
        Mockito.when(configurationDao.getSupplierData(any(String.class))).thenReturn(result);
        when(result.first()).thenReturn(Optional.of(row));
        returnPurchaseOrderService.getReturnPurchaseOrderDetailsBasedOnNumber("12");
    }

    @Test
    public void getDraftReturnOrderByComplaintIDTest() {
        returnPurchaseOrders.setComplaintId("101");
        Optional<cds.gen.managereturnpurchaseorderservice.ReturnPurchaseOrders> opt = Optional.empty();
        when(db.run(any(CqnSelect.class), any(Object[].class))).thenReturn(result);
        returnPurchaseOrderService.getDraftReturnOrderByComplaintID(anyString());
    }
    @Test
    public void getActiveReturnPurchaseOrdersTest() {
        Optional<cds.gen.managereturnpurchaseorderservice.ReturnPurchaseOrders> opt = Optional.empty();
        when(db.run(any(CqnSelect.class), any(Object[].class))).thenReturn(result);
        returnPurchaseOrderService.getActiveReturnPurchaseOrders(anyString());
    }


    @Test
    public void getReturnPurchaseOrderDetailsBasedOnComplaintIdNullTest() {
        Optional<ReturnPurchaseOrders> emptyOpt = Optional.empty();
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(ReturnPurchaseOrders.class)).thenReturn(emptyOpt);
        Result result1 = Mockito.mock(Result.class);
        when(returnPurchaseOrderDao.getReturnPurchaseOrderDetailsBasedOnComplaintId(any(String.class))).thenReturn(result1);
        returnPurchaseOrderService.getReturnPurchaseOrderDetailsBasedOnComplaintId("returnPurchaseOrders");
    }

    @Test
    public void checkIfReturnPOExistsBasedOnNumberNullTest() {
        Optional<ReturnPurchaseOrders> emptyOpt = Optional.empty();
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(ReturnPurchaseOrders.class)).thenReturn(emptyOpt);
        Result result1 = Mockito.mock(Result.class);
        when(returnPurchaseOrderDao.checkIfReturnPOExistsBasedOnNumber(any(String.class))).thenReturn(result1);
        returnPurchaseOrderService.checkIfReturnPOExistsBasedOnNumber("returnPurchaseOrders");
    }

    @Test
    public void getReturnPurchaseOrderDetailsNullTest() {
        Optional<ReturnPurchaseOrders> emptyOpt = Optional.empty();
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(ReturnPurchaseOrders.class)).thenReturn(emptyOpt);
        Result result1 = Mockito.mock(Result.class);
        when(returnPurchaseOrderDao.getReturnPurchaseOrderDetails("202")).thenReturn(result1);
        returnPurchaseOrderService.getReturnPurchaseOrderDetails("202");
    }

    @Test
    public void createReturnPORequestElseTest() {
        returnPurchaseOrders.setItemNumber("2");
        PurchaseOrganizations prOrg = Struct.create(PurchaseOrganizations.class);
        prOrg.setPurchaseOrganization("test");
        MaterialMasterGeneralDatas matiral = Struct.create(MaterialMasterGeneralDatas.class);
        matiral.setMaterialCode("Gun");
        masterData.setPurchaseOrg(prOrg);
        masterData.setMaterial(matiral);
        BusinessPartners businessPartners = Struct.create(BusinessPartners.class);
        businessPartners.setBusinessPartnerNumber("2");
        when(configurationService.getCompanyCodes(returnPurchaseOrders.getCompanyId())).thenReturn(companycode);
        when(configurationService.getMasterDataDetails(returnPurchaseOrders.getMaterialId(), returnPurchaseOrders.getPlantId()
                , returnPurchaseOrders.getSupplierId(), returnPurchaseOrders.getPurchasingOrganizationId())).thenReturn(masterData);
        when(configurationService.getSupplier(returnPurchaseOrders.getPersonResponsibleId())).thenReturn(businessPartners);
        when(unitOfMeasureService.getUnitOfMeasureDetails(returnPurchaseOrders.getUnit())).thenReturn(unitOfMeasures);
        returnPurchaseOrderService.createReturnPORequest(returnPurchaseOrders);
    }


    private BinaryRelationDataModel setRequest() {
        BinaryRelationDataModel binaryRelationDataModel = new BinaryRelationDataModel();
        binaryRelationDataModel.setObjectA(new BinaryRelationObject());
        binaryRelationDataModel.setObjectB(new BinaryRelationObject());
        binaryRelationDataModel.setRelationType(Constants.BINARY_RELATIONSHIP_TYPE);
        return binaryRelationDataModel;
    }


    @Test
    public void getReturnOrderStatusAndCompanyCodeTest() {
        returnPurchaseOrders.setId("111");
        List<ReturnPurchaseOrders> list = new ArrayList<>();
        list.add(returnPurchaseOrders);
        List<Row> rowvalues = new ArrayList<>();
        row.put("destination", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(opt);
        Mockito.when(result.listOf(ReturnPurchaseOrders.class)).thenReturn(list);
        when(returnPurchaseOrderDao.getReturnOrderStatusAndCompanyCode(returnPurchaseOrders.getId())).thenReturn(result);
        returnPurchaseOrderService.getReturnOrderStatusAndCompanyCode(returnPurchaseOrders.getId());
    }
    @Test
    public void testDeleteDraftReturnOrderByID() {
        returnPurchaseOrderService.deleteDraftReturnOrderByID(returnPurchaseOrders.getId());
    }
}
