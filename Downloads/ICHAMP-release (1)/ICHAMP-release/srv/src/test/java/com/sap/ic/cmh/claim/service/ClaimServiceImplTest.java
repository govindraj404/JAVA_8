package com.sap.ic.cmh.claim.service;

import cds.gen.claimservice.Claims;
import cds.gen.configurationservice.BusinessObjectConfigurations;
import cds.gen.costcollectorservice.CostCollectors;
import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.masterdataservice.MaterialMasterGeneralDatas;
import cds.gen.masterdataservice.Plants;
import cds.gen.masterdataservice.PurchaseOrganizations;
import cds.gen.masterdataservice.UnitOfMeasures;
import cds.gen.qualitynotificationservice.QualityNotifications;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cloud.sdk.cloudplatform.connectivity.ScpCfDestinationLoader;
import com.sap.ic.cmh.businessobjects.service.BusinessObjectService;
import com.sap.ic.cmh.claim.model.ClaimDTO;
import com.sap.ic.cmh.claim.model.ClaimItem;
import com.sap.ic.cmh.claim.model.ResponseModel;
import com.sap.ic.cmh.claim.model.binary_relation.BinaryRelationDataModel;
import com.sap.ic.cmh.claim.model.binary_relation.BinaryRelationObject;
import com.sap.ic.cmh.claim.persistency.ClaimDao;
import com.sap.ic.cmh.claim.validations.ClaimValidation;
import com.sap.ic.cmh.configuration.model.MasterData;
import com.sap.ic.cmh.configuration.persistency.BusinessObjectConfigurationDao;
import com.sap.ic.cmh.configuration.persistency.ConditionTypeDao;
import com.sap.ic.cmh.configuration.persistency.DestinationConfigurationDao;
import com.sap.ic.cmh.configuration.persistency.ServiceMaterialDao;
import com.sap.ic.cmh.configuration.service.ConfigurationService;
import com.sap.ic.cmh.costcollector.service.CostCollectorService;

import com.sap.ic.cmh.masterdata.unitofmeasure.service.UnitOfMeasureService;
import com.sap.ic.cmh.network.service.HttpService;
import com.sap.ic.cmh.qualitynotification.service.QualityNotificationService;
import com.sap.ic.cmh.utils.CommonFunctions;
import com.sap.ic.cmh.utils.Constants;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.IOException;
import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Stream;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class ClaimServiceImplTest {

    @InjectMocks
    @Autowired
    ClaimServiceImpl claimService;

    @Mock
    ClaimValidation claimValidator;

    @Mock
    ClaimDao claimDao;

    @Mock
    Result result;

    @Mock
    DestinationConfigurationDao destinationConfigDao;

    @Mock
    ConfigurationService configurationService;

    @Mock
    ServiceMaterialDao serviceMaterialDao;

    @Mock
    CommonFunctions commonFunctions;

    @Mock
    ConditionTypeDao conditionTypeDao;

    @Mock
    CostCollectorService costCollectorService;

    @Mock
    BusinessObjectService businessObjectService;

    @Mock
    Messages messages;

    @Mock
    Stream<Message> stream;

    @Mock
    QualityNotificationService qualityNotificationService;

    @Mock
    HttpService httpService;

    @Mock
    BusinessObjectConfigurationDao businessObjectConfigurationDao;

    @Mock
    UnitOfMeasureService unitOfMeasureService;

    Map<String, Object> claimRequestMap = new HashMap<>();
    private QualityNotifications qualityNotification;
    private Claims claim;
    private List<Claims> claimList = new ArrayList<>();
    private Row row;
    private Optional<Row> opt;
    private BusinessPartners partners;
    private MasterData masterDataDetails;
    private List<CostCollectors> cost = new ArrayList<>();
    private CostCollectors costCollectors;
    UnitOfMeasures unitOfMeasures;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        claim = Struct.create(Claims.class);
        claim.setItemTypeCode("I01");
        claim.setClaimType("C01");
        claim.setSupplierRole("Manager");
        claim.setVersionCategory("101");
        claim.setId("3456");
        claim.setQuantity(new BigDecimal(100));
        claim.setComplaintId("2345");
        claim.setPersonResponsibleId("101");
        claim.setUnit("ST");
        

        claimList.add(claim);

        row = Struct.create(Row.class);
        partners = Struct.create(BusinessPartners.class);
        partners.setBusinessPartnerNumber("1234");
        partners.setId("234");

        masterDataDetails = new MasterData();

        MaterialMasterGeneralDatas datas = Struct.create(MaterialMasterGeneralDatas.class);
        datas.setMaterialCode("1234");
        datas.setId("1234");

        Plants plant = Struct.create(Plants.class);
        plant.setId("11234");
        plant.setPlant("457");

        PurchaseOrganizations pos = Struct.create(PurchaseOrganizations.class);
        pos.setPurchaseOrganization("457665");
        pos.setId("2345");

        masterDataDetails.setMaterial(datas);
        masterDataDetails.setSupplier(partners);
        masterDataDetails.setPlants(plant);
        masterDataDetails.setPurchaseOrg(pos);

        costCollectors = Struct.create(CostCollectors.class);
        costCollectors.setId("1234");
        costCollectors.setClaim("CLM");
        costCollectors.setQuantity(BigDecimal.ONE);
        costCollectors.setUnitCode("ST");
        costCollectors.setTotalCost(BigDecimal.TEN);
        cost.add(costCollectors);

        qualityNotification = Struct.create(QualityNotifications.class);
        qualityNotification.setComplaintId("1234");
        qualityNotification.setId("101");
        qualityNotification.setIdentifier("1234");

        claimRequestMap.put("1", new Object());
        
        unitOfMeasures = Struct.create(UnitOfMeasures.class);
		unitOfMeasures.setCode("ST");
		unitOfMeasures.setISOCode("PCE");
    }

    @Test
    public void testGetClaim() {
        when(claimDao.getClaim(any(String.class))).thenReturn(result);
        when(result.listOf(Claims.class)).thenReturn(claimList);
        List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "ClaimID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        assertEquals(claim, claimService.getClaim(claim.getComplaintId()));
    }

    @Test
    public void testGetClaimBasedOnId() {

        when(claimDao.getClaimBasedOnId(claim.getId())).thenReturn(result);
        when(result.listOf(Claims.class)).thenReturn(claimList);
        List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "ClaimID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        claimService.getClaimBasedOnId(claim.getId());
    }

    @Test
    public void testGetClaimBasedOnIdForNull() {

        when(claimDao.getClaimBasedOnId(claim.getId())).thenReturn(result);
        when(result.listOf(Claims.class)).thenReturn(claimList);
        assertNull(claimService.getClaimBasedOnId(claim.getId()));
    }
    @Test
    public void testCreateClaimAtDestination() throws IOException {
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(
                claim.getCompanyId(), Constants.QUALITYNOTIFICATION_CODE)).thenReturn(result);
        when(configurationService.getSupplier(claim.getPersonResponsibleId())).thenReturn(partners);
        when(configurationService.getMasterDataDetails(claim.getMaterialId(), claim.getPlantId(),
                claim.getSupplierId(), claim.getPurchasingOrganizationId())).thenReturn(masterDataDetails);
        when(unitOfMeasureService.getUnitOfMeasureDetails(claim.getUnit())).thenReturn(unitOfMeasures);
        when(costCollectorService.selectTransferToClaimCostCollector(claim.getComplaintId())).thenReturn(cost);
        when(serviceMaterialDao.getServiceMaterialsBasedOnDestinationSubItemTypeAndItemType(any(), any(),any())).thenReturn(result);
        when(unitOfMeasureService.getUnitOfMeasureDetails(claim.getUnit())).thenReturn(unitOfMeasures);
        when(conditionTypeDao.getConditionTypesBasedOnDestinationAndItemType(any(),any())).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("destination", "destination");
        row.put("serviceMaterial", "serviceMaterial");
        row.put("conditionType", "conditionType");
        row.put("ISOCode", "PCE");
        opt = Optional.of(row);
        rowvalues.add(row);
        ResponseModel responseModel = new ResponseModel();
        responseModel.setResult("9999");
        responseModel.setErrorMessage("");
        responseModel.setStatusCode("B004");
        when(result.list()).thenReturn(rowvalues);

        when(result.first()).thenReturn(opt);
        when(commonFunctions.convertObjectToMap(any(ClaimDTO.class))).thenReturn(claimRequestMap);
        when(httpService.callCpiFlow(any(), any(), any())).thenReturn(responseModel);
        when(qualityNotificationService.getQualityNotificationDetailsByComplaintId(any(String.class))).thenReturn(qualityNotification);
        claimService.createClaimAtDestination(claim);
    }

    @Test
    public void testCreateClaimAtDestinationStatusCodeNull() throws IOException {
        ScpCfDestinationLoader scpCfDestinationLoader = new ScpCfDestinationLoader();
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(
                claim.getCompanyId(), Constants.QUALITYNOTIFICATION_CODE)).thenReturn(result);
        when(configurationService.getSupplier(claim.getPersonResponsibleId())).thenReturn(partners);
        when(configurationService.getMasterDataDetails(claim.getMaterialId(), claim.getPlantId(),
                claim.getSupplierId(), claim.getPurchasingOrganizationId())).thenReturn(masterDataDetails);
        when(unitOfMeasureService.getUnitOfMeasureDetails(claim.getUnit())).thenReturn(unitOfMeasures);
        when(costCollectorService.selectTransferToClaimCostCollector(claim.getComplaintId())).thenReturn(cost);
        when(serviceMaterialDao.getServiceMaterialsBasedOnDestinationSubItemTypeAndItemType(any(), any(),any())).thenReturn(result);
        when(unitOfMeasureService.getUnitOfMeasureDetails(claim.getUnit())).thenReturn(unitOfMeasures);
        when(conditionTypeDao.getConditionTypesBasedOnDestinationAndItemType(any(),any())).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("destination", "destination");
        row.put("serviceMaterial", "serviceMaterial");
        row.put("conditionType", "conditionType");
        row.put("ISOCode", "PCE");
        opt = Optional.of(row);
        rowvalues.add(row);
        ResponseModel responseModel = new ResponseModel();
        responseModel.setResult("9999");
        responseModel.setErrorMessage("");
        responseModel.setStatusCode("B004");
        when(result.list()).thenReturn(rowvalues);

        when(result.first()).thenReturn(opt);
        when(httpService.getLogicalSystem(scpCfDestinationLoader, "destination")).thenReturn("ASDCLNT100");
        when(httpService.getTargetLogicalSystem(scpCfDestinationLoader, "destination")).thenReturn("ASDCLNT100");
        when(commonFunctions.setBinaryRelationDataModel("1222", "QN", "RPO", "ASD", "ASD")).thenReturn(setRequest());
        when(commonFunctions.convertObjectToMap(any(ClaimDTO.class))).thenReturn(claimRequestMap);
        when(httpService.callCpiFlow("/http/createClaimCost", claimRequestMap, "destination")).thenReturn(responseModel);
        when(qualityNotificationService.getQualityNotificationDetailsByComplaintId(any(String.class))).thenReturn(qualityNotification);
        claimService.createClaimAtDestination(claim);
    }

    @Test
    public void testCreateClaimAtDestinationResponseModelNull() throws IOException {
        claim.setPersonResponsibleId("");
        claim.setUnit("");
        claim.setQuantity(null);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(
                claim.getCompanyId(), Constants.QUALITYNOTIFICATION_CODE)).thenReturn(result);
        when(configurationService.getSupplier(claim.getPersonResponsibleId())).thenReturn(partners);
        when(configurationService.getMasterDataDetails(claim.getMaterialId(), claim.getPlantId(),
                claim.getSupplierId(), claim.getPurchasingOrganizationId())).thenReturn(masterDataDetails);
        when(unitOfMeasureService.getUnitOfMeasureDetails("ST")).thenReturn(unitOfMeasures);
        when(costCollectorService.selectTransferToClaimCostCollector(claim.getComplaintId())).thenReturn(null);
        when(serviceMaterialDao.getServiceMaterialsBasedOnDestinationSubItemTypeAndItemType("destination",
                costCollectors.getSubItemTypeCode(),
                costCollectors.getItemTypeCode())).thenReturn(result);
        when(conditionTypeDao.getConditionTypesBasedOnDestinationAndItemType(any(),any())).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("destination", "destination");
        row.put("serviceMaterial", "serviceMaterial");
        row.put("conditionType", "conditionType");
        row.put("ISOCode", "PCE");

        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);

        when(result.first()).thenReturn(opt);
        when(commonFunctions.convertObjectToMap(any(ClaimDTO.class))).thenReturn(claimRequestMap);
        when(httpService.callCpiFlow("/http/createClaimCost", claimRequestMap, "destination")).thenReturn(null);
        when(qualityNotificationService.getQualityNotificationDetailsByComplaintId(any(String.class))).thenReturn(qualityNotification);
        claimService.createClaimAtDestination(claim);
    }

    @Test
    public void testCreateClaimAtDestinationException() throws IOException {
        costCollectors.setTotalCost(null);
        costCollectors.setQuantity(null);
        cost.add(costCollectors);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(
                claim.getCompanyId(), Constants.QUALITYNOTIFICATION_CODE)).thenReturn(result);
        when(configurationService.getSupplier(claim.getPersonResponsibleId())).thenReturn(partners);
        when(configurationService.getMasterDataDetails(claim.getMaterialId(), claim.getPlantId(),
                claim.getSupplierId(), claim.getPurchasingOrganizationId())).thenReturn(masterDataDetails);
        when(unitOfMeasureService.getUnitOfMeasureDetails(claim.getUnit())).thenReturn(unitOfMeasures);
        when(costCollectorService.selectTransferToClaimCostCollector(claim.getComplaintId())).thenReturn(cost);
        when(serviceMaterialDao.getServiceMaterialsBasedOnDestinationSubItemTypeAndItemType(any(), any(),any())).thenReturn(result);
        when(conditionTypeDao.getConditionTypesBasedOnDestinationAndItemType(any(),any())).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("destination", "destination");
        row.put("serviceMaterial", "serviceMaterial");
        row.put("conditionType", "conditionType");
        row.put("ISOCode", "PCE");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(commonFunctions.convertObjectToMap(any(ClaimDTO.class))).thenReturn(claimRequestMap);
        when(httpService.callCpiFlow(any(), any(), any())).thenThrow(IOException.class);
        claimService.createClaimAtDestination(claim);
    }

    @Test
    public void testSetConfiguredValuesElse() {
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(
                claim.getCompanyId(), Constants.CLAIM_CODE)).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("destination", "destination");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(businessObjectConfigurationDao
                .getBusinessObjectConfigBasedOnDestinationAndBOAndDest("F007", "boType",
                        "destination")).thenReturn(result);
        claimService.setConfiguredValues(claim, "boType", "F007");
    }

    @Test
    public void testSetConfiguredValuesIfPartClaimType() {
        BusinessObjectConfigurations configurations = Struct.create(BusinessObjectConfigurations.class);
        configurations.setId("677");
        configurations.setBusinessObjectAttributeCode("claimType");
        List<BusinessObjectConfigurations> businessObjectConfigurations = new ArrayList<>();
        businessObjectConfigurations.add(configurations);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(
                claim.getCompanyId(), Constants.CLAIM_CODE)).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("destination", "destination");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(businessObjectConfigurationDao
                .getBusinessObjectConfigBasedOnDestinationAndBOAndDest("F007", "boType",
                        "destination")).thenReturn(result);
        when(result.listOf(BusinessObjectConfigurations.class)).thenReturn(businessObjectConfigurations);
        claimService.setConfiguredValues(claim, "boType", "F007");
    }

    @Test
    public void testSetConfiguredValuesIfPartSupplierRole() {
        BusinessObjectConfigurations configurations = Struct.create(BusinessObjectConfigurations.class);
        configurations.setId("677");
        configurations.setBusinessObjectAttributeCode("supplierRole");
        List<BusinessObjectConfigurations> businessObjectConfigurations = new ArrayList<>();
        businessObjectConfigurations.add(configurations);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(
                claim.getCompanyId(), Constants.CLAIM_CODE)).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("destination", "destination");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(businessObjectConfigurationDao
                .getBusinessObjectConfigBasedOnDestinationAndBOAndDest("F007", "boType",
                        "destination")).thenReturn(result);
        when(result.listOf(BusinessObjectConfigurations.class)).thenReturn(businessObjectConfigurations);
        claimService.setConfiguredValues(claim, "boType", "F007");
    }

    @Test
    public void testSetConfiguredValuesIfPartVersionCategory() {
        BusinessObjectConfigurations configurations = Struct.create(BusinessObjectConfigurations.class);
        configurations.setId("677");
        configurations.setBusinessObjectAttributeCode("versionCategory");
        List<BusinessObjectConfigurations> businessObjectConfigurations = new ArrayList<>();
        businessObjectConfigurations.add(configurations);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(
                claim.getCompanyId(), Constants.CLAIM_CODE)).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("destination", "destination");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(businessObjectConfigurationDao
                .getBusinessObjectConfigBasedOnDestinationAndBOAndDest("F007", "boType",
                        "destination")).thenReturn(result);
        when(result.listOf(BusinessObjectConfigurations.class)).thenReturn(businessObjectConfigurations);
        claimService.setConfiguredValues(claim, "boType", "F007");
    }

    @Test
    public void testSetConfiguredValuesIfPartItemType() {
        BusinessObjectConfigurations configurations = Struct.create(BusinessObjectConfigurations.class);
        configurations.setId("677");
        configurations.setBusinessObjectAttributeCode("itemType");
        List<BusinessObjectConfigurations> businessObjectConfigurations = new ArrayList<>();
        businessObjectConfigurations.add(configurations);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(
                claim.getCompanyId(), Constants.CLAIM_CODE)).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("destination", "destination");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(businessObjectConfigurationDao
                .getBusinessObjectConfigBasedOnDestinationAndBOAndDest("F007", "boType",
                        "destination")).thenReturn(result);
        when(result.listOf(BusinessObjectConfigurations.class)).thenReturn(businessObjectConfigurations);
        claimService.setConfiguredValues(claim, "boType", "F007");
    }

    @Test
    public void testValidateClaimFields() {
        claimService.validateClaimFields(claim);
    }

    @Test
    public void testValidateClaimFieldsError() {
        when(messages.stream()).thenReturn(stream);
        when(stream.noneMatch(any())).thenReturn(false);
        claimService.validateClaimFields(claim);
    }

    @Test
    public void testValidateIfClaimExists() {
        claimService.validateIfClaimExists(claim);
    }

    @Test
    public void testValidateIfClaimExistsError() {
        when(messages.stream()).thenReturn(stream);
        when(stream.noneMatch(any())).thenReturn(false);
        claimService.validateIfClaimExists(claim);
    }

    @Test
    public void testValidateIfClaimExistsForComplaint() {
        claimService.validateIfClaimExistsForComplaint("1234");
    }

    @Test
    public void testValidateIfClaimExistsForComplaintError() {
        when(messages.stream()).thenReturn(stream);
        when(stream.noneMatch(any())).thenReturn(false);
        claimService.validateIfClaimExistsForComplaint("1234");
    }
    @Test
    public void getIsoCodeUnitNullTest(){
        ClaimItem item=new ClaimItem();
        claimService.getIsoCodeUnit(item,null);
    }
    @Test
    public void getIsoCodeUnitElseTest(){
        ClaimItem item=new ClaimItem();
        when(unitOfMeasureService.getUnitOfMeasureDetails(any())).thenReturn(unitOfMeasures);
        claimService.getIsoCodeUnit(item,"test");
    }
    
    private BinaryRelationDataModel setRequest() {
    	BinaryRelationDataModel binaryRelationDataModel = new BinaryRelationDataModel();
		binaryRelationDataModel.setObjectA(new BinaryRelationObject());
		binaryRelationDataModel.setObjectB(new BinaryRelationObject());
		binaryRelationDataModel.setRelationType(Constants.BINARY_RELATIONSHIP_TYPE);
		return binaryRelationDataModel;
    }

    @Test
    public void fetchConfiguredDestinationTest(){
        Claims claims=Struct.create(Claims.class);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(any(),any())).thenReturn(result);
        claimService.fetchConfiguredDestination(claims,"test");
    }

    @Test
    public void setServiceMaterialConfigDetailsTest(){
        ClaimItem claims=new ClaimItem();
        CostCollectors cost=Struct.create(CostCollectors.class);
        when(serviceMaterialDao.getServiceMaterialsBasedOnDestinationSubItemTypeAndItemType(any(),any(),any())).thenReturn(result);
        claimService.setServiceMaterialConfigDetails("",cost,claims);
    }


    @Test
    public void testSetConfiguredValuesnull() {
        BusinessObjectConfigurations configurations = Struct.create(BusinessObjectConfigurations.class);
        configurations.setId("677");
         configurations.setBusinessObjectAttributeCode("itemType");
        List<BusinessObjectConfigurations> businessObjectConfigurations = new ArrayList<>();
        businessObjectConfigurations.add(configurations);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(
                claim.getCompanyId(), Constants.CLAIM_CODE)).thenReturn(result);
        claimService.setConfiguredValues(claim, "boType", "F007");
    }

    @Test
    public void testGetClaimStatusAndCompanyCode() {
        List<Claims>  list = new ArrayList<>();
        claim.setCompanyId("111");
        claim.setStatusCode("CLMCRTD");
        list.add(claim);
        when(claimDao.getClaimStatusAndCompanyCode(claim.getId())).thenReturn(result);
        when(result.listOf(Claims.class)).thenReturn(list);
        List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "ClaimID");
        row.put("status_code", "CLMCRTD");
        row.put("company_ID", "111");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        claimService.getClaimStatusAndCompanyCode(claim.getId());
    }
    @Test
    public void testGetActiveClaimBasedOnId() {
        when(claimDao.getActiveClaimBasedOnId("1234")).thenReturn(result);
        claimService.getActiveClaimBasedOnId("1234");
    }

    @Test
    public void testGetDraftClaimByComplaintID() {
        when(claimDao.getDraftClaimByComplaintID("1234")).thenReturn(result);
        claimService.getDraftClaimByComplaintID("1234");
    }
    @Test
    public void testDeleteDraftClaimByID() {
        claimDao.deleteDraftClaimByID("1234");
        claimService.deleteDraftClaimByID("1234");
    }

    @Test
    public void testCheckIfClaimExistsBasedOnNumber() {
        when(claimDao.checkIfClaimExistsBasedOnNumber("1234")).thenReturn(result);
        claimService.checkIfClaimExistsBasedOnNumber("1234");
    }

    @Test
    public void testCheckIfClaimExistsBasedOnNumberResultNotNull() {
        when(claimDao.checkIfClaimExistsBasedOnNumber("1234")).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        claimService.checkIfClaimExistsBasedOnNumber("1234");
    }

}