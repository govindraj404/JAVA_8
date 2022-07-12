package com.sap.ic.cmh.configuration.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import com.sap.cloud.sdk.cloudplatform.connectivity.Destination;
import com.sap.cloud.sdk.cloudplatform.connectivity.ScpCfDestination;
import com.sap.cloud.sdk.cloudplatform.connectivity.ScpCfDestinationLoader;
import com.sap.ic.cmh.configuration.model.MasterData;
import com.sap.ic.cmh.configuration.validations.MasterDataValidation;
import com.sap.ic.cmh.network.service.DestinationService;
import io.vavr.control.Try;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.persistency.BusinessObjectConfigurationDao;
import com.sap.ic.cmh.configuration.persistency.ClaimStatusMappingsDao;
import com.sap.ic.cmh.configuration.persistency.ConditionTypeDao;
import com.sap.ic.cmh.configuration.persistency.ConfigurationDao;
import com.sap.ic.cmh.configuration.persistency.DestinationConfigurationDao;
import com.sap.ic.cmh.configuration.persistency.ServiceMaterialDao;
import cds.gen.configurationservice.BusinessObjectConfigurations;
import cds.gen.configurationservice.ClaimStatusMappings;
import cds.gen.configurationservice.ConditionTypes;
import cds.gen.configurationservice.DestinationConfigurations;
import cds.gen.configurationservice.ServiceMaterials;
import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.masterdataservice.CompanyCodes;
import cds.gen.masterdataservice.MaterialMasterGeneralDatas;
import cds.gen.masterdataservice.Plants;
import cds.gen.masterdataservice.PurchaseOrganizations;
import org.springframework.beans.factory.annotation.Autowired;
import static org.junit.Assert.assertNull;

public class ConfigurationServiceTest {

	@Mock
	ScpCfDestination scpCfDestination;
	@Mock
	ConfigurationDao configurationDao;
	@InjectMocks
	ConfigurationService handler;
	@Mock
	public PersistenceService db;
	@Mock
	public Row row;
	@Mock
	Result result;
	@Mock
	private Messages messages;
	@Mock
	private Message msg;
	@Mock
	MasterDataValidation masterDataValidation;
	MaterialMasterGeneralDatas materialMasterGeneralDatas;
	Plants plants;
	BusinessPartners businessPartners;
	@Mock
	DestinationService destinationService;
	@Mock
	ScpCfDestinationLoader scpCfDestinationLoader;
	@Mock
	Try<Destination> destinationTry;
	@Mock
	Destination destination;
    @Mock
    BusinessObjectConfigurationDao businessObjectConfigurationDao;
    @Mock
    ClaimStatusMappingsDao claimStatusMappingsDao;
    @Mock
    ConditionTypeDao conditionTypeDao;
    @Mock
    DestinationConfigurationDao destinationConfigurationDao;
    @Mock
    ServiceMaterialDao serviceMaterialDao;

	private Optional<Row> opt;

    private BusinessObjectConfigurations businessObjectConfigurations;
    private ClaimStatusMappings claimStatusMappings;
    private ConditionTypes conditionTypes;
    private DestinationConfigurations destinationConfigurations;
    private ServiceMaterials serviceMaterials;
	private PurchaseOrganizations purchaseOrganizations;
    private List<BusinessObjectConfigurations> bolist = new ArrayList<>();
    private List<ClaimStatusMappings> claimStatusList = new ArrayList<>();
    private List<ConditionTypes> cList = new ArrayList<>();
    private List<DestinationConfigurations> dList = new ArrayList<>();
    private List<ServiceMaterials> sList = new ArrayList<>();
	private List<PurchaseOrganizations> pList = new ArrayList<>();
	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		materialMasterGeneralDatas = Struct.create(MaterialMasterGeneralDatas.class);
		materialMasterGeneralDatas.setId("10000001");
		plants = Struct.create(Plants.class);
		plants.setId("Fp01");
		businessPartners = Struct.create(BusinessPartners.class);
        businessObjectConfigurations = Struct.create(BusinessObjectConfigurations.class);
        businessObjectConfigurations.setId("123");
        bolist.add(businessObjectConfigurations);


        claimStatusMappings = Struct.create(ClaimStatusMappings.class);
        claimStatusMappings.setId("123");
        claimStatusList.add(claimStatusMappings);

        conditionTypes = Struct.create(ConditionTypes.class);
        conditionTypes.setId("123");
        cList.add(conditionTypes);

        destinationConfigurations = Struct.create(DestinationConfigurations.class);
        destinationConfigurations.setId("123");
        dList.add(destinationConfigurations);

        serviceMaterials = Struct.create(ServiceMaterials.class);
        serviceMaterials.setId("123");
        sList.add(serviceMaterials);
	}

	@Test
	public void testGetMasterDataDetailsByCodesTest() {
		Optional<MaterialMasterGeneralDatas> opt = Optional.of(materialMasterGeneralDatas);
		when(configurationDao.getMaterialDataBasedOnCode(any(String.class))).thenReturn(result);
		when(result.first(MaterialMasterGeneralDatas.class)).thenReturn(opt);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		Optional<Plants> opt1 = Optional.of(plants);
		when(configurationDao.getPlantDataBasedOnPlant(any(String.class))).thenReturn(result);
		when(result.first(Plants.class)).thenReturn(opt1);
		Optional<BusinessPartners> opt2 = Optional.of(businessPartners);
		when(configurationDao.getSupplierDataBasedOnNumber(any(String.class))).thenReturn(result);
		when(result.first(BusinessPartners.class)).thenReturn(opt2);
		when(configurationDao.getPersonResponsibleBasedOnNumber(any(String.class))).thenReturn(result);
		when(configurationDao.getPurchOrgDataBasedOnPurchOrg(any(String.class))).thenReturn(result);
		when(result.first(BusinessPartners.class)).thenReturn(opt2);
		handler.getMasterDataDetailsByCodes("20", "201", "203", "90","");
	}

	@Test
	public void testGetMasterDataDetailsTestEmptyResult() {
		Optional<MaterialMasterGeneralDatas> opt = Optional.of(materialMasterGeneralDatas);
		when(configurationDao.getMaterialData(any(String.class))).thenReturn(result);
		when(result.first(MaterialMasterGeneralDatas.class)).thenReturn(opt);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		Optional<Plants> opt1 = Optional.of(plants);
		when(configurationDao.getPlantData(any(String.class))).thenReturn(result);
		when(result.first(Plants.class)).thenReturn(opt1);
		Optional<BusinessPartners> opt2 = Optional.of(businessPartners);
		when(configurationDao.getSupplierData(any(String.class))).thenReturn(result);
		when(result.first(BusinessPartners.class)).thenReturn(opt2);
		when(configurationDao.getPurchOrgData(any(String.class))).thenReturn(result);
		when(result.first(BusinessPartners.class)).thenReturn(opt2);
		handler.getMasterDataDetails("20", "201", "203", "90");
	}

	@Test
	public void testGetMasterDataDetailsTest() {
		row.put("test", "test");
		Optional<Row> op = Optional.of(row);
		when(result.first()).thenReturn(op);
		MaterialMasterGeneralDatas comp = Struct.create(MaterialMasterGeneralDatas.class);
		comp.setMaterialCode("matCode");
		List<MaterialMasterGeneralDatas> compList = new ArrayList<>();
		compList.add(comp);
		when(result.listOf(MaterialMasterGeneralDatas.class)).thenReturn(compList);

		BusinessPartners bp = Struct.create(BusinessPartners.class);
		bp.setBusinessPartnerName1("bp");
		List<BusinessPartners> bpList = new ArrayList<>();
		bpList.add(bp);
		when(result.listOf(BusinessPartners.class)).thenReturn(bpList);

		Plants plant = Struct.create(Plants.class);
		plant.setPlant("plantq");
		List<Plants> plantList = new ArrayList<>();
		plantList.add(plant);
		when(result.listOf(Plants.class)).thenReturn(plantList);

		PurchaseOrganizations purOrg = Struct.create(PurchaseOrganizations.class);
		purOrg.setPurchaseOrganization("org");
		List<PurchaseOrganizations> purOrgList = new ArrayList<>();
		purOrgList.add(purOrg);
		when(result.listOf(PurchaseOrganizations.class)).thenReturn(purOrgList);

		Optional<MaterialMasterGeneralDatas> opt = Optional.of(materialMasterGeneralDatas);
		when(configurationDao.getMaterialData(any(String.class))).thenReturn(result);
		when(result.first(MaterialMasterGeneralDatas.class)).thenReturn(opt);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		Optional<Plants> opt1 = Optional.of(plants);
		when(configurationDao.getPlantData(any(String.class))).thenReturn(result);
		when(result.first(Plants.class)).thenReturn(opt1);
		Optional<BusinessPartners> opt2 = Optional.of(businessPartners);
		when(configurationDao.getSupplierData(any(String.class))).thenReturn(result);
		when(result.first(BusinessPartners.class)).thenReturn(opt2);
		when(configurationDao.getPurchOrgData(any(String.class))).thenReturn(result);
		when(result.first(BusinessPartners.class)).thenReturn(opt2);
		handler.getMasterDataDetails("20", "201", "203", "90");
	}

	@Test
	public void testGetCompanyCodesTestEmptyResult() {
		when(configurationDao.getCompanyCodes(any(String.class))).thenReturn(result);
		handler.getCompanyCodes("20");
	}

	@Test
	public void testGetCompanyCodesTest() {
		when(configurationDao.getCompanyCodes(any(String.class))).thenReturn(result);
		row.put("test", "test");
		Optional<Row> op = Optional.of(row);
		when(result.first()).thenReturn(op);
		CompanyCodes comp = Struct.create(CompanyCodes.class);
		comp.setCompanyCode("comp");
		List<CompanyCodes> compList = new ArrayList<>();
		compList.add(comp);
		when(result.listOf(CompanyCodes.class)).thenReturn(compList);
		handler.getCompanyCodes("20");
	}

	@Test
	public void testgGetSupplierTestEmptyResult() {
		when(configurationDao.getSupplierData(any(String.class))).thenReturn(result);
		handler.getSupplier("20");
	}

	@Test
	public void testgGetSupplierTest() {
		when(configurationDao.getSupplierData(any(String.class))).thenReturn(result);
		row.put("test", "test");
		Optional<Row> op = Optional.of(row);
		when(result.first()).thenReturn(op);
		BusinessPartners comp = Struct.create(BusinessPartners.class);
		comp.setBusinessPartnerName1("bp");
		List<BusinessPartners> compList = new ArrayList<>();
		compList.add(comp);
		when(result.listOf(BusinessPartners.class)).thenReturn(compList);
		handler.getSupplier("20");
	}


	@Test
	public void validateSupplierContactPersonTest() {
		when(configurationDao.getSupplierDataBasedOnNumber(any(String.class))).thenReturn(result);
		row.put("test", "test");
		Optional<Row> op = Optional.of(row);
		when(result.first()).thenReturn(op);
		BusinessPartners comp = Struct.create(BusinessPartners.class);
		comp.setBusinessPartnerName1("bp");
		List<BusinessPartners> compList = new ArrayList<>();
		compList.add(comp);
		when(result.listOf(BusinessPartners.class)).thenReturn(compList);
		handler.validateSupplierContactPerson("20");
	}

	@Test
	public void validateSupplierContactPersonElseTest() {
		when(configurationDao.getSupplierDataBasedOnNumber(any(String.class))).thenReturn(result);
		//row.put("test", "test");
		//Optional<Row> op = Optional.of(row);
		//when(result.first()).thenReturn(op);
		BusinessPartners comp = Struct.create(BusinessPartners.class);
		comp.setBusinessPartnerName1("bp");
		List<BusinessPartners> compList = new ArrayList<>();
		compList.add(comp);
		when(result.listOf(BusinessPartners.class)).thenReturn(compList);
		Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		handler.validateSupplierContactPerson("20");
	}
	@Test
	public void validateSupplierContactPersonElse1Test() {
		when(configurationDao.getSupplierDataBasedOnNumber(any(String.class))).thenReturn(result);
		BusinessPartners comp = Struct.create(BusinessPartners.class);
		comp.setBusinessPartnerName1("bp");
		List<BusinessPartners> compList = new ArrayList<>();
		compList.add(comp);
		when(result.listOf(BusinessPartners.class)).thenReturn(null);
		Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		handler.validateSupplierContactPerson("20");
	}
    @Test
	public void validateSupplierContactPersonNullTest() {
		when(configurationDao.getSupplierDataBasedOnNumber(any(String.class))).thenReturn(result);
		BusinessPartners comp = Struct.create(BusinessPartners.class);
		comp.setBusinessPartnerName1("bp");
		List<BusinessPartners> compList = new ArrayList<>();
		compList.add(comp);
		when(result.listOf(BusinessPartners.class)).thenReturn(null);
		Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		handler.validateSupplierContactPerson("");
	}

	@Test
	public void validatePersonResponsibleCodeTest() {
		when(configurationDao.getPersonResponsibleBasedOnNumber(any(String.class))).thenReturn(result);
		row.put("test", "test");
		Optional<Row> op = Optional.of(row);
		when(result.first()).thenReturn(op);
		BusinessPartners comp = Struct.create(BusinessPartners.class);
		comp.setBusinessPartnerName1("bp");
		List<BusinessPartners> compList = new ArrayList<>();
		compList.add(comp);
		when(result.listOf(BusinessPartners.class)).thenReturn(compList);
		handler.validatePersonResponsibleCode("20");
	}

	@Test
	public void validatePersonResponsibleCodeElseTest() {
		when(configurationDao.getPersonResponsibleBasedOnNumber(any(String.class))).thenReturn(result);
		//row.put("test", "test");
		//Optional<Row> op = Optional.of(row);
		//when(result.first()).thenReturn(op);
		BusinessPartners comp = Struct.create(BusinessPartners.class);
		comp.setBusinessPartnerName1("bp");
		List<BusinessPartners> compList = new ArrayList<>();
		compList.add(comp);
		when(result.listOf(BusinessPartners.class)).thenReturn(compList);
		Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		handler.validatePersonResponsibleCode("20");
	}
	@Test
	public void validatePersonResponsibleCodeElse1Test() {
		Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		handler.validatePersonResponsibleCode("");
	}

	@Test
	public void validateMaterialTest() {
		MasterData data=new MasterData();
		when(configurationDao.getMaterialDataBasedOnCode(any(String.class))).thenReturn(result);
		row.put("test", "test");
		Optional<Row> op = Optional.of(row);
		when(result.first()).thenReturn(op);
		MaterialMasterGeneralDatas comp = Struct.create(MaterialMasterGeneralDatas.class);
		comp.setMaterialCode("bp");
		List<MaterialMasterGeneralDatas> compList = new ArrayList<>();
		compList.add(comp);
		when(result.listOf(MaterialMasterGeneralDatas.class)).thenReturn(compList);
		handler.validateMaterial("20",data);
	}

	@Test
	public void validateMaterialNullTest() {
		MasterData data=new MasterData();
		handler.validateMaterial("",data);
	}

	@Test
	public void validatePurchaseOrganisationNullTest() {
		MasterData data=new MasterData();
		handler.validatePurchaseOrganisation("",data);
	}

	@Test
	public void validatePurchaseOrganisationTest() {
		MasterData data=new MasterData();
		when(configurationDao.getPurchOrgDataBasedOnPurchOrg(any(String.class))).thenReturn(result);
		row.put("test", "test");
		Optional<Row> op = Optional.of(row);
		when(result.first()).thenReturn(op);
		PurchaseOrganizations comp = Struct.create(PurchaseOrganizations.class);
		comp.setId("bp");
		List<PurchaseOrganizations> compList = new ArrayList<>();
		compList.add(comp);
		when(result.listOf(PurchaseOrganizations.class)).thenReturn(compList);
		handler.validatePurchaseOrganisation("20",data);
	}

	@Test
	public void validatePurchaseOrganisationElseTest() {
		MasterData data=new MasterData();
		when(configurationDao.getPurchOrgDataBasedOnPurchOrg(any(String.class))).thenReturn(result);
		Optional<Row> op = Optional.empty();
		when(result.first()).thenReturn(op);
		PurchaseOrganizations comp = Struct.create(PurchaseOrganizations.class);
		List<PurchaseOrganizations> compList = new ArrayList<>();
		compList.add(comp);
		when(result.listOf(PurchaseOrganizations.class)).thenReturn(compList);
		Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		handler.validatePurchaseOrganisation("20",data);
	}

	@Test
	public void validateSupplierNullTest() {
		MasterData data=new MasterData();
		Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		handler.validateSupplier(" ",data);
	}

	@Test
	public void validateSupplierElseTest() {
		MasterData data=new MasterData();
		when(configurationDao.getSupplierDataBasedOnNumber(any(String.class))).thenReturn(result);
		BusinessPartners comp = Struct.create(BusinessPartners.class);
		comp.setBusinessPartnerName1("bp");
		List<BusinessPartners> compList = new ArrayList<>();
		compList.add(comp);
		when(result.listOf(BusinessPartners.class)).thenReturn(null);
		Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		handler.validateSupplier("20",data);
	}
	@Test
	public void validateSupplierElse1Test() {
		MasterData data=new MasterData();
		when(configurationDao.getSupplierDataBasedOnNumber(any(String.class))).thenReturn(result);
		Optional<Row> op = Optional.of(row);
		when(result.first()).thenReturn(op);
		BusinessPartners comp = Struct.create(BusinessPartners.class);
		List<BusinessPartners> compList = new ArrayList<>();
		compList.add(comp);
		when(result.listOf(BusinessPartners.class)).thenReturn(compList);
		handler.validateSupplier("20",data);
	}


	@Test
	public void validatePlantNullTest() {
		MasterData data=new MasterData();
		Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		handler.validatePlant(" ",data);
	}

	@Test
	public void validatePlantElseTest() {
		MasterData data=new MasterData();
		when(configurationDao.getPlantDataBasedOnPlant(any(String.class))).thenReturn(result);
		Plants comp = Struct.create(Plants.class);
		comp.setId("bp");
		List<Plants> compList = new ArrayList<>();
		compList.add(comp);
		when(result.listOf(Plants.class)).thenReturn(compList);
		Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		handler.validatePlant("20",data);
	}
	@Test
	public void validatePlantElse1Test() {
		MasterData data=new MasterData();
		when(configurationDao.getPlantDataBasedOnPlant(any(String.class))).thenReturn(result);
		Optional<Row> op = Optional.of(row);
		when(result.first()).thenReturn(op);
		Plants comp = Struct.create(Plants.class);
		List<Plants> compList = new ArrayList<>();
		compList.add(comp);
		when(result.listOf(Plants.class)).thenReturn(compList);
		handler.validatePlant("20",data);
	}


	@Test
	public void validatePersonResponsibleTest() {
		MasterData data=new MasterData();
		when(configurationDao.getPersonResponsibleBasedOnNumber(any(String.class))).thenReturn(result);
		row.put("test", "test");
		Optional<Row> op = Optional.of(row);
		when(result.first()).thenReturn(op);
		BusinessPartners comp = Struct.create(BusinessPartners.class);
		comp.setBusinessPartnerName1("bp");
		List<BusinessPartners> compList = new ArrayList<>();
		compList.add(comp);
		when(result.listOf(BusinessPartners.class)).thenReturn(compList);
		handler.validatePersonResponsible("20",data);
	}

	@Test
	public void validatePersonResponsibleElseTest() {
		MasterData data=new MasterData();
		when(configurationDao.getPersonResponsibleBasedOnNumber(any(String.class))).thenReturn(result);
		BusinessPartners comp = Struct.create(BusinessPartners.class);
		comp.setBusinessPartnerName1("bp");
		List<BusinessPartners> compList = new ArrayList<>();
		compList.add(comp);
		when(result.listOf(BusinessPartners.class)).thenReturn(compList);
		Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		handler.validatePersonResponsible("20",data);
	}
	@Test
	public void validatePersonResponsibleElse1Test() {
		MasterData data=new MasterData();
		Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		handler.validatePersonResponsible("",data);
	}

	@Test
	public void getallDestinationsFromBTPTest() {
		Mockito.when(scpCfDestinationLoader.tryGetDestination(any(),any())).thenReturn(destinationTry);
		Mockito.when(destinationTry.get()).thenReturn(destination);
		ArrayList<ScpCfDestination> listScpDestination = new ArrayList<>();
		listScpDestination.add(scpCfDestination);
		Iterable<ScpCfDestination> itrIterable = listScpDestination;
		Try<Iterable<ScpCfDestination>> itr = Try.success(itrIterable);
		Mockito.when(destinationService.getallDestination(any())).thenReturn(itr);
		handler.getallDestinationsFromBTP();
	}


    @Test
    public void testGetBusinessObjectConfigurationsDetails(){
        row.put("BusinessObjectConfigurations", businessObjectConfigurations);
		opt = Optional.of(row);
        when(result.listOf(BusinessObjectConfigurations.class)).thenReturn(bolist);
		when(result.first()).thenReturn(opt);
        when(businessObjectConfigurationDao.getBusinessObjectConfigurationsDetails(businessObjectConfigurations.getId())).thenReturn(result);
        BusinessObjectConfigurations busConfigurationsDetails = handler.getBusinessObjectConfigurationsDetails(businessObjectConfigurations.getId());
    }

    @Test
    public void testGetBusinessObjectConfigurationsDetailsForNull() {
        when(businessObjectConfigurationDao.getBusinessObjectConfigurationsDetails(businessObjectConfigurations.getId())).thenReturn(result);
        when(result.listOf(BusinessObjectConfigurations.class)).thenReturn(bolist);
        assertNull(handler.getBusinessObjectConfigurationsDetails(businessObjectConfigurations.getId()));
    }

    @Test
    public void testGetclaimStatusMappingsDetails(){
        row.put("ClaimStatusMappings", claimStatusMappings);
		opt = Optional.of(row);
        when(result.listOf(ClaimStatusMappings.class)).thenReturn(claimStatusList);
		when(result.first()).thenReturn(opt);
        when(claimStatusMappingsDao.getClaimStatusMappingDetails(claimStatusMappings.getId())).thenReturn(result);
        ClaimStatusMappings claimDetails = handler.getclaimStatusMappingsDetails(claimStatusMappings.getId());
    }

    @Test
    public void testGetclaimStatusMappingsDetailsForNull() {
        when(claimStatusMappingsDao.getClaimStatusMappingDetails(claimStatusMappings.getId())).thenReturn(result);
        when(result.listOf(ClaimStatusMappings.class)).thenReturn(claimStatusList);
        assertNull(handler.getclaimStatusMappingsDetails(claimStatusMappings.getId()));
    }

    @Test
    public void testGetConditionTypeDetail(){
        row.put("ConditionTypes", conditionTypes);
		opt = Optional.of(row);
        when(result.listOf(ConditionTypes.class)).thenReturn(cList);
		when(result.first()).thenReturn(opt);
        when(conditionTypeDao.getConditionTypesDetail(conditionTypes.getId())).thenReturn(result);
        ConditionTypes cDetails = handler.getConditionTypeDetail(conditionTypes.getId());
    }

    @Test
    public void testGetConditionTypeDetailForNull() {
        when(conditionTypeDao.getConditionTypesDetail(conditionTypes.getId())).thenReturn(result);
        when(result.listOf(ConditionTypes.class)).thenReturn(cList);
        assertNull(handler.getConditionTypeDetail(conditionTypes.getId()));
    }

    @Test
    public void testGetDestinationConfigDetails(){
        row.put("DestinationConfigurations", destinationConfigurations);
		opt = Optional.of(row);
        when(result.listOf(DestinationConfigurations.class)).thenReturn(dList);
		when(result.first()).thenReturn(opt);
        when(destinationConfigurationDao.getDestinationConfigDetail(destinationConfigurations.getId())).thenReturn(result);
        DestinationConfigurations dDetails = handler.getDestinationConfigDetails(destinationConfigurations.getId());
    }

    @Test
    public void testGetDestinationConfigDetailsForNull(){

        
        when(destinationConfigurationDao.getDestinationConfigDetail(destinationConfigurations.getId())).thenReturn(result);
        when(result.listOf(DestinationConfigurations.class)).thenReturn(dList);
        assertNull(handler.getDestinationConfigDetails(destinationConfigurations.getId()));
    }

	@Test
	public void testGetServiceMaterialsDetails(){
		row.put("ServiceMaterials", serviceMaterials);
		opt = Optional.of(row);
		when(result.listOf(ServiceMaterials.class)).thenReturn(sList);
		when(result.first()).thenReturn(opt);
		when(serviceMaterialDao.getServiceMaterialsDetail(serviceMaterials.getId())).thenReturn(result);
		ServiceMaterials dDetails = handler.getServiceMaterialsDetails(serviceMaterials.getId());
	}

	@Test
	public void testGetServiceMaterialsDetailsForNull(){
		when(serviceMaterialDao.getServiceMaterialsDetail(serviceMaterials.getId())).thenReturn(result);
		when(result.listOf(ServiceMaterials.class)).thenReturn(sList);
		assertNull(handler.getServiceMaterialsDetails(serviceMaterials.getId()));
	}



}
