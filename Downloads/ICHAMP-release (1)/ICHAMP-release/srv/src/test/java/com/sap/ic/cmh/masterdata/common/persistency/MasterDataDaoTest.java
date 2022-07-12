package com.sap.ic.cmh.masterdata.common.persistency;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.persistence.PersistenceService;

import cds.gen.masterdataservice.ActionPreconditions;
import cds.gen.masterdataservice.Actions;
import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.masterdataservice.CompanyCodes;
import cds.gen.masterdataservice.Countries;
import cds.gen.masterdataservice.Currencies;
import cds.gen.masterdataservice.DefectGroups;
import cds.gen.masterdataservice.ItemTypes;
import cds.gen.masterdataservice.MaterialMasterGeneralDatas;
import cds.gen.masterdataservice.Plants;
import cds.gen.masterdataservice.PurchaseOrganizations;

public class MasterDataDaoTest {

	@Mock
    private PersistenceService db;
    @InjectMocks
    private MasterDataDao masterDataDao;
    @Mock
    Result result;
    @Mock
    Runnable run;
    @Mock
    CdsCreateEventContext createContextMock;
    @Mock
    CqnInsert cqnInsert;
    @Mock
    Row row;
    
    private List<Plants> plantsList = new ArrayList<>();
    private Plants plants;
    private List<CompanyCodes> companyCodesList = new ArrayList<>();
    private CompanyCodes companyCodes;
    
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        
        plants =  Struct.create(Plants.class);
        plants.setId("100");
        plants.setPlant("F001");
        plants.setPlantName("test");
        plants.setCompanyCode("HY");
        plantsList.add(plants);
        
        companyCodes =  Struct.create(CompanyCodes.class);
        companyCodes.setId("COMP1");
        companyCodes.setCountryKey(null);
        companyCodes.setCompanyCode("F789");
        companyCodes.setCompanyCodeName("Company");
        companyCodesList.add(companyCodes);
    }

    @Test
    public void testGetCurrencies() {
    	List<Currencies> currenciesList = new ArrayList<>();
        Currencies currencies =  Struct.create(Currencies.class);
        currencies.setCode("100");
        currencies.setName("F001");
        currencies.setDescr("test");
        currenciesList.add(currencies);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.listOf(Currencies.class)).thenReturn(currenciesList);
		List<Row> rowvalues = new ArrayList<>();
		row.put("code", "100");
		row.put("name", "F001");
		row.put("descr", "test");
		Optional<Row> opt = Optional.of(row);
		rowvalues.add(row);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.list()).thenReturn(rowvalues);
		when(result.first()).thenReturn(opt); 
        masterDataDao.getCurrencies(currencies.getCode());
    }
    
    @Test
    public void testGetCurrenciesIsNull() {
        masterDataDao.getCurrencies(null);
    }
    
    @Test
    public void testGetCountryCode() {
        Countries countries =  Struct.create(Countries.class);
        countries.setCode("100");
        countries.setName("F001");
        countries.setDescr("test");
        Optional<Countries> opt = Optional.of(countries);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(Countries.class)).thenReturn(opt);
        masterDataDao.getCountryCode(countries.getCode());
    }
    
    @Test
    public void testGetCountryCodeIsNull() {
        masterDataDao.getCountryCode(null);
    }
    
    @Test
    public void testGetMaterial() {
    	List<MaterialMasterGeneralDatas> materialMasterGeneralDatasList = new ArrayList<>();
    	MaterialMasterGeneralDatas masterGeneralDatas =  Struct.create(MaterialMasterGeneralDatas.class);
        masterGeneralDatas.setId("100");
        masterGeneralDatas.setMaterialCode("F001");
        masterGeneralDatas.setMaterialType("test");
        materialMasterGeneralDatasList.add(masterGeneralDatas);        
        when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.listOf(MaterialMasterGeneralDatas.class)).thenReturn(materialMasterGeneralDatasList);
		List<Row> rowvalues = new ArrayList<>();
		row.put("ID", "100");
		row.put("materialCode", "F001");
		row.put("materialType", "test");
		Optional<Row> opt = Optional.of(row);
		rowvalues.add(row);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.list()).thenReturn(rowvalues);
		when(result.first()).thenReturn(opt); 
        masterDataDao.getMaterial(masterGeneralDatas.getId());
    }
    
    @Test
    public void testGetMaterialIsNull() {
    	MaterialMasterGeneralDatas masterGeneralDatas =  Struct.create(MaterialMasterGeneralDatas.class);
        Optional<MaterialMasterGeneralDatas> opt = Optional.of(masterGeneralDatas);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(MaterialMasterGeneralDatas.class)).thenReturn(opt);
        masterDataDao.getMaterial(null);
    }
    
    @Test
    public void testGetPlant() {
    	when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.listOf(Plants.class)).thenReturn(plantsList);
		List<Row> rowvalues = new ArrayList<>();
		row.put("ID", "100");
		row.put("plant", "F001");
		row.put("plantName", "test");
		Optional<Row> opt = Optional.of(row);
		rowvalues.add(row);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.list()).thenReturn(rowvalues);
		when(result.first()).thenReturn(opt); 
        masterDataDao.getPlant(plants.getId());
    }
    
    @Test
    public void testGetPlantIsNull() {
        Plants plants =  Struct.create(Plants.class);
        Optional<Plants> opt = Optional.of(plants);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(Plants.class)).thenReturn(opt);
        masterDataDao.getPlant(plants.getId());
    }
    
    @Test
    public void testValidateCompanyCodeBasedOnPlant() {
    	when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.listOf(Plants.class)).thenReturn(plantsList);
		List<Row> rowvalues = new ArrayList<>();
		row.put("ID", "100");
		row.put("plant", "F001");
		row.put("plantName", "test");
		Optional<Row> opt = Optional.of(row);
		rowvalues.add(row);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.list()).thenReturn(rowvalues);
		when(result.first()).thenReturn(opt); 
        masterDataDao.validateCompanyCodeBasedOnPlant(plants.getCompanyCode());
    }
    
    @Test
    public void testValidateCompanyCodeBasedOnPlantIsNull() {
        Plants plants =  Struct.create(Plants.class);
        Optional<Plants> opt = Optional.of(plants);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(Plants.class)).thenReturn(opt);
        masterDataDao.validateCompanyCodeBasedOnPlant(plants.getCompanyCode());
    }
    
    @Test
    public void testGetCompanyCodeBasedOnPlants() {
    	when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.listOf(Plants.class)).thenReturn(plantsList);
		List<Row> rowvalues = new ArrayList<>();
		row.put("ID", "100");
		row.put("plant", "F001");
		row.put("plantName", "test");
		Optional<Row> opt = Optional.of(row);
		rowvalues.add(row);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.list()).thenReturn(rowvalues);
		when(result.first()).thenReturn(opt); 
        masterDataDao.getCompanyCodeBasedOnPlants(plants.getCompanyCode());
    }
    
    @Test
    public void testGetCompanyCodeBasedOnPlanstIsNull() {
        Plants plants =  Struct.create(Plants.class);
        Optional<Plants> opt = Optional.of(plants);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(Plants.class)).thenReturn(opt);
        masterDataDao.getCompanyCodeBasedOnPlants(plants.getCompanyCode());
    }
    
    @Test
    public void testGetCompanyCodeID() {    	
        when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.listOf(CompanyCodes.class)).thenReturn(companyCodesList);
		List<Row> rowvalues = new ArrayList<>();
		row.put("ID", "COMP1");
		row.put("countryKey", null);
		row.put("companyCode", "F789");
		Optional<Row> opt = Optional.of(row);
		rowvalues.add(row);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.list()).thenReturn(rowvalues);
		when(result.first()).thenReturn(opt);
        masterDataDao.getCompanyCodeID(companyCodes.getCompanyCode());
    }

    @Test
    public void testGetCompanyCodeIDIsNull() {
        CompanyCodes companyCodes =  Struct.create(CompanyCodes.class);
        Optional<CompanyCodes> opt = Optional.of(companyCodes);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(CompanyCodes.class)).thenReturn(opt);
        masterDataDao.getCompanyCodeID(companyCodes.getCompanyCode());
    }
    
    @Test
    public void testGetCompanyCode() {
    	when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.listOf(CompanyCodes.class)).thenReturn(companyCodesList);
		List<Row> rowvalues = new ArrayList<>();
		row.put("ID", "COMP1");
		row.put("countryKey", null);
		row.put("companyCode", "F789");
		Optional<Row> opt = Optional.of(row);
		rowvalues.add(row);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.list()).thenReturn(rowvalues);
		when(result.first()).thenReturn(opt);        
        masterDataDao.getCompanyCode(companyCodes.getId());
    }

    @Test
    public void testGetCompanyCodeIsNull() {
    	CompanyCodes companyCodes =  Struct.create(CompanyCodes.class);
        Optional<CompanyCodes> opt = Optional.of(companyCodes);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(CompanyCodes.class)).thenReturn(opt);
        masterDataDao.getCompanyCode(companyCodes.getId());
    }

    @Test
    public void testGetPurchasingOrganization() {
    	List<PurchaseOrganizations> purchaseOrganizationsList = new ArrayList<>();
        PurchaseOrganizations purchaseOrganizations = Struct.create(PurchaseOrganizations.class);
        purchaseOrganizations.setCompanyCodeIDId("677");
        purchaseOrganizations.setCompanyCodeID(null);
        purchaseOrganizations.setPurchaseOrganizationName("AnyName");
        purchaseOrganizations.setPurchaseOrganization("NAP");
        purchaseOrganizations.setId("10001");
        purchaseOrganizations.setCompanyCode("F123");
        purchaseOrganizationsList.add(purchaseOrganizations);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.listOf(PurchaseOrganizations.class)).thenReturn(purchaseOrganizationsList);
		List<Row> rowvalues = new ArrayList<>();
		row.put("ID", "10001");
		row.put("companyCode", "F123");
		Optional<Row> opt = Optional.of(row);
		rowvalues.add(row);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.list()).thenReturn(rowvalues);
		when(result.first()).thenReturn(opt);        
        masterDataDao.getPurchasingOrganization(purchaseOrganizations.getId());
    }
    
    @Test
    public void testGetPurchasingOrganizationIsNull() {
        PurchaseOrganizations purchaseOrganizations = Struct.create(PurchaseOrganizations.class);
        Optional<CompanyCodes> opt = Optional.of(companyCodes);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(CompanyCodes.class)).thenReturn(opt);
        masterDataDao.getPurchasingOrganization(purchaseOrganizations.getId());
    }

    @Test
    public void testGetCurrency() {
    	List<cds.gen.sap.common.Currencies> currenciesList = new ArrayList<>();
    	cds.gen.sap.common.Currencies currencies =  Struct.create(cds.gen.sap.common.Currencies.class);
        currencies.setCode("100");
        currencies.setName("F001");
        currencies.setDescr("test");
        currenciesList.add(currencies);
        
        when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.listOf(cds.gen.sap.common.Currencies.class)).thenReturn(currenciesList);
		List<Row> rowvalues = new ArrayList<>();
		row.put("code", "100");
		row.put("name", "F001");
		row.put("descr", "test");
		Optional<Row> opt = Optional.of(row);
		rowvalues.add(row);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.list()).thenReturn(rowvalues);
		when(result.first()).thenReturn(opt);
        masterDataDao.getCurrency(currencies.getCode());
    }
    
    @Test
    public void testGetCurrencyIsNull() {
        Currencies currencies =  Struct.create(Currencies.class);
        Optional<Currencies> opt = Optional.of(currencies);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(Currencies.class)).thenReturn(opt);
        masterDataDao.getCurrency(currencies.getCode());
    }

    @Test
    public void testGetCurrencyBasedOnCompanyCodes() {
    	when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.listOf(CompanyCodes.class)).thenReturn(companyCodesList);
		List<Row> rowvalues = new ArrayList<>();
		row.put("ID", "COMP1");
		row.put("countryKey", null);
		row.put("companyCode", "F789");
		Optional<Row> opt = Optional.of(row);
		rowvalues.add(row);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.list()).thenReturn(rowvalues);
		when(result.first()).thenReturn(opt);
        masterDataDao.getCurrencyBasedOnCompanyCodes(companyCodes.getId());
    }
    
    @Test
    public void testGetCurrencyBasedOnCompanyCodesIsNull() {
    	CompanyCodes companyCodes =  Struct.create(CompanyCodes.class);
        Optional<CompanyCodes> opt = Optional.of(companyCodes);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(CompanyCodes.class)).thenReturn(opt);
        masterDataDao.getCurrencyBasedOnCompanyCodes(companyCodes.getId());
    }

    @Test
    public void testValidateCurrencyBasedOnCompanyCode() {
    	when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.listOf(CompanyCodes.class)).thenReturn(companyCodesList);
		List<Row> rowvalues = new ArrayList<>();
		row.put("ID", "COMP1");
		row.put("countryKey", null);
		row.put("companyCode", "F789");
		Optional<Row> opt = Optional.of(row);
		rowvalues.add(row);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.list()).thenReturn(rowvalues);
		when(result.first()).thenReturn(opt);
        masterDataDao.validateCurrencyBasedOnCompanyCode(companyCodes.getId());
    }
    
    @Test
    public void testValidateCurrencyBasedOnCompanyCodeIsNull() {
        CompanyCodes companyCodes =  Struct.create(CompanyCodes.class);
        Optional<CompanyCodes> opt = Optional.of(companyCodes);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(CompanyCodes.class)).thenReturn(opt);
        masterDataDao.validateCurrencyBasedOnCompanyCode(companyCodes.getId());
    }

    @Test
    public void testGetDefectGroup() {
        DefectGroups defectGroups = Struct.create(DefectGroups.class);
        defectGroups.setCode("Code");
        defectGroups.setDescription("description");
        Optional<DefectGroups> opt = Optional.of(defectGroups);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(DefectGroups.class)).thenReturn(opt);
        masterDataDao.getDefectGroup(defectGroups.getCode());
    }
    
    @Test
    public void testGetDefectGroupIsNull() {
        masterDataDao.getDefectGroup(null);
    }

    @Test
    public void testGetItemTypeCode() {
        ItemTypes itemTypes = Struct.create(ItemTypes.class);
        itemTypes.setCode("Code");
        itemTypes.setDescription("description");
        Optional<ItemTypes> opt = Optional.of(itemTypes);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(ItemTypes.class)).thenReturn(opt);
        masterDataDao.getItemTypeCode(itemTypes.getCode());
    }
    
    @Test
    public void testGetItemTypeCodeIsNull() {
        masterDataDao.getItemTypeCode(null);
    }

    @Test
    public void testGetAction() {
        Actions actions = Struct.create(Actions.class);
        actions.setCode("Code");
        actions.setName("name");
        Optional<Actions> opt = Optional.of(actions);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(Actions.class)).thenReturn(opt);
        masterDataDao.getAction(actions.getCode());
    }

    @Test
    public void testGetActionPrecondition() {
        ActionPreconditions actionPreconditions = Struct.create(ActionPreconditions.class);
        actionPreconditions.setCodeCode("Code");
        actionPreconditions.setBusinessObjectStatusBusinessObjectType(null);
        Optional<ActionPreconditions> opt = Optional.of(actionPreconditions);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(ActionPreconditions.class)).thenReturn(opt);
        masterDataDao.getActionPrecondition(actionPreconditions.getCodeCode());
    }
    
    @Test
    public void testGeSupplier() {
    	List<BusinessPartners> suppliersList = new ArrayList<>();
        BusinessPartners supplier = Struct.create(BusinessPartners.class);
        supplier.setId("supplierID");
        supplier.setCustomerCode("CUST01");
        supplier.setVendorCode("VEND01");
        supplier.setCompanyCode("HY");
        suppliersList.add(supplier);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.listOf(BusinessPartners.class)).thenReturn(suppliersList);
		List<Row> rowvalues = new ArrayList<>();
		row.put("ID", "supplierID");
		row.put("customerCode", "CUST01");
		row.put("vendorCode", "VEND01");
		row.put("companyCode", "HY");
		Optional<Row> opt = Optional.of(row);
		rowvalues.add(row);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.list()).thenReturn(rowvalues);
		when(result.first()).thenReturn(opt); 
        masterDataDao.getSupplier(supplier.getId());
    }
    
    @Test
    public void testGeSupplierIsNull() {
        BusinessPartners supplier = Struct.create(BusinessPartners.class);  
        Optional<BusinessPartners> opt = Optional.of(supplier);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(BusinessPartners.class)).thenReturn(opt);        
        masterDataDao.getSupplier(supplier.getId());
    }

}