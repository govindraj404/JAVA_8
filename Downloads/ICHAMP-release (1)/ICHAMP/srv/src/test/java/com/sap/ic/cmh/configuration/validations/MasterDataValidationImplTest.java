package com.sap.ic.cmh.configuration.validations;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.complaint.service.ComplaintService;
import com.sap.ic.cmh.configuration.persistency.ConfigurationDao;
import com.sap.ic.cmh.masterdata.businesspartner.repository.BusinessPartnerRepository;
import com.sap.ic.cmh.masterdata.distributionchannel.persistency.DistributionChannelRepository;
import com.sap.ic.cmh.masterdata.division.persistency.DivisionRepository;
import com.sap.ic.cmh.masterdata.salesorganization.persistency.SalesOrganizationRepository;
import com.sap.ic.cmh.masterdata.unitofmeasure.service.UnitOfMeasureService;

import cds.gen.complaintservice.BTPUsers;
import cds.gen.configurationservice.ConditionTypes;
// import cds.gen.configurationservice.DestinationMappings_;
import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.masterdataservice.CompanyCodes;
import cds.gen.masterdataservice.MaterialMasterGeneralDatas;
import cds.gen.masterdataservice.Plants;
import cds.gen.masterdataservice.PurchaseOrganizations;
import cds.gen.masterdataservice.UnitOfMeasures;

public class MasterDataValidationImplTest {
	@InjectMocks
	private MasterDataValidationImpl validator;
	@Mock
	ConfigurationDao configurationDao;
	@Mock
	UnitOfMeasureService unitOfMeasureService;
	@Mock
	Messages messages;
	@Mock
	Message msg;
	private ConditionTypes conditionTypes;
	@Mock
	private Result result;

	@Mock
	private Row row;
	@Mock
	BusinessPartnerRepository businessPartnerDao;
	@Mock
	ComplaintService complaintService;

	@Mock
	public PersistenceService db;
	List<BusinessPartners> businessPartnersLint=new ArrayList<>();
	BusinessPartners businessPartners;
	UnitOfMeasures unitOfMeasures;
	
    @Mock
    SalesOrganizationRepository salesOrganizationRepository;
    @Mock
    DistributionChannelRepository distributionChannelRepository;
    @Mock
    DivisionRepository divisionRepository;

	
	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		 businessPartners= Struct.create(BusinessPartners.class);
		businessPartners.setBusinessPartnerNumber("22");
		businessPartners.setBusinessPartnerType("test");
		businessPartners.setBusinessPartnerName1("2");
		businessPartners.setId("23");
		businessPartners.setAddress("GNT");
		businessPartners.setAddressIDId("GNT");
		businessPartners.setBusinessPartnerName2("3");
		businessPartners.setCompanyCode("303");
		businessPartners.setCustomerCode("34");
		businessPartners.setVendorCode("34");
		businessPartners.setIsMarkedForDeletion(true);
		List<BusinessPartners> businessPartnersLint=new ArrayList<>();
		businessPartnersLint.add(businessPartners);
		
		unitOfMeasures = Struct.create(UnitOfMeasures.class);
		unitOfMeasures.setCode("ST");
		unitOfMeasures.setISOCode("PCE");
	}
	@Test
	public void validatesSupplierPersonTypeTest() {
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.listOf(BusinessPartners.class)).thenReturn(businessPartnersLint);
		Row row=Struct.create(Row.class);
		row.put("ID", "201");
		row.put("businessPartnerType", "test");
		row.put("isMarkedForDeletion", true);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first()).thenReturn(Optional.of(row));
		when(businessPartnerDao.getBusinessPartners(any(String.class))).thenReturn(result);
		when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		when(msg.target(any(String.class))).thenReturn(msg);
		validator.validateSupplierPersonType(businessPartners.getId());
	}
	@Test
	public void validateBTPUserTest() {
		BTPUsers s=Struct.create(BTPUsers.class);
		s.setPersonResponsibleId("SUBL");
		List<BTPUsers> responsiblePersonList = new ArrayList<>();
		responsiblePersonList.add(s);
		when(complaintService.getAllResponsiblePerson()).thenReturn(responsiblePersonList);
		validator.validateBTPUser("SUBL");
	}
	@Test(expected = Exception.class)
	public void validateMaterialTest() {
		validator.validateMaterial("test");
	}

	@Test(expected = Exception.class)
	public void validateMaterialNullTest() {
		validator.validateMaterial(null);
	}

	@Test(expected = Exception.class)
	public void validateCompanyCodeTest() {
		validator.validateCompanyCode("test");
	}

	@Test(expected = Exception.class)
	public void validateCompanyCodeNullTest() {
		validator.validateCompanyCode(null);
	}

	@Test(expected = Exception.class)
	public void validatePlantTest() {
		validator.validatePlant("test");
	}

	@Test(expected = Exception.class)
	public void validatePlantNullTest() {
		validator.validatePlant(null);
	}

	@Test(expected = Exception.class)
	public void validateSupplierTest() {
		validator.validateSupplier("test");
	}

	@Test(expected = Exception.class)
	public void validateSupplierNullTest() {
		validator.validateSupplier(null);
	}

	@Test(expected = Exception.class)
	public void validatePurchaseOrgTest() {
		validator.validatePurchaseOrg("test");
	}

	@Test(expected = Exception.class)
	public void validatePurchaseOrgNullTest() {
		validator.validatePurchaseOrg(null);
	}

	@Test
	public void validateQuantityTest() {
		validator.validateQuantity(new BigDecimal(1));
	}

	@Test(expected = Exception.class)
	public void validateQuantityNullTest() {
		validator.validateQuantity(null);
	}

	@Test
	public void validatePlantEmptyTest() {
		Mockito.when(configurationDao.getPlantData("23")).thenReturn(result);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validatePlant("23");
	}

	@Test
	public void validateSupplierEmptyTest() {
		Mockito.when(configurationDao.getSupplierData("23")).thenReturn(result);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validateSupplier("23");
	}

	@Test
	public void validatePurchaseOrgEmptyTest() {
		Mockito.when(configurationDao.getPurchOrgData("23")).thenReturn(result);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validatePurchaseOrg("23");
	}

	@Test
	public void validateQuantityEmptyTest() {
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validateQuantity(new BigDecimal(1));
	}

	@Test
	public void validateQuantityoneTest() {
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validateQuantity(new BigDecimal(0));
	}

	@Test
	public void validateMaterialNullResult() {
		Mockito.when(configurationDao.getMaterialData(any(String.class))).thenReturn(result);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validateMaterial("material");
	}

	@Test
	public void validateCompanyCodeTestNullResult() {
		Mockito.when(configurationDao.getCompanyCodes(any(String.class))).thenReturn(result);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validateCompanyCode("test");
	}

	@Test
	public void validatePlantTestNullResult() {
		Mockito.when(configurationDao.getPlantData(any(String.class))).thenReturn(result);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validatePlant("test");
	}

	@Test
	public void validateSupplierTestNullResult() {
		Mockito.when(configurationDao.getSupplierData(any(String.class))).thenReturn(result);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validateSupplier("test");
	}

	@Test
	public void validatePurchaseOrderTestNullResult() {
		Mockito.when(configurationDao.getPurchOrgData(any(String.class))).thenReturn(result);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validatePurchaseOrg("test");
	}

	@Test
	public void validateUnitOfmeasureTest() {
		Mockito.when(unitOfMeasureService.getUnitOfMeasureDetails(any(String.class))).thenReturn(unitOfMeasures);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validUnitOfMeasure("PC");
	}

	@Test
	public void validateUnitOfmeasureTestNull() {
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validUnitOfMeasure(null);
	}

	@Test
	public void validateSupplierPersonTypeTest() {
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.listOf(BusinessPartners.class)).thenReturn(businessPartnersLint);
		Row row=Struct.create(Row.class);
		row.put("ID", "201");
		row.put("businessPartnerType", "test");
		row.put("isMarkedForDeletion", true);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first()).thenReturn(Optional.of(row));
		when(businessPartnerDao.getBusinessPartners(any(String.class))).thenReturn(result);
		when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		when(msg.target(any(String.class))).thenReturn(msg);
		validator.validateSupplierPersonType(businessPartners.getId());
	}

	@Test
	public void validateSupplierPersonTypeNegativeTest() {
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.listOf(BusinessPartners.class)).thenReturn(businessPartnersLint);
		Row row=Struct.create(Row.class);
		row.put("ID", "201");
		row.put("businessPartnerType", "SUPCON");
		row.put("isMarkedForDeletion", false);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first()).thenReturn(Optional.of(row));
		when(businessPartnerDao.getBusinessPartners(any(String.class))).thenReturn(result);
		when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		when(msg.target(any(String.class))).thenReturn(msg);
		validator.validateSupplierPersonType(businessPartners.getId());
	}

	@Test
	public void validateSupplierPersonTypeElseTest() {
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.listOf(BusinessPartners.class)).thenReturn(businessPartnersLint);
		Row row=Struct.create(Row.class);
		row.put("ID", "201");
		row.put("businessPartnerType", "SUPCON");
		row.put("isMarkedForDeletion", false);
		Optional<Row> rowOption=Optional.empty();
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first()).thenReturn(rowOption);
		when(businessPartnerDao.getBusinessPartners(any(String.class))).thenReturn(result);
		when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		when(msg.target(any(String.class))).thenReturn(msg);
		validator.validateSupplierPersonType(businessPartners.getId());
	}

	@Test
	public void validateUnitOfMeasureElseTest() {
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.listOf(BusinessPartners.class)).thenReturn(businessPartnersLint);
		Mockito.when(unitOfMeasureService.getUnitOfMeasureDetails(any(String.class))).thenReturn(null);
		Row row=Struct.create(Row.class);
		row.put("ID", "201");
		row.put("businessPartnerType", "SUPCON");
		row.put("isMarkedForDeletion", false);
		Optional<Row> rowOption=Optional.of(row);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first()).thenReturn(rowOption);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validUnitOfMeasure("PC");
	}

	@Test
	public void validatePurchaseOrgElseTest() {
		PurchaseOrganizations purchaseOrganizations=Struct.create(PurchaseOrganizations.class);
		purchaseOrganizations.setId("20");
		List<PurchaseOrganizations> list=new ArrayList<>();
		list.add(purchaseOrganizations);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.listOf(PurchaseOrganizations.class)).thenReturn(list);
		Mockito.when(configurationDao.getPurchOrgData(any(String.class))).thenReturn(result);
		Row row=Struct.create(Row.class);
		row.put("ID", "201");
		row.put("businessPartnerType", "SUPCON");
		row.put("isMarkedForDeletion", false);
		Optional<Row> rowOption=Optional.of(row);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first()).thenReturn(rowOption);
		validator.validatePurchaseOrg("test");
	}

	@Test
	public void validateSupplierElseTest() {
		BusinessPartners purchaseOrganizations=Struct.create(BusinessPartners.class);
		purchaseOrganizations.setId("20");
		List<BusinessPartners> list=new ArrayList<>();
		list.add(purchaseOrganizations);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.listOf(BusinessPartners.class)).thenReturn(list);
		Mockito.when(configurationDao.getSupplierData(any(String.class))).thenReturn(result);
		Row row=Struct.create(Row.class);
		row.put("ID", "201");
		row.put("businessPartnerType", "SUPCON");
		row.put("isMarkedForDeletion", true);
		Optional<Row> rowOption=Optional.of(row);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first()).thenReturn(rowOption);
		validator.validateSupplier("test");
	}

	@Test
	public void validatePlantElseTest() {
		Plants purchaseOrganizations=Struct.create(Plants.class);
		purchaseOrganizations.setId("20");
		List<Plants> list=new ArrayList<>();
		list.add(purchaseOrganizations);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.listOf(Plants.class)).thenReturn(list);
		Mockito.when(configurationDao.getPlantData(any(String.class))).thenReturn(result);
		Row row=Struct.create(Row.class);
		row.put("ID", "201");
		row.put("businessPartnerType", "SUPCON");
		row.put("isMarkedForDeletion", true);
		Optional<Row> rowOption=Optional.of(row);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first()).thenReturn(rowOption);
		validator.validatePlant("test");
	}

	@Test
	public void validateCompanyCodeElseTest() {
		CompanyCodes purchaseOrganizations=Struct.create(CompanyCodes.class);
		purchaseOrganizations.setId("20");
		List<CompanyCodes> list=new ArrayList<>();
		list.add(purchaseOrganizations);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.listOf(CompanyCodes.class)).thenReturn(list);
		Mockito.when(configurationDao.getCompanyCodes(any(String.class))).thenReturn(result);
		Row row=Struct.create(Row.class);
		row.put("ID", "201");
		row.put("businessPartnerType", "SUPCON");
		row.put("isMarkedForDeletion", true);
		Optional<Row> rowOption=Optional.of(row);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first()).thenReturn(rowOption);
		validator.validateCompanyCode("test");
	}

	@Test
	public void validateMaterialElseTest() {
		MaterialMasterGeneralDatas purchaseOrganizations=Struct.create(MaterialMasterGeneralDatas.class);
		purchaseOrganizations.setId("20");
		List<MaterialMasterGeneralDatas> list=new ArrayList<>();
		list.add(purchaseOrganizations);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.listOf(MaterialMasterGeneralDatas.class)).thenReturn(list);
		Mockito.when(configurationDao.getMaterialData(any(String.class))).thenReturn(result);
		Row row=Struct.create(Row.class);
		row.put("ID", "201");
		row.put("businessPartnerType", "SUPCON");
		row.put("isMarkedForDeletion", true);
		Optional<Row> rowOption=Optional.of(row);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first()).thenReturn(rowOption);
		validator.validateMaterial("test");
	}


	@Test
	public void validateSupplierEmptyElseTest() {
		BusinessPartners purchaseOrganizations=Struct.create(BusinessPartners.class);
		purchaseOrganizations.setId("20");
		purchaseOrganizations.setIsMarkedForDeletion(true);
		List<BusinessPartners> list=new ArrayList<>();
		list.add(purchaseOrganizations);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.listOf(BusinessPartners.class)).thenReturn(list);

		Row row=Struct.create(Row.class);
		row.put("ID", "201");
		row.put("businessPartnerType", "SUPCON");
		row.put("isMarkedForDeletion", true);
		Optional<Row> rowOption=Optional.of(row);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first()).thenReturn(rowOption);
		Mockito.when(configurationDao.getSupplierData("23")).thenReturn(result);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validateSupplier("23");
	}
	
	// @Test
	// public void testValidateSalesOrganizationwithEmptyDataFromDb(){
	// 	Optional<Row> emptyOpt = Optional.empty();
	// 	when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
	// 	when(msg.target(any(String.class))).thenReturn(msg);
	// 	when(salesOrganizationRepository.getSalesOrganizationById(any(String.class))).thenReturn(result);
	// 	when(result.first()).thenReturn(emptyOpt);
	// 	validator.validateSalesOrganization("id",DestinationMappings_.class, DestinationMappings_::salesOrganization_ID);
	// }

	//@Test
	// public void testValidateSalesOrganizationwithNullValue(){
	// 	Optional<Row> rowOption=Optional.of(row);
	// 	when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
	// 	when(msg.target(any(String.class))).thenReturn(msg);
	// 	when(salesOrganizationRepository.getSalesOrganizationById(any(String.class))).thenReturn(result);
	// 	when(result.first()).thenReturn(rowOption);
	// 	validator.validateSalesOrganization(null,DestinationMappings_.class, DestinationMappings_::salesOrganization_ID);
	// }
	
	// @Test
	// public void testValidateSalesOrganizationwithData(){
	// 	Optional<Row> rowOption=Optional.of(row);
	// 	when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
	// 	when(msg.target(any(String.class))).thenReturn(msg);
	// 	when(salesOrganizationRepository.getSalesOrganizationById(any(String.class))).thenReturn(result);
	// 	when(result.first()).thenReturn(rowOption);
	// 	validator.validateSalesOrganization("id",DestinationMappings_.class, DestinationMappings_::salesOrganization_ID);
	// }


	// @Test
	// public void testValidateDistributeChannelWithData(){
	// 	Row row=Struct.create(Row.class);
	// 	row.put("salesOrganizationID_ID", "201");
	// 	Optional<Row> rowOption=Optional.of(row);
	// 	when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
	// 	when(msg.target(any(String.class))).thenReturn(msg);
	// 	when(distributionChannelRepository.getDistributionChannelById(any(String.class))).thenReturn(result);
	// 	when(result.first()).thenReturn(rowOption);
	// 	validator.validateDistributeChannel("id1","id2",DestinationMappings_.class, DestinationMappings_::distributionChannel_ID);
	// }

	// @Test
	// public void testValidateDistributeChannelWithNullValue(){
	// 	Optional<Row> emptyOpt = Optional.empty();
	// 	when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
	// 	when(msg.target(any(String.class))).thenReturn(msg);
	// 	when(distributionChannelRepository.getDistributionChannelById(any(String.class))).thenReturn(result);
	// 	when(result.first()).thenReturn(emptyOpt);
	// 	validator.validateDistributeChannel(null,null,DestinationMappings_.class, DestinationMappings_::distributionChannel_ID);
	// }
	
	// @Test
	// public void testValidateDistributeChannelWithEmptyData(){
	// 	Optional<Row> emptyOpt = Optional.empty();
	// 	when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
	// 	when(msg.target(any(String.class))).thenReturn(msg);
	// 	when(distributionChannelRepository.getDistributionChannelById(any(String.class))).thenReturn(result);
	// 	when(result.first()).thenReturn(emptyOpt);
	// 	validator.validateDistributeChannel("id","id2",DestinationMappings_.class, DestinationMappings_::distributionChannel_ID);
	// }
	
	// @Test
	// public void testValidateDivisionWithData(){
	// 	Row row=Struct.create(Row.class);
	// 	row.put("salesOrganizationID_ID", "201");
	// 	Optional<Row> rowOption=Optional.of(row);
	// 	when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
	// 	when(msg.target(any(String.class))).thenReturn(msg);
	// 	when(divisionRepository.getDivisionById(any(String.class))).thenReturn(result);
	// 	when(result.first()).thenReturn(rowOption);
	// 	validator.validateDivision("id","id2",DestinationMappings_.class, DestinationMappings_::division_ID);
	// }
	
	// @Test
	// public void testValidateDivisionWithNullValue(){
	// 	Optional<Row> emptyOpt = Optional.empty();
	// 	when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
	// 	when(msg.target(any(String.class))).thenReturn(msg);
	// 	when(divisionRepository.getDivisionById(any(String.class))).thenReturn(result);
	// 	when(result.first()).thenReturn(emptyOpt);
	// 	validator.validateDivision(null,null,DestinationMappings_.class, DestinationMappings_::division_ID);
	// }
	
	// @Test
	// public void testValidateDivisionWithEmptyData(){
	// 	Optional<Row> emptyOpt = Optional.empty();
	// 	when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
	// 	when(msg.target(any(String.class))).thenReturn(msg);
	// 	when(divisionRepository.getDivisionById(any(String.class))).thenReturn(result);
	// 	when(result.first()).thenReturn(emptyOpt);
	// 	validator.validateDivision("id","id2",DestinationMappings_.class, DestinationMappings_::division_ID);
	// }
}
