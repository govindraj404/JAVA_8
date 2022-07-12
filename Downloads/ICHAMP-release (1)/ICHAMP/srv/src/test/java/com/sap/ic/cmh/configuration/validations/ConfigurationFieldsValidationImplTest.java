package com.sap.ic.cmh.configuration.validations;

import static org.junit.Assert.assertEquals;
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
import org.mockito.MockitoAnnotations;

import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.persistency.ItemCategoriesDao;
import com.sap.ic.cmh.configuration.persistency.ReferenceTypeDao;
import com.sap.ic.cmh.configuration.service.ConfigurationService;
import com.sap.ic.cmh.masterdata.common.persistency.MasterDataDao;
import com.sap.ic.cmh.utils.SecurityValidator;

import cds.gen.com.sap.ic.cmh.targetreferencetypemapping.TargetReferenceTypeMapping_;
import cds.gen.configurationservice.BusinessObjectAttributes;
import cds.gen.configurationservice.BusinessObjectTypes;
import cds.gen.configurationservice.CompanyCodes;
import cds.gen.configurationservice.ComplaintCategories;
// import cds.gen.configurationservice.DestinationMappings_;
import cds.gen.configurationservice.Destinations;
import cds.gen.configurationservice.ItemTypes;
import cds.gen.configurationservice.SubItemTypes;
// import cds.gen.configurationservice.TargetReferenceTypeMappings_;

public class ConfigurationFieldsValidationImplTest {
	@InjectMocks
	ConfigurationFieldsValidationImpl validator;
	@Mock
	PersistenceService db;
	@Mock
	Messages messages;
	@Mock
	private Result result;
	@Mock
	MasterDataDao masterDataDao;

	@Mock
	SecurityValidator securityValidator;
	@Mock
	ConfigurationService configurationService;
	@Mock
	private Row row;
	@Mock
	Message msg;
	@Mock
	ItemCategoriesDao complaintItemCategoriesDao;
	@Mock
	ReferenceTypeDao referenceTypeDao;

	private ComplaintCategories complaintTypes;
	private BusinessObjectAttributes businessObjectAttributes;
	private BusinessObjectTypes businessObjectTypes;
	private CompanyCodes companyCodes;
	private ItemTypes itemTypes;
	private SubItemTypes subItemTypes;

	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		complaintTypes = Struct.create(ComplaintCategories.class);
		businessObjectAttributes = Struct.create(BusinessObjectAttributes.class);
		businessObjectTypes = Struct.create(BusinessObjectTypes.class);
		companyCodes = Struct.create(CompanyCodes.class);
		itemTypes = Struct.create(ItemTypes.class);
		subItemTypes = Struct.create(SubItemTypes.class);
	}

	@Test
	public void testValidateComplaintTypeTest() {
		Optional<ComplaintCategories> emptyOpt = Optional.of(complaintTypes);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(ComplaintCategories.class)).thenReturn(emptyOpt);
		validator.validateComplaintType("test");
	}

	@Test
	public void testValidateComplaintTypeNullTest() {
		Optional<ComplaintCategories> emptyOpt = Optional.empty();
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(ComplaintCategories.class)).thenReturn(emptyOpt);
		validator.validateComplaintType(null);
	}

	@Test
	public void testValidateBusinessObjectAttributeTest() {
		Optional<BusinessObjectAttributes> emptyOpt = Optional.of(businessObjectAttributes);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(BusinessObjectAttributes.class)).thenReturn(emptyOpt);
		validator.validateBusinessObjectAttribute("test");

	}

	@Test
	public void testValidateBusinessObjectAttributeNullTest() {
		Optional<BusinessObjectAttributes> emptyOpt = Optional.empty();
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(BusinessObjectAttributes.class)).thenReturn(emptyOpt);
		validator.validateBusinessObjectAttribute(null);

	}

	@Test
	public void testValidateBusinessObjectAttributeEmptyTest() {
		Optional<BusinessObjectAttributes> emptyOpt = Optional.empty();
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(BusinessObjectAttributes.class)).thenReturn(emptyOpt);
		validator.validateBusinessObjectAttribute("");

	}

	@Test
	public void testValidateDestinationTest() {
		validator.validateDestination("test");

	}

	@Test
	public void testValidateDestinationEmptyTest() {
		validator.validateDestination("");

	}

	@Test
	public void testValidateDestinationNullTest() {
		validator.validateDestination(null);
	}

	@Test
	public void testValidateBusinessObjectTypeTest() {
		Optional<BusinessObjectTypes> emptyOpt = Optional.of(businessObjectTypes);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(BusinessObjectTypes.class)).thenReturn(emptyOpt);
		validator.validateBusinessObjectType("test");
	}

	@Test
	public void testValidateBusinessObjectTypeNullTest() {
		Optional<BusinessObjectTypes> emptyOpt = Optional.empty();
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(BusinessObjectTypes.class)).thenReturn(emptyOpt);
		validator.validateBusinessObjectType(null);

	}

	@Test
	public void testValidateCompanyCodeTest() {
		Optional<CompanyCodes> emptyOpt = Optional.of(companyCodes);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(CompanyCodes.class)).thenReturn(emptyOpt);
		validator.validateCompanyCode("test");

	}

	@Test
	public void testValidateCompanyCodeNullTest() {
		Optional<CompanyCodes> emptyOpt = Optional.empty();
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(CompanyCodes.class)).thenReturn(emptyOpt);
		validator.validateCompanyCode(null);
	}

	@Test
	public void testValidateCompanyCodeEmptyTest() {
		Optional<CompanyCodes> emptyOpt = Optional.empty();
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(CompanyCodes.class)).thenReturn(emptyOpt);
		validator.validateCompanyCode("");
	}

	@Test
	public void testValidateItemTypeTest() {
		Optional<ItemTypes> emptyOpt = Optional.of(itemTypes);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(ItemTypes.class)).thenReturn(emptyOpt);
		validator.validateItemType("test");

	}

	@Test
	public void testValidateItemTypeNullTest() {
		Optional<ItemTypes> emptyOpt = Optional.empty();
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(ItemTypes.class)).thenReturn(emptyOpt);
		validator.validateItemType(null);

	}

	@Test
	public void testValidateItemTypeEmptyTest() {
		Optional<ItemTypes> emptyOpt = Optional.empty();
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(ItemTypes.class)).thenReturn(emptyOpt);
		validator.validateItemType("");

	}

	@Test
	public void testValidateSubItemTypeTest() {
		Optional<SubItemTypes> emptyOpt = Optional.of(subItemTypes);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(SubItemTypes.class)).thenReturn(emptyOpt);
		validator.validateSubItemType("test");

	}

	@Test
	public void testValidateSubItemTypeNullTest() {
		Optional<SubItemTypes> emptyOpt = Optional.empty();
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(SubItemTypes.class)).thenReturn(emptyOpt);
		validator.validateSubItemType(null);

	}

	@Test
	public void testValidateSubItemTypeEmptyTest() {
		Optional<SubItemTypes> emptyOpt = Optional.empty();
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(SubItemTypes.class)).thenReturn(emptyOpt);
		validator.validateSubItemType("");

	}

	@Test
	public void testValidateBusinessObjectValueTest() {
		validator.validateBusinessObjectValue("test");
	}

	@Test
	public void testValidateBusinessObjectValueNullTest() {
		validator.validateBusinessObjectValue(null);
	}

	@Test
	public void testValidateBusinessObjectValueEmptyTest() {
		validator.validateBusinessObjectValue("");
	}

	@Test
	public void testValidateBusinessObjectValueIfExistTest() {
		validator.validateBusinessObjectValueIfExist("test");
	}

	@Test
	public void testValidateBusinessObjectValueIfExistNullTest() {
		validator.validateBusinessObjectValueIfExist(null);
		assertEquals(false, validator.validateBusinessObjectValueIfExist(null));
	}

	@Test
	public void testValidateBusinessObjectValueIfExistEmptyTest() {
		validator.validateBusinessObjectValueIfExist("");
		assertEquals(false, validator.validateBusinessObjectValueIfExist(""));
	}

	@Test
	public void testValidateBusinessObjectValueElse1Test() {
		validator.validateBusinessObjectValueIfExist("test");
	}

	@Test
	public void testValidateBusinessObjectValueElse2Test() {
		validator.validateBusinessObjectValueIfExist(null);
	}

	@Test
	public void testValidateBusinessObjectValueElse3Test() {
		validator.validateBusinessObjectValueIfExist("");
	}

	@Test
	public void testValidateComplaintTypeEmptyTest() {
		Optional<ComplaintCategories> emptyOpt = Optional.empty();
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(ComplaintCategories.class)).thenReturn(emptyOpt);
		validator.validateComplaintType("");
	}

	@Test
	public void testValidateComplaintTypeIfExist() {
		complaintTypes.setCode("SREC");
		complaintTypes.setName("Supplier Recovery");
		Optional<ComplaintCategories> option = Optional.of(complaintTypes);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(ComplaintCategories.class)).thenReturn(option);
		validator.validateComplaintTypeIfExist("SREC");
		assertEquals(false, validator.validateComplaintTypeIfExist("SREC"));
	}

	@Test
	public void testValidateComplaintTypeIfExistNull() {
		complaintTypes.setCode("SREC");
		complaintTypes.setName("Supplier Recovery");
		Optional<ComplaintCategories> option = Optional.of(complaintTypes);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(ComplaintCategories.class)).thenReturn(option);

		assertEquals(false, validator.validateComplaintTypeIfExist(null));

	}

	@Test
	public void testValidateBusinessObjectAttributeIfExist() {

		businessObjectAttributes.setBusinessObjectAttribute("qnType");
		Optional<BusinessObjectAttributes> emptyOpt = Optional.of(businessObjectAttributes);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(BusinessObjectAttributes.class)).thenReturn(emptyOpt);
		// validator.validateBusinessObjectAttributeIfExist("qnType");
		assertEquals(false, validator.validateBusinessObjectAttributeIfExist("qnType"));
	}

	@Test
	public void testValidateBusinessObjectAttributeIfExistNull() {
		businessObjectAttributes.setBusinessObjectAttribute("qnType");
		Optional<BusinessObjectAttributes> emptyOpt = Optional.of(businessObjectAttributes);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(BusinessObjectAttributes.class)).thenReturn(emptyOpt);
		// validator.validateBusinessObjectAttributeIfExist(null);
		assertEquals(false, validator.validateBusinessObjectAttributeIfExist(null));
	}

	@Test
	public void testValidateBusinessObjectTypeIfExist() {
		businessObjectTypes.setCode("QN");
		Optional<BusinessObjectTypes> emptyOpt = Optional.of(businessObjectTypes);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(BusinessObjectTypes.class)).thenReturn(emptyOpt);
		// validator.validateBusinessObjectTypeIfExist("QN");
		assertEquals(false, validator.validateBusinessObjectTypeIfExist("QN"));
	}

	@Test
	public void testValidateBusinessObjectTypeIfExistNull() {
		businessObjectTypes.setCode("QN");
		Optional<BusinessObjectAttributes> emptyOpt = Optional.of(businessObjectAttributes);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(BusinessObjectAttributes.class)).thenReturn(emptyOpt);
		// validator.validateBusinessObjectTypeIfExist(null);
		assertEquals(false, validator.validateBusinessObjectTypeIfExist(null));
	}

	@Test
	public void testValidateBusinessObjectTypeIfExistEmpty() {
		businessObjectTypes.setCode("");
		Optional<BusinessObjectAttributes> emptyOpt = Optional.of(businessObjectAttributes);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(BusinessObjectAttributes.class)).thenReturn(emptyOpt);
		// validator.validateBusinessObjectTypeIfExist(null);
		assertEquals(false, validator.validateBusinessObjectTypeIfExist(""));
	}

	@Test
	public void testValidateCompanyCodeIfExist() {
		companyCodes.setId("1");
		Optional<CompanyCodes> emptyOpt = Optional.of(companyCodes);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(CompanyCodes.class)).thenReturn(emptyOpt);
		// validator.validateCompanyCodeIfExist("1");
		assertEquals(false, validator.validateCompanyCodeIfExist("1"));
	}

	@Test
	public void testValidateCompanyCodeIfExistNull() {
		companyCodes.setId("1");
		Optional<CompanyCodes> emptyOpt = Optional.of(companyCodes);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(CompanyCodes.class)).thenReturn(emptyOpt);
		// validator.validateCompanyCodeIfExist(null);
		assertEquals(false, validator.validateCompanyCodeIfExist(null));
	}

	@Test
	public void testValidateItemTypeIfExist() {
		itemTypes.setCode("SUBL");
		Optional<ItemTypes> emptyOpt = Optional.of(itemTypes);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(ItemTypes.class)).thenReturn(emptyOpt);
		// validator.validateItemTypeIfExist("SUBL");
		assertEquals(false, validator.validateItemTypeIfExist("SUBL"));
	}

	@Test
	public void testValidateItemTypeIfExistNull() {
		itemTypes.setCode("SUBL");
		Optional<ItemTypes> emptyOpt = Optional.of(itemTypes);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(ItemTypes.class)).thenReturn(emptyOpt);
		// validator.validateItemTypeIfExist(null);
		assertEquals(false, validator.validateItemTypeIfExist(null));
	}

	@Test
	public void testValidateSubItemTypeIfExist() {
		subItemTypes.setCode("FR");
		Optional<SubItemTypes> emptyOpt = Optional.of(subItemTypes);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(SubItemTypes.class)).thenReturn(emptyOpt);
		// validator.validateSubItemTypeIfExist("SUBL");
		assertEquals(false, validator.validateSubItemTypeIfExist("SUBL"));
	}

	@Test
	public void testValidateSubItemTypeIfExistNull() {
		subItemTypes.setCode("SUBL");
		Optional<SubItemTypes> emptyOpt = Optional.of(subItemTypes);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(SubItemTypes.class)).thenReturn(emptyOpt);
		// validator.validateSubItemTypeIfExist(null);
		assertEquals(false, validator.validateSubItemTypeIfExist(null));
	}

	@Test
	public void testValidateComplaintTypeIfExistElse() {
		complaintTypes.setCode("SREC");
		complaintTypes.setName("Supplier Recovery");
		Optional<ComplaintCategories> option = Optional.of(complaintTypes);
		when(db.run(any(CqnSelect.class))).thenReturn(null);
		when(result.first(ComplaintCategories.class)).thenReturn(option);
		validator.validateComplaintTypeIfExist("SREC");

	}

	@Test
	public void testValidateBusinessObjectAttributeIfExistNullExp() {
		businessObjectAttributes.setBusinessObjectAttribute("qnType");
		Optional<BusinessObjectAttributes> emptyOpt = Optional.of(businessObjectAttributes);
		when(db.run(any(CqnSelect.class))).thenReturn(null);
		when(result.first(BusinessObjectAttributes.class)).thenReturn(emptyOpt);
		validator.validateBusinessObjectAttributeIfExist("SUB");
	}

	@Test
	public void testValidateCompanyCodeIfExistNullExp() {
		companyCodes.setId("1");
		Optional<CompanyCodes> emptyOpt = Optional.of(companyCodes);
		when(db.run(any(CqnSelect.class))).thenReturn(null);
		when(result.first(CompanyCodes.class)).thenReturn(emptyOpt);
		validator.validateCompanyCodeIfExist("1");
	}

	@Test
	public void testValidateItemTypeIfExistNullExp() {
		itemTypes.setCode("SUBL");
		Optional<ItemTypes> emptyOpt = Optional.of(itemTypes);
		when(db.run(any(CqnSelect.class))).thenReturn(null);
		when(result.first(ItemTypes.class)).thenReturn(emptyOpt);
		validator.validateItemTypeIfExist("SUBL");
	}

	@Test
	public void testValidateSubItemTypeIfExistNullExp() {
		subItemTypes.setCode("FR");
		Optional<SubItemTypes> emptyOpt = Optional.of(subItemTypes);
		when(db.run(any(CqnSelect.class))).thenReturn(null);
		when(result.first(SubItemTypes.class)).thenReturn(emptyOpt);
		validator.validateSubItemTypeIfExist("SUBL");
	}

	@Test
	public void validateDestinationValueTest() {
		Destinations s = Struct.create(Destinations.class);
		s.setDestination("SUBL");
		List<Destinations> des = new ArrayList<>();
		des.add(s);
		when(configurationService.getallDestinationsFromBTP()).thenReturn(des);
		validator.validateDestinationValue("SUBL");
	}

	@Test
	public void isValidateNumericValueTest() {
		validator.isValidateNumericValue(BigDecimal.valueOf(22));
	}

	@Test
	public void validateCurrencyTest() {
		validator.validateCurrency(null);
	}

	@Test
	public void validateCurrencyEmptyTest() {
		validator.validateCurrency("");
	}

	@Test
	public void validateCurrencyNullTest() {
		when(masterDataDao.getCurrencies(any())).thenReturn(result);
		validator.validateCurrency("test");
	}

	@Test
	public void validateCurrencyIfExistEmptyTest() {
		validator.validateCurrencyIfExist("");
	}

	// @Test
	// public void testValidateItemCategoryWithData(){
	// Optional<Row> rowOption=Optional.of(row);
	// when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
	// when(msg.target(any(String.class))).thenReturn(msg);
	// when(complaintItemCategoriesDao.getComplaintItemCategory(any(String.class))).thenReturn(result);
	// when(result.first()).thenReturn(rowOption);
	// validator.validateItemCategory("id",DestinationMappings_.class,DestinationMappings_::itemCategoryMappings);
	// }

	// @Test
	// public void testValidateItemCategoryWithNoData(){
	// Optional<Row> emptyOpt = Optional.empty();
	// when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
	// when(msg.target(any(String.class))).thenReturn(msg);
	// when(complaintItemCategoriesDao.getComplaintItemCategory(any(String.class))).thenReturn(result);
	// when(result.first()).thenReturn(emptyOpt);
	// validator.validateItemCategory("id",DestinationMappings_.class,DestinationMappings_::itemCategoryMappings);
	// }

	// @Test
	// public void testValidateItemCategoryWithNullValue(){
	// Optional<Row> emptyOpt = Optional.empty();
	// when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
	// when(msg.target(any(String.class))).thenReturn(msg);
	// when(complaintItemCategoriesDao.getComplaintItemCategory(any(String.class))).thenReturn(result);
	// when(result.first()).thenReturn(emptyOpt);
	// validator.validateItemCategory(null,DestinationMappings_.class,DestinationMappings_::itemCategoryMappings);
	// }

	// @Test
	// public void testValidateReferenceTypeWithData() {
	// Optional<Row> rowOption=Optional.of(row);
	// when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
	// when(msg.target(any(String.class))).thenReturn(msg);
	// when(referenceTypeDao.getReferenceTypeBasedOnId(any(String.class))).thenReturn(result);
	// when(result.first()).thenReturn(rowOption);
	// validator.validateReferenceType("id",TargetReferenceTypeMappings_.class,
	// TargetReferenceTypeMappings_::referenceType_ID);
	// }

	// @Test
	// public void testValidateReferenceTypeWithNoData() {
	// Optional<Row> rowOption=Optional.empty();
	// when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
	// when(msg.target(any(String.class))).thenReturn(msg);
	// when(referenceTypeDao.getReferenceTypeBasedOnId(any(String.class))).thenReturn(result);
	// when(result.first()).thenReturn(rowOption);
	// validator.validateReferenceType("id",TargetReferenceTypeMappings_.class,
	// TargetReferenceTypeMappings_::referenceType_ID);
	// }

	// @Test
	// public void testValidateReferenceTypeWithNullValue() {
	// Optional<Row> rowOption=Optional.of(row);
	// when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
	// when(msg.target(any(String.class))).thenReturn(msg);
	// when(referenceTypeDao.getReferenceTypeBasedOnId(any(String.class))).thenReturn(result);
	// when(result.first()).thenReturn(rowOption);
	// validator.validateReferenceType(null,TargetReferenceTypeMappings_.class,
	// TargetReferenceTypeMappings_::referenceType_ID);
	// }
}
