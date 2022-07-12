package com.sap.ic.cmh.configuration.validations;

import cds.gen.configurationservice.ConditionTypes;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.persistency.ConditionTypeDao;
import com.sap.ic.cmh.utils.SecurityValidator;
import com.sap.ic.cmh.utils.datavalidation.DataValidatorImpl;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class ConditionTypeValidationImplTest {
	@InjectMocks
	private ConditionTypeValidationImpl validator;
	@Mock
	private DataValidatorImpl dataValidator;
	@Mock
	private Messages messages;
	@Mock
	private Message msg;
	@Mock
	private ConditionTypeDao dao;
	@Mock
	public PersistenceService db;
	@Mock
	private Result result;
	@Mock
	@Autowired
	ConditionTypeDao conditionTypeDao;

	@Mock
	ConfigurationFieldsValidation configurationFieldsValidation;

	@Mock
	SecurityValidator securityValidator;
	private ConditionTypes conditionTypes;

	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		conditionTypes = Struct.create(ConditionTypes.class);

		conditionTypes.setConditionType("test");
		conditionTypes.setDestination("test");
		conditionTypes.setDescription("test");
		conditionTypes.setIdentifier(10);
		conditionTypes.setItemTypeCode("test");
		conditionTypes.setId("100");

	}


	@Test
	public void testCheckInputsSanitized_ValidationFail() {
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		Mockito.when(configurationFieldsValidation.validateDestination(conditionTypes.getDestination()))
				.thenReturn(false);
		Mockito.when(configurationFieldsValidation.validateItemType(conditionTypes.getItemTypeCode()))
				.thenReturn(false);
		Mockito.when(securityValidator.isValidText(conditionTypes.getDescription()))
				.thenReturn(false);
		Mockito.when(dao.getConditionTypesBasedOnDestinationAndItemType(conditionTypes.getDestination(),
				conditionTypes.getItemTypeCode())).thenReturn(result);
		Optional<ConditionTypes> emptyOpt = Optional.of(conditionTypes);
		when(conditionTypeDao.getConditionTypesBasedOnDestinationAndItemType(conditionTypes.getDestination(),
				conditionTypes.getItemTypeCode())).thenReturn(result);
		when(result.first(ConditionTypes.class)).thenReturn(emptyOpt);
		validator.validateConditionTypes(conditionTypes);
	}

	@Test(expected = Exception.class)
	public void testCheckInputsSanitized_ValidationFailedForAlphaNumericData() {
		conditionTypes.setDestination(null);
		Mockito.when(messages.error("any", "test")).thenReturn(msg);
		Mockito.when(msg.target("any")).thenReturn(msg);
		validator.validateConditionTypes(conditionTypes);
	}

	@Test
	public void testValidateConditionTypesTest() {
		conditionTypes.setDestination(null);
		Optional<ConditionTypes> emptyOpt = Optional.of(conditionTypes);
		when(conditionTypeDao.getConditionTypesBasedOnDestinationAndItemType(conditionTypes.getDestination(),
				conditionTypes.getItemTypeCode())).thenReturn(result);
		when(result.first(ConditionTypes.class)).thenReturn(emptyOpt);
		Mockito.when(configurationFieldsValidation.validateDestination(any(String.class))).thenReturn(true);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validateConditionTypes(conditionTypes);
	}

	@Test
	public void testValidateConditionTyGestinationTest() {
		conditionTypes.setConditionType(null);
		Optional<ConditionTypes> emptyOpt = Optional.of(conditionTypes);
		when(conditionTypeDao.getConditionTypesBasedOnDestinationAndItemType(conditionTypes.getDestination(),
				conditionTypes.getItemTypeCode())).thenReturn(result);
		when(result.first(ConditionTypes.class)).thenReturn(emptyOpt);
		Mockito.when(configurationFieldsValidation.validateDestination(any(String.class))).thenReturn(true);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validateConditionTypes(conditionTypes);
	}

	@Test
	public void testValidateConditionTypesCodeTest() {
		ConditionTypes claimStatusMappings = Struct.create(ConditionTypes.class);
		claimStatusMappings.setDestination("202");
		claimStatusMappings.setDescription("sap");
		claimStatusMappings.setConditionType("201");
		claimStatusMappings.setId("7");
		claimStatusMappings.setItemTypeCode("4");
		claimStatusMappings.setItemTypeCode("3");
		ConditionTypes claimStatusMappings1 = Struct.create(ConditionTypes.class);
		claimStatusMappings.setId("9");

		when(conditionTypeDao.getConditionTypesBasedOnDestinationAndItemType(claimStatusMappings.getDestination(),
				claimStatusMappings.getItemTypeCode())).thenReturn(result);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validateConditionTypes(claimStatusMappings);
	}

	@Test
	public void testValidateClaimStatusesStatudsPassTest() {
		ConditionTypes claimStatusMappings = Struct.create(ConditionTypes.class);
		claimStatusMappings.setDestination("202");
		claimStatusMappings.setDescription("sap");
		claimStatusMappings.setConditionType("201");
		claimStatusMappings.setId("7");
		claimStatusMappings.setItemTypeCode("4");
		claimStatusMappings.setItemTypeCode("3");
		Optional<ConditionTypes> emptyOpt = Optional.of(conditionTypes);
		when(conditionTypeDao.getConditionTypesBasedOnDestinationAndItemType(claimStatusMappings.getDestination(),
				claimStatusMappings.getItemTypeCode())).thenReturn(result);
		when(result.first(ConditionTypes.class)).thenReturn(emptyOpt);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validateConditionTypes(claimStatusMappings);
	}

	@Test
	public void testValidateClaimStatusesStatudsPassEmptyTest() {
		ConditionTypes claimStatusMappings = Struct.create(ConditionTypes.class);
		claimStatusMappings.setDestination("");
		claimStatusMappings.setDescription("");
		claimStatusMappings.setConditionType("");
		claimStatusMappings.setId("");
		claimStatusMappings.setItemTypeCode("");
		claimStatusMappings.setItemTypeCode("");
		Optional<ConditionTypes> emptyOpt = Optional.of(conditionTypes);
		when(conditionTypeDao.getConditionTypesBasedOnDestinationAndItemType(claimStatusMappings.getDestination(),
				claimStatusMappings.getItemTypeCode())).thenReturn(result);
		when(result.first(ConditionTypes.class)).thenReturn(emptyOpt);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validateConditionTypes(claimStatusMappings);
	}


	@Test
	public void testValidateClaimStatusesTest() {
		ConditionTypes claimStatusMappings = Struct.create(ConditionTypes.class);
		claimStatusMappings.setDestination("202");
		claimStatusMappings.setDescription("sap");
		claimStatusMappings.setConditionType("201");
		claimStatusMappings.setId("7");
		claimStatusMappings.setItemTypeCode("4");
		claimStatusMappings.setItemTypeCode("3");
		Row row=Struct.create(Row.class);
		row.put("ID", "201");
		row.put("itemType_code", "202");
		Optional<ConditionTypes> emptyOpt = Optional.of(conditionTypes);
		when(conditionTypeDao.getConditionTypesBasedOnDestinationAndItemType
				(claimStatusMappings.getDestination(),
				claimStatusMappings.getItemTypeCode())).thenReturn(result);
		when(result.first(ConditionTypes.class)).thenReturn(emptyOpt);
		when(result.first()).thenReturn(Optional.of(row));
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validateConditionTypes(claimStatusMappings);
	}

	@Test
	public void conditionTypesDesNullTest() {
		ConditionTypes claimStatusMappings = Struct.create(ConditionTypes.class);
		claimStatusMappings.setDestination("202");
		claimStatusMappings.setDescription("sap");
		claimStatusMappings.setConditionType(null);
		claimStatusMappings.setId("7");
		claimStatusMappings.setItemTypeCode("4");
		claimStatusMappings.setItemTypeCode("3");
		Row row=Struct.create(Row.class);
		row.put("ID", "201");
		row.put("itemType_code", "202");
		Optional<ConditionTypes> emptyOpt = Optional.of(conditionTypes);
		when(conditionTypeDao.getConditionTypesBasedOnDestinationAndItemType
				(claimStatusMappings.getDestination(),
						claimStatusMappings.getItemTypeCode())).thenReturn(result);
		when(result.first(ConditionTypes.class)).thenReturn(emptyOpt);
		when(result.first()).thenReturn(Optional.of(row));
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validateConditionTypes(claimStatusMappings);
	}

	@Test
	public void conditionTypesEmptyTest() {
		ConditionTypes claimStatusMappings = Struct.create(ConditionTypes.class);
		claimStatusMappings.setDestination("202");
		claimStatusMappings.setDescription("sap");
		claimStatusMappings.setConditionType("");
		claimStatusMappings.setId("7");
		claimStatusMappings.setItemTypeCode("4");
		claimStatusMappings.setItemTypeCode("3");
		Row row=Struct.create(Row.class);
		row.put("ID", "201");
		row.put("itemType_code", "202");
		Optional<ConditionTypes> emptyOpt = Optional.of(conditionTypes);
		when(conditionTypeDao.getConditionTypesBasedOnDestinationAndItemType
				(claimStatusMappings.getDestination(),
						claimStatusMappings.getItemTypeCode())).thenReturn(result);
		when(result.first(ConditionTypes.class)).thenReturn(emptyOpt);
		when(result.first()).thenReturn(Optional.of(row));
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validateConditionTypes(claimStatusMappings);
	}
	@Test
	public void setDestinationEmptyTest() {
		ConditionTypes claimStatusMappings = Struct.create(ConditionTypes.class);
		claimStatusMappings.setDestination("null");
		claimStatusMappings.setDescription(null);
		claimStatusMappings.setConditionType("202");
		claimStatusMappings.setId("7");
		claimStatusMappings.setItemTypeCode("4");
		claimStatusMappings.setItemTypeCode("3");
		Row row=Struct.create(Row.class);
		row.put("ID", "201");
		row.put("itemType_code", "202");
		Optional<ConditionTypes> emptyOpt = Optional.of(conditionTypes);
		when(conditionTypeDao.getConditionTypesBasedOnDestinationAndItemType
				(claimStatusMappings.getDestination(),
						claimStatusMappings.getItemTypeCode())).thenReturn(result);
		when(result.first(ConditionTypes.class)).thenReturn(emptyOpt);
		when(result.first()).thenReturn(Optional.of(row));
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validateConditionTypes(claimStatusMappings);
	}

	@Test
	public void setConditionTypesIDNullTest() {
		ConditionTypes claimStatusMappings = Struct.create(ConditionTypes.class);
		claimStatusMappings.setDestination("test");
		claimStatusMappings.setDescription("null");
		claimStatusMappings.setConditionType("202");
		claimStatusMappings.setId("2");
		claimStatusMappings.setItemTypeCode("4");
		claimStatusMappings.setItemTypeCode("3");
		Row row=Struct.create(Row.class);
		row.put("ID", "201");
		row.put("itemType_code", "202");
		Optional<ConditionTypes> emptyOpt = Optional.of(conditionTypes);
		when(conditionTypeDao.getConditionTypesBasedOnDestinationAndItemType
				(claimStatusMappings.getDestination(),
						claimStatusMappings.getItemTypeCode())).thenReturn(result);
		when(result.first(ConditionTypes.class)).thenReturn(emptyOpt);
		//when(result.first()).thenReturn(Optional.of(row));
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validateConditionTypes(claimStatusMappings);
	}


	@Test
	public void setConditionTypesIDElseTest() {
		ConditionTypes claimStatusMappings = Struct.create(ConditionTypes.class);
		claimStatusMappings.setDestination(null);
		claimStatusMappings.setConditionType("202");
		claimStatusMappings.setId("2");
		claimStatusMappings.setItemTypeCode("89");

		Row row=Struct.create(Row.class);
		row.put("ID", "201");
		row.put("itemType_code", null);
		Optional<ConditionTypes> emptyOpt = Optional.empty();
		when(conditionTypeDao.getConditionTypesBasedOnDestinationAndItemType
				(claimStatusMappings.getDestination(),
						claimStatusMappings.getItemTypeCode())).thenReturn(result);
		when(result.first(ConditionTypes.class)).thenReturn(emptyOpt);
		//when(result.first()).thenReturn(Optional.of(row));
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validateConditionTypes(claimStatusMappings);
	}


}
