package com.sap.ic.cmh.configuration.validations;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.Optional;

import com.sap.cds.Row;
import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.persistency.DestinationConfigurationDao;
import com.sap.ic.cmh.utils.SecurityValidator;

import cds.gen.configurationservice.DestinationConfigurations;

public class DestinationConfigurationValidationImplTest {
	@InjectMocks
	private DestinationConfigurationValidationImpl validator;
	@Mock
	DestinationConfigurationDao destinationConfigurationDao;
	@Mock
	ConfigurationFieldsValidation configurationFieldsValidation;
	@Mock
	SecurityValidator securityValidator;
	@Mock
	private Messages messages;
	@Mock
	private Message msg;
	@Mock
	private Result result;
	@Mock
	public PersistenceService db;
	private DestinationConfigurations destinationConfigurations;

	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		destinationConfigurations = Struct.create(DestinationConfigurations.class);
		destinationConfigurations.setCompanyCodeId("test");
		destinationConfigurations.setNavigationDestination("test");
		destinationConfigurations.setDestination("test");
		destinationConfigurations.setDescription("test");
		destinationConfigurations.setIdentifier(10);
		destinationConfigurations.setId("100");
	}

	// @Test
	// public void checkInputsSanitized_ValidationPass() {
	// 	Mockito.when(configurationFieldsValidation.validateDestination(destinationConfigurations.getDestination()))
	// 			.thenReturn(true);
	// 	Mockito.when(configurationFieldsValidation.validateCompanyCode(destinationConfigurations.getDestination()))
	// 			.thenReturn(true);
	// 	Mockito.when(configurationFieldsValidation
	// 			.validateBusinessObjectType(destinationConfigurations.getBusinessObjectTypeCode())).thenReturn(true);
	// 	Mockito.when(securityValidator.isValidText(destinationConfigurations.getDescription()))
	// 			.thenReturn(true);
	// 	Mockito.when(destinationConfigurationDao.getDestinationConfigBasedOnCompanyAndDestination(
	// 			destinationConfigurations.getCompanyCodeId(),
	// 			destinationConfigurations.getBusinessObjectTypeCode())).thenReturn(result);

	// 	Assertions.assertDoesNotThrow(() -> validator.validateDestinationConfiguration(destinationConfigurations));
	// }

	@Test
	public void checkInputsSanitized_ValidationError() {
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		Mockito.when(configurationFieldsValidation.validateDestination(destinationConfigurations.getDestination()))
				.thenReturn(false);
		Mockito.when(configurationFieldsValidation.validateCompanyCode(destinationConfigurations.getDestination()))
				.thenReturn(false);
		Mockito.when(configurationFieldsValidation
				.validateBusinessObjectType(destinationConfigurations.getBusinessObjectTypeCode())).thenReturn(false);
		Mockito.when(securityValidator.isValidText(destinationConfigurations.getDescription()))
				.thenReturn(false);
		Mockito.when(destinationConfigurationDao.getDestinationConfigBasedOnCompanyAndDestination(
				destinationConfigurations.getCompanyCodeId(),
				destinationConfigurations.getBusinessObjectTypeCode())).thenReturn(result);

		Assertions.assertDoesNotThrow(() -> validator.validateDestinationConfiguration(destinationConfigurations));
	}

	 @Test
	 public void validateDestinationConfigurationTest() {
		 destinationConfigurations.setDestination("test");
		 destinationConfigurations.setCompanyCodeId("1");
		 destinationConfigurations.setBusinessObjectTypeCode("23");
		 destinationConfigurations.setDescription("test");
	 	Mockito.when(configurationFieldsValidation.validateDestination(any(String.class))).thenReturn(true);
	 	Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
	 	Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		 when(destinationConfigurationDao.getDestinationConfigBasedOnCompanyAndDestination(destinationConfigurations.getCompanyCodeId(),
				destinationConfigurations.getBusinessObjectTypeCode())).thenReturn(result);
	 	validator.validateDestinationConfiguration(destinationConfigurations);
	 }

	 @Test
	 public void validateDestinationConfigurationOneTest() {
	 	destinationConfigurations.setDestination("test");
		 destinationConfigurations.setCompanyCodeId("1");
		 destinationConfigurations.setBusinessObjectTypeCode("23");
		 destinationConfigurations.setDescription("test");
	 	Mockito.when(configurationFieldsValidation.validateDestination(any(String.class))).thenReturn(true);
	 	Optional<DestinationConfigurations> emptyOpt = Optional.of(destinationConfigurations);
	 	when(db.run(any(CqnSelect.class))).thenReturn(result);
		 when(destinationConfigurationDao.getDestinationConfigBasedOnCompanyAndDestination(destinationConfigurations.getCompanyCodeId(),
				destinationConfigurations.getBusinessObjectTypeCode())).thenReturn(result);
	 	when(result.first(DestinationConfigurations.class)).thenReturn(emptyOpt);
	 	Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
	 	Mockito.when(msg.target(any(String.class))).thenReturn(msg);
	 	validator.validateDestinationConfiguration(destinationConfigurations);
	 }
	@Test
	public void validateDestinationConfigurationErrorTest() {
		destinationConfigurations.setDestination("test");
		destinationConfigurations.setCompanyCodeId("1");
		destinationConfigurations.setBusinessObjectTypeCode("23");
		destinationConfigurations.setDescription("test");
		destinationConfigurations.setId("23");
		Row row=Struct.create(Row.class);
		row.put("ID", "201");
		row.put("itemType_code", "202");
		//
		Mockito.when(configurationFieldsValidation.validateDestination(any(String.class))).thenReturn(true);
		Optional<DestinationConfigurations> emptyOpt = Optional.of(destinationConfigurations);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(destinationConfigurationDao.getDestinationConfigBasedOnCompanyAndDestination(destinationConfigurations.getCompanyCodeId(),
			destinationConfigurations.getBusinessObjectTypeCode())).thenReturn(result);
				when(result.first()).thenReturn(Optional.of(row));
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validateDestinationConfiguration(destinationConfigurations);
	}

	@Test
	public void validateDestinationConfigurationElseTest() {
		destinationConfigurations.setDestination(null);
		destinationConfigurations.setCompanyCodeId("1");
		destinationConfigurations.setBusinessObjectTypeCode("23");
		destinationConfigurations.setId("201");
		Row row=Struct.create(Row.class);
		row.put("ID", "201");
		row.put("itemType_code", "202");
		Mockito.when(configurationFieldsValidation.validateDestination(any(String.class))).thenReturn(true);
		Optional<DestinationConfigurations> emptyOpt = Optional.of(destinationConfigurations);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(destinationConfigurationDao.getDestinationConfigBasedOnCompanyAndDestination(destinationConfigurations.getCompanyCodeId(),
				destinationConfigurations.getBusinessObjectTypeCode())).thenReturn(result);
		when(result.first()).thenReturn(Optional.of(row));
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validateDestinationConfiguration(destinationConfigurations);
	}
}
