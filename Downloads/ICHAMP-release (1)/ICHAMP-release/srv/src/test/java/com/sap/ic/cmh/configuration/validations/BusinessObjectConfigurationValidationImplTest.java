package com.sap.ic.cmh.configuration.validations;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

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
import com.sap.cds.ql.Select;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.persistency.BusinessObjectConfigurationDao;
import com.sap.ic.cmh.utils.SecurityValidator;

import cds.gen.configurationservice.BusinessObjectAttributes;
import cds.gen.configurationservice.BusinessObjectConfigurations;
import cds.gen.configurationservice.ComplaintCategories;

public class BusinessObjectConfigurationValidationImplTest {
	@InjectMocks
	BusinessObjectConfigurationValidationImpl validator;
	@Mock
	BusinessObjectConfigurationDao businessObjectConfigurationDao;
	@Mock
	Messages messages;
	@Mock
	Message message;
	@Mock
	SecurityValidator securityValidator;
	@Mock
	ConfigurationFieldsValidation configurationFieldsValidation;
	@Mock
	CdsCreateEventContext createContextMock;
	@Mock
	CqnInsert cqnInsert;
	@Mock
	protected PersistenceService mockDb;
	@Mock
	Result result;
	private Row row;
	private Optional<Row> opt;

	private BusinessObjectConfigurations businessObjectConfigurations;
	private ComplaintCategories addressItem;
	@Mock
	public PersistenceService db;
	@Mock
	ConfigurationFieldsValidation configurationFieldsValidation1;

	@Mock
	private Message msg;

	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		businessObjectConfigurations = Struct.create(BusinessObjectConfigurations.class);
		addressItem = Struct.create(ComplaintCategories.class);
		addressItem.setCode("12");
		businessObjectConfigurations.setId("100");
		businessObjectConfigurations.setDestination("GNT");
		businessObjectConfigurations.setIdentifier(10);
		businessObjectConfigurations.setComplaintTypeCode("test");
		businessObjectConfigurations.setBusinessObjectValue("test");
		businessObjectConfigurations.setBusinessObjectTypeCode("1010");

		businessObjectConfigurations.setBusinessObjectAttributeCode("test");

	}

	@Test(expected = Exception.class)
	public void testValidateBusinessObjectConfigurationTest() {
		businessObjectConfigurations.setDestination(null);
		Optional<ComplaintCategories> opt = Optional.of(addressItem);
		when(configurationFieldsValidation.validateDestination(businessObjectConfigurations.getDestination()))
				.thenReturn(true);
		Mockito.when(messages.error("any", "test")).thenReturn(message);
		Mockito.when(message.target("any")).thenReturn(message);
		when(configurationFieldsValidation.validateComplaintType("test")).thenReturn(true);
		Mockito.when(messages.error("any", "test")).thenReturn(message);
		Mockito.when(message.target("any")).thenReturn(message);
		when(configurationFieldsValidation.validateBusinessObjectType("test")).thenReturn(true);
		when(configurationFieldsValidation.validateBusinessObjectAttribute("test")).thenReturn(true);
		when(configurationFieldsValidation.validateBusinessObjectValue("test")).thenReturn(false);
		when(configurationFieldsValidation.validateBusinessObjectValueIfExist("test")).thenReturn(true);
		when(configurationFieldsValidation.validateComplaintType("test")).thenReturn(true);
		validator.validateBusinessObjectConfiguration(businessObjectConfigurations);
	}

	 @Test
	 public void testValidateBusinessObjectConfigurationValidateTest() {
	 	Optional<ComplaintCategories> opt = Optional.of(addressItem);
	 	Mockito.when(db.run(any(Select.class))).thenReturn(result);
	 	Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
	 	Mockito.when(msg.target(any(String.class))).thenReturn(msg);
	 	when(configurationFieldsValidation.validateComplaintType("test")).thenReturn(false);
	 	when(configurationFieldsValidation.validateBusinessObjectType("test")).thenReturn(false);
	 	when(configurationFieldsValidation.validateBusinessObjectAttribute("test")).thenReturn(false);
	 	when(configurationFieldsValidation.validateBusinessObjectValue("test")).thenReturn(true);
	 	when(configurationFieldsValidation.validateBusinessObjectValueIfExist("test")).thenReturn(false);
	 	when(configurationFieldsValidation.validateComplaintType("test")).thenReturn(true);
	 	when(configurationFieldsValidation.validateDestination(businessObjectConfigurations.getDestination()))
	 			.thenReturn(true);
		 when(businessObjectConfigurationDao.getBusinessObjectConfigBasedOnDestinationAndBO(
				 businessObjectConfigurations.getComplaintTypeCode(), businessObjectConfigurations.getDestination(), businessObjectConfigurations.getBusinessObjectTypeCode(),
				 businessObjectConfigurations.getBusinessObjectAttributeCode()))
				 .thenReturn(result);
	 	validator.validateBusinessObjectConfiguration(businessObjectConfigurations);
	 }

	@Test(expected = Exception.class)
	public void testCheckInputsSanitized_ValidationFailedForAlphaNumericData() {
		businessObjectConfigurations.setDestination("<html><head></head><body></body></html>");
		when(configurationFieldsValidation1.validateDestination(businessObjectConfigurations.getDestination()))
				.thenReturn(true);

		validator.validateBusinessObjectConfiguration(businessObjectConfigurations);
	}

	 @Test
	 public void testValidateBusinessObjectConfigurationDestinationTest() {
	 	BusinessObjectConfigurations claimStatusMappings = Struct.create(BusinessObjectConfigurations.class);
	 	claimStatusMappings.setBusinessObjectAttributeCode("202");
	 	claimStatusMappings.setBusinessObjectTypeCode("202");
		 claimStatusMappings.setDestination("test");
		 claimStatusMappings.setComplaintTypeCode("23");
		 claimStatusMappings.setComplaintTypeCode("23");
	 	Optional<BusinessObjectAttributes> emptyOpt = Optional.empty();
	 	when(db.run(any(CqnSelect.class))).thenReturn(result);
	 	when(result.first(BusinessObjectAttributes.class)).thenReturn(emptyOpt);
		 when(businessObjectConfigurationDao.getBusinessObjectConfigBasedOnDestinationAndBO(
				 claimStatusMappings.getComplaintTypeCode(),
				 claimStatusMappings.getDestination(),
				 claimStatusMappings.getBusinessObjectTypeCode(),
				 claimStatusMappings.getBusinessObjectAttributeCode()))
				 .thenReturn(result);
	 	Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
	 	Mockito.when(msg.target(any(String.class))).thenReturn(msg);

	 	validator.validateBusinessObjectConfiguration(claimStatusMappings);

	 }

	 @Test
	 public void testValidateBusinessObjectConfigurationvalTest() {
	 	BusinessObjectConfigurations claimStatusMappings = Struct.create(BusinessObjectConfigurations.class);
		 claimStatusMappings.setBusinessObjectAttributeCode("202");
		 claimStatusMappings.setBusinessObjectTypeCode("202");
		 claimStatusMappings.setDestination("test");
		 claimStatusMappings.setComplaintTypeCode("23");
		 claimStatusMappings.setComplaintTypeCode("23");
	 	Optional<BusinessObjectConfigurations> emptyOpt = Optional.of(claimStatusMappings);
	 	when(db.run(any(CqnSelect.class))).thenReturn(result);
	 	when(result.first(BusinessObjectConfigurations.class)).thenReturn(emptyOpt);
		 when(businessObjectConfigurationDao.getBusinessObjectConfigBasedOnDestinationAndBO(
				 claimStatusMappings.getComplaintTypeCode(),
				 claimStatusMappings.getDestination(),
				 claimStatusMappings.getBusinessObjectTypeCode(),
				 claimStatusMappings.getBusinessObjectAttributeCode()))
				 .thenReturn(result);
	 	Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
	 	Mockito.when(msg.target(any(String.class))).thenReturn(msg);
	 	validator.validateBusinessObjectConfiguration(claimStatusMappings);

	 }

	@Test
	public void validateBusinessObjectTypeTest() {
		BusinessObjectConfigurations claimStatusMappings = Struct.create(BusinessObjectConfigurations.class);
		claimStatusMappings.setBusinessObjectAttributeCode("202");
		claimStatusMappings.setBusinessObjectTypeCode("202");
		claimStatusMappings.setDestination("test");
		claimStatusMappings.setComplaintTypeCode("23");
		claimStatusMappings.setId("202");
		Optional<BusinessObjectConfigurations> emptyOpt = Optional.of(claimStatusMappings);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(BusinessObjectConfigurations.class)).thenReturn(emptyOpt);
		when(businessObjectConfigurationDao.getBusinessObjectConfigBasedOnDestinationAndBO(
				claimStatusMappings.getComplaintTypeCode(),
				claimStatusMappings.getDestination(),
				claimStatusMappings.getBusinessObjectTypeCode(),
				claimStatusMappings.getBusinessObjectAttributeCode()))
				.thenReturn(result);
		List<Row> rowValues = new ArrayList<>();
		Row row=Struct.create(Row.class);
		row.put("ID", "201");
		row.put("businessObjectType", "202");
		Optional<Row> opt = Optional.of(row);
		rowValues.add(row);

		when(result.first()).thenReturn(Optional.of(row));
		when(result.iterator()).thenReturn(rowValues.iterator());
		when(result.first()).thenReturn(Optional.of(row));
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validateBusinessObjectConfiguration(claimStatusMappings);

	}

	@Test
	public void validateBusinessObjectTypeElseTest() {
		BusinessObjectConfigurations claimStatusMappings = Struct.create(BusinessObjectConfigurations.class);
		claimStatusMappings.setBusinessObjectAttributeCode("/(23");
		claimStatusMappings.setBusinessObjectTypeCode("/(23");
		claimStatusMappings.setBusinessObjectValue("test");
		claimStatusMappings.setDestination("test");
		claimStatusMappings.setComplaintTypeCode("23");
		claimStatusMappings.setId("201");
		Optional<BusinessObjectConfigurations> emptyOpt = Optional.of(claimStatusMappings);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(BusinessObjectConfigurations.class)).thenReturn(emptyOpt);
		when(businessObjectConfigurationDao.getBusinessObjectConfigBasedOnDestinationAndBO(
				claimStatusMappings.getComplaintTypeCode(),
				claimStatusMappings.getDestination(),
				claimStatusMappings.getBusinessObjectTypeCode(),
				claimStatusMappings.getBusinessObjectAttributeCode()))
				.thenReturn(result);
		List<Row> rowValues = new ArrayList<>();
		Row row=Struct.create(Row.class);
		row.put("ID", "201");
		row.put("businessObjectType", "202");
		Optional<Row> opt = Optional.of(row);
		rowValues.add(row);
		when(result.first()).thenReturn(Optional.of(row));
		when(result.iterator()).thenReturn(rowValues.iterator());
		when(result.first()).thenReturn(Optional.of(row));
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validateBusinessObjectConfiguration(claimStatusMappings);

	}
	@Test
	public void validateBusinessObjectTypeElse1Test() {
		BusinessObjectConfigurations claimStatusMappings = Struct.create(BusinessObjectConfigurations.class);
		claimStatusMappings.setBusinessObjectAttributeCode("");
		claimStatusMappings.setBusinessObjectTypeCode("");
		claimStatusMappings.setBusinessObjectValue("re");
		claimStatusMappings.setDestination("7t");
		claimStatusMappings.setComplaintTypeCode("23");
		claimStatusMappings.setId("201");
		Optional<BusinessObjectConfigurations> emptyOpt = Optional.of(claimStatusMappings);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(BusinessObjectConfigurations.class)).thenReturn(emptyOpt);
		when(businessObjectConfigurationDao.getBusinessObjectConfigBasedOnDestinationAndBO(
				claimStatusMappings.getComplaintTypeCode(),
				claimStatusMappings.getDestination(),
				claimStatusMappings.getBusinessObjectTypeCode(),
				claimStatusMappings.getBusinessObjectAttributeCode()))
				.thenReturn(result);
		List<Row> rowValues = new ArrayList<>();
		Row row=Struct.create(Row.class);
		row.put("ID", "201");
		row.put("businessObjectType", "1");
		Optional<Row> opt = Optional.of(row);
		rowValues.add(row);
		when(result.first()).thenReturn(Optional.of(row));
		when(result.iterator()).thenReturn(rowValues.iterator());
		when(result.first()).thenReturn(Optional.of(row));
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validateBusinessObjectType("test","202");

	}

	@Test
	public void validateBusinessObjectTypeElse2Test() {
		BusinessObjectConfigurations claimStatusMappings = Struct.create(BusinessObjectConfigurations.class);
		claimStatusMappings.setId("201");
		Optional<BusinessObjectConfigurations> emptyOpt = Optional.of(claimStatusMappings);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(BusinessObjectConfigurations.class)).thenReturn(emptyOpt);
		List<Row> rowValues = new ArrayList<>();
		Row row=Struct.create(Row.class);
		row.put("ID", "201");
		row.put("businessObjectType", "1");
		Optional<Row> opt = Optional.of(row);
		rowValues.add(row);
		when(result.iterator()).thenReturn(rowValues.iterator());
		when(result.first()).thenReturn(Optional.empty());
		validator.validateBusinessObjectType("test","56");

	}



}
