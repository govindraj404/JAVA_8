package com.sap.ic.cmh.configuration.validations;

import cds.gen.configurationservice.ServiceMaterials;
import cds.gen.configurationservice.SubItemTypes;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.persistency.ConditionTypeDao;
import com.sap.ic.cmh.configuration.persistency.ServiceMaterialDao;
import com.sap.ic.cmh.utils.SecurityValidator;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class ServiceMaterialValidationImplTest {
	@InjectMocks
	private ServiceMaterialValidationImpl validator;
	@Mock
	private ServiceMaterialDao serviceMaterialDao;
	@Mock
	private Messages messages;
	@Mock
	private Message msg;
	@Mock
	protected PersistenceService mockDb;
	@Mock
	private ConditionTypeDao dao;
	@Mock
	private Result result;
	@Mock
	public PersistenceService db;
	@Mock
	private Row row;

	@Mock
	ConfigurationFieldsValidation configurationFieldsValidation;
	@Mock
	CdsCreateEventContext createContextMock;
	@Mock
	CqnInsert cqnInsert;
	@Mock
	SecurityValidator securityValidator;
	private ServiceMaterials serviceMaterials;
	private List<SubItemTypes> subItemTypesList=new ArrayList<>();
	private SubItemTypes subItemTypes;
	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		serviceMaterials = Struct.create(ServiceMaterials.class);
		serviceMaterials.setServiceMaterial("test");
		serviceMaterials.setDestination("test");
		serviceMaterials.setDescription("test");
		serviceMaterials.setIdentifier(10);
		serviceMaterials.setItemTypeCode("test");
		serviceMaterials.setSubItemTypeCode("test");
		serviceMaterials.setId("100");
		subItemTypes = Struct.create(SubItemTypes.class);
		subItemTypes.setCode("23");
		subItemTypes.setItemTypeCode("12");
		subItemTypes.setDescription("test");
		subItemTypesList.add(subItemTypes);
	}

	@Test(expected = Exception.class)
	public void testCheckInputsSanitized_ValidationPass() {
		Mockito.when(configurationFieldsValidation.validateDestination(serviceMaterials.getDestination()))
				.thenReturn(true);
		Mockito.when(configurationFieldsValidation.validateItemType(serviceMaterials.getItemTypeCode()))
				.thenReturn(true);
		Mockito.when(configurationFieldsValidation.validateSubItemType(serviceMaterials.getSubItemTypeCode()))
				.thenReturn(true);
		Mockito.when(configurationFieldsValidation.validateSubItemType(serviceMaterials.getSubItemTypeCode()))
				.thenReturn(true);
		Mockito.when(securityValidator.isValidText(serviceMaterials.getDescription()))
				.thenReturn(true);
		Mockito.when(serviceMaterialDao.getServiceMaterialsBasedOnDestinationAndSubItemType(
				serviceMaterials.getDestination(), serviceMaterials.getItemTypeCode())).thenReturn(result);
		validator.validateServiceMaterials(serviceMaterials);
	}

	@Test
	public void testValidateServiceMaterialsTest() {
		ServiceMaterials claimStatusMappings = Struct.create(ServiceMaterials.class);
		claimStatusMappings.setItemTypeCode("202");
		claimStatusMappings.setSubItemTypeCode("202");
		Optional<SubItemTypes> emptyOpt = Optional.empty();
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(serviceMaterialDao.getServiceMaterialsBasedOnDestinationAndSubItemType(claimStatusMappings.getDestination(), claimStatusMappings.getSubItemTypeCode())).thenReturn(result);
		when(result.first(SubItemTypes.class)).thenReturn(emptyOpt);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validateServiceMaterials(claimStatusMappings);

	}

	@Test(expected = Exception.class)
	public void testCheckInputsSanitized_ValidationFailedForAlphaNumericData() {
		serviceMaterials.setDestination(null);
		Mockito.when(messages.error("any", "test")).thenReturn(msg);
		Mockito.when(msg.target("any")).thenReturn(msg);
		validator.validateServiceMaterials(serviceMaterials);
	}

	@Test
	public void testValidateServiceMaterialsDecTest() {
		ServiceMaterials claimStatusMappings = Struct.create(ServiceMaterials.class);
		claimStatusMappings.setItemTypeCode("202");
		claimStatusMappings.setSubItemTypeCode("202");
		claimStatusMappings.setDescription("test");
		Optional<ServiceMaterials> emptyOpt = Optional.of(claimStatusMappings);
		Optional<SubItemTypes> emptyOpt1 = Optional.empty();
		Row row=Struct.create(Row.class);
		row.put("ID", "202");
		row.put("itemType_code", "202");
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(serviceMaterialDao.getServiceMaterialsBasedOnDestinationAndSubItemType(claimStatusMappings.getDestination(),
				claimStatusMappings.getSubItemTypeCode())).thenReturn(result);
		when(result.first(ServiceMaterials.class)).thenReturn(emptyOpt);
		when(result.first(SubItemTypes.class)).thenReturn(emptyOpt1);
		//when(result.first()).thenReturn(Optional.of(row));
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validateServiceMaterials(claimStatusMappings);

	}

	@Test
	public void validateItemAndSubItemTest() {
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.listOf(SubItemTypes.class)).thenReturn(subItemTypesList);
		List<Row> rowValues = new ArrayList<>();
		Row row=Struct.create(Row.class);
		row.put("ID", "201");
		row.put("itemType_code", "202");
		Optional<Row> opt = Optional.of(row);
		rowValues.add(row);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.list()).thenReturn(rowValues);
		when(result.first()).thenReturn(opt);
		when(result.iterator()).thenReturn(rowValues.iterator());
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);

		ServiceMaterials claimStatusMappings = Struct.create(ServiceMaterials.class);
		claimStatusMappings.setItemTypeCode("202");
		claimStatusMappings.setSubItemTypeCode("202");
		claimStatusMappings.setDescription("test");
		claimStatusMappings.setId("202");
		Optional<ServiceMaterials> emptyOpt = Optional.of(claimStatusMappings);
		Optional<SubItemTypes> emptyOpt1 = Optional.empty();
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(serviceMaterialDao.getServiceMaterialsBasedOnDestinationAndSubItemType(claimStatusMappings.getDestination(),
				claimStatusMappings.getSubItemTypeCode())).thenReturn(result);
		when(result.first(ServiceMaterials.class)).thenReturn(emptyOpt);
		when(result.first()).thenReturn(Optional.of(row));
		when(result.first(SubItemTypes.class)).thenReturn(emptyOpt1);
		validator.validateServiceMaterials(claimStatusMappings);
	}

	@Test
	public void testValidateServiceMaterialsDecElseTest() {
		ServiceMaterials claimStatusMappings = Struct.create(ServiceMaterials.class);
		claimStatusMappings.setItemTypeCode("test");
		claimStatusMappings.setSubItemTypeCode("test");
		claimStatusMappings.setDescription("test");
		claimStatusMappings.setDestination("test");
		claimStatusMappings.setServiceMaterial("test");
		claimStatusMappings.setDescription("test");
		claimStatusMappings.setId("201");
		claimStatusMappings.setIdentifier(1);
		Optional<ServiceMaterials> emptyOpt = Optional.of(claimStatusMappings);
		List<Row> rowValues = new ArrayList<>();
		Row row=Struct.create(Row.class);
		row.put("ID", "201");
		row.put("itemType_code", "202");
		Optional<Row> opt = Optional.of(row);
		rowValues.add(row);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(serviceMaterialDao.getServiceMaterialsBasedOnDestinationAndSubItemType(claimStatusMappings.getDestination(),
				claimStatusMappings.getSubItemTypeCode())).thenReturn(result);
		when(result.first(ServiceMaterials.class)).thenReturn(emptyOpt);
		when(result.first()).thenReturn(Optional.of(row));
		when(result.iterator()).thenReturn(rowValues.iterator());
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validateServiceMaterials(claimStatusMappings);

	}
}
