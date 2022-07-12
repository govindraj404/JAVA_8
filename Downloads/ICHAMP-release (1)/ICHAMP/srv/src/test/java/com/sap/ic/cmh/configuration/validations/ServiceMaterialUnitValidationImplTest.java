package com.sap.ic.cmh.configuration.validations;

import cds.gen.configurationservice.ServiceMaterials;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import java.util.List;
import java.util.ArrayList;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.persistency.ServiceMaterialUnitDao;
import com.sap.ic.cmh.configuration.service.ConfigurationService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import cds.gen.configurationservice.ServiceMaterialUnits;
import java.math.BigDecimal;

import static org.mockito.ArgumentMatchers.any;

public class ServiceMaterialUnitValidationImplTest {
	@InjectMocks
	private ServiceMaterialUnitValidationImpl validator;
	@Mock
	private ServiceMaterialUnitDao serviceMaterialDao;
	@Mock
	private Messages messages;
	@Mock
	private Message msg;
	@Mock
	protected PersistenceService mockDb;
	@Mock
	private Result result;
	@Mock
	public PersistenceService db;
	@Mock
	private Row row;
	@Mock
	CdsCreateEventContext createContextMock;
	@Mock
	CqnInsert cqnInsert;

	@Mock
	ConfigurationService configurationService;
	@Mock
	ServiceMaterialUnitDao serviceMaterialUnitDao;

	private ServiceMaterialUnits serviceMaterialUnits;
    private ServiceMaterials serviceMaterials;
    List<ServiceMaterialUnits> serviceMaterialUnitList = new ArrayList<>();
	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		serviceMaterialUnits = Struct.create(ServiceMaterialUnits.class);
        serviceMaterials = Struct.create(ServiceMaterials.class);
		serviceMaterialUnits.setServiceMaterialId("www");
		serviceMaterialUnits.setUnitCode("12");
	}

	@Test
	public void testValidateServiceMaterialsTest() {
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validateServiceMaterialUnits(serviceMaterials);

	}

	/*@Test
	public void testValidateServiceMaterialsValuesTest() {
		serviceMaterialUnits.setServiceMaterialId("f55");
		serviceMaterialUnits.setUnitCode("TAG");
		serviceMaterialUnits.setNumerator(new BigDecimal(1).negate());
		serviceMaterialUnits.setDenominator(new BigDecimal(0));
        serviceMaterialUnits.setDefaultUnit(true);
		serviceMaterials.setSubItemTypeCode("FR");
        serviceMaterialUnitList.add(serviceMaterialUnits);
        serviceMaterials.setServiceMaterialUnit(serviceMaterialUnitList);
		Mockito.when(configurationService.getServiceMaterialsDetails(serviceMaterialUnits.getServiceMaterialId())).thenReturn(serviceMaterials);
		Mockito.when(serviceMaterialUnitDao.getServiceMaterialUnitBasedOnMaterialIdAndUnit(serviceMaterialUnits.getServiceMaterialId(),serviceMaterialUnits.getUnitCode())).thenReturn(result);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		validator.validateServiceMaterialUnits(serviceMaterials);

	}*/
}