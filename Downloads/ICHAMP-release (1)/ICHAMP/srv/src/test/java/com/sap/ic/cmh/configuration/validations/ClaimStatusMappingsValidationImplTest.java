package com.sap.ic.cmh.configuration.validations;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.Optional;

import com.sap.ic.cmh.utils.SecurityValidator;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.configuration.persistency.ClaimStatusMappingsDao;
import com.sap.ic.cmh.returnpo.service.ReturnPurchaseOrderService;
import com.sap.ic.cmh.utils.datavalidation.DataValidatorImpl;

import cds.gen.configurationservice.ClaimStatusMappings;

public class ClaimStatusMappingsValidationImplTest {
	@InjectMocks
	ClaimStatusMappingsValidationImpl impl;

	@Mock
	private DataValidatorImpl dataValidator;
	@Mock
	private Messages messages;
	@Mock
	private Message msg;
	@Mock
	private Runnable run;
	@Mock
	ReturnPurchaseOrderService returnPurchaseOrderService;
	@Mock
	MasterDataValidation masterDataValidation;
	@Mock
	Result result;
	@Mock
	CdsCreateEventContext createContextMock;
	@Mock
	CqnInsert cqnInsert;
	@Mock
	ClaimStatusMappingsDao claimStatusMappingsDao;
	@Mock
	SecurityValidator securityValidator;

	ClaimStatusMappings claimStatusMappings;

	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		claimStatusMappings = Struct.create(ClaimStatusMappings.class);
		claimStatusMappings.setId("FPo1");
		claimStatusMappings.setStatusCode("202");
		claimStatusMappings.setCode("202");
		claimStatusMappings.setName("test");

	}

	@Test
	public void validateClaimStatusesTest() {
		ClaimStatusMappings claimStatusMappings = Struct.create(ClaimStatusMappings.class);
		Optional<ClaimStatusMappings> claimStatusMapping = Optional.of(claimStatusMappings);
		when(claimStatusMappingsDao.getClaimStatusMappings(any(String.class))).thenReturn(claimStatusMapping);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		impl.validateClaimStatuses(claimStatusMappings);
	}

	@Test
	public void validateClaimStatusesNullTest() {
		Optional<ClaimStatusMappings> claimStatusMapping = Optional.of(claimStatusMappings);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		impl.validateClaimStatuses(claimStatusMappings);
	}

	@Test
	public void validateClaimStatusesNullNameTest() {
		ClaimStatusMappings claimStatusMappings = Struct.create(ClaimStatusMappings.class);
		claimStatusMappings.setCode("202");
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		impl.validateClaimStatuses(claimStatusMappings);
	}

	@Test
	public void validateClaimStatusesNullStatudsTest() {
		ClaimStatusMappings claimStatusMappings = Struct.create(ClaimStatusMappings.class);
		claimStatusMappings.setCode("202");
		claimStatusMappings.setName("sap");
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		impl.validateClaimStatuses(claimStatusMappings);
	}

	@Test
	public void validateClaimStatusesNullStatudsCodeTest() {
		ClaimStatusMappings claimStatusMappings = Struct.create(ClaimStatusMappings.class);
		claimStatusMappings.setCode("202");
		claimStatusMappings.setName("sap");
		claimStatusMappings.setStatusCode("201");
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		impl.validateClaimStatuses(claimStatusMappings);
	}

	@Test
	public void validateClaimStatusesStatudsCodeTest() {
		ClaimStatusMappings claimStatusMappings = Struct.create(ClaimStatusMappings.class);
		claimStatusMappings.setCode("202");
		claimStatusMappings.setName("sap");
		claimStatusMappings.setStatusCode("201");
		claimStatusMappings.setId("7");
		ClaimStatusMappings claimStatusMappings1 = Struct.create(ClaimStatusMappings.class);
		claimStatusMappings.setId("9");
		Optional<ClaimStatusMappings> empty = Optional.of(claimStatusMappings1);
		when(claimStatusMappingsDao.getClaimStatusMappings(claimStatusMappings.getCode())).thenReturn(empty);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		impl.validateClaimStatuses(claimStatusMappings);
	}

	@Test
	public void validateClaimStatusesStatusPassTest() {
		ClaimStatusMappings claimStatusMappings = Struct.create(ClaimStatusMappings.class);
		claimStatusMappings.setCode("202");
		claimStatusMappings.setName("sap");
		claimStatusMappings.setStatusCode("201");
		claimStatusMappings.setId("7");
		Mockito.when(securityValidator.isValidText(claimStatusMappings.getCode())).thenReturn(true);
		Mockito.when(securityValidator.isValidText(claimStatusMappings.getName())).thenReturn(true);
		impl.validateClaimStatuses(claimStatusMappings);
	}

	@Test
	public void validateClaimStatusesStatusPassEmptyTest() {
		ClaimStatusMappings claimStatusMappings = Struct.create(ClaimStatusMappings.class);
		claimStatusMappings.setCode("");
		claimStatusMappings.setName("");
		claimStatusMappings.setStatusCode("");
		claimStatusMappings.setId("");
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		impl.validateClaimStatuses(claimStatusMappings);
	}

	@Test
	public void validateClaimStatusesCodeTest() {
		ClaimStatusMappings claimStatusMappings = Struct.create(ClaimStatusMappings.class);
		claimStatusMappings.setCode("202");
		claimStatusMappings.setName("sap");
		claimStatusMappings.setStatusCode("201");
		claimStatusMappings.setId("7");
		Optional<ClaimStatusMappings> empty = Optional.empty();
		when(claimStatusMappingsDao.getClaimStatusMappings(claimStatusMappings.getCode())).thenReturn(empty);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		impl.validateClaimStatuses(claimStatusMappings);
	}
}
