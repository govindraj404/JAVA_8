package com.sap.ic.cmh.configuration.handler;

import com.sap.ic.cmh.auditlog.AuditLogHelper;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.ic.cmh.configuration.service.ReferenceTypeService;
import com.sap.ic.cmh.configuration.validations.ReferenceTypeValidation;
import static org.mockito.Mockito.doNothing;
import cds.gen.configurationservice.ReferenceTypes;
import com.sap.cds.Row;
import java.util.Optional;
import static org.mockito.Mockito.when;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import static org.mockito.ArgumentMatchers.any;


public class ReferenceTypeHandlerTest {

	@InjectMocks
	ReferenceTypeHandler handler;

	@Mock
	ReferenceTypeValidation validation;

	@Mock
	AuditLogHelper auditLogHelper;


	@Mock
	ReferenceTypeService service;

	@Mock
	Result result;

	@Mock
	Messages messages;

	@Mock
	private Message msg;

	ReferenceTypes referenceTypes;
	private Row row;
	private Optional<Row> opt;

	@Before
	public void setBefore() {
		MockitoAnnotations.openMocks(this);
		referenceTypes=Struct.create(ReferenceTypes.class);

		row = Struct.create(Row.class);
		row.put("code", "CODE100");
		row.put("identifier", "11");
		opt = Optional.of(row);
	}

	@Test
	public void testBeforeCreateReferenceType() {
		doNothing().when(validation).validateReferenceType(referenceTypes);
		when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		when(msg.target(any(String.class))).thenReturn(msg);
		handler.beforeCreateReferenceType(referenceTypes);
	}

	@Test
	public void testOnCreateReferenceType() {
		when(service.getReferenceTypes()).thenReturn(result);
		when(result.first()).thenReturn(opt);
		handler.onCreateReferenceType(referenceTypes);
	}

	@Test
	public void testOnCreateReferenceTypeWithNullData() {
		opt=Optional.empty();
		when(service.getReferenceTypes()).thenReturn(result);
		when(result.first()).thenReturn(opt);
		handler.onCreateReferenceType(referenceTypes);
	}

	@Test
	public void testBeforeUpdateReferenceType() {
		when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		when(msg.target(any(String.class))).thenReturn(msg);
		doNothing().when(validation).validateReferenceType(referenceTypes);
		handler.beforeUpdateReferenceType(referenceTypes);
	}
	@Test
	public void testOnCreateReferenceTypeWithoutIdentifier() {
		row = Struct.create(Row.class);
		row.put("code", "CODE100");
		opt = Optional.of(row);
		when(service.getReferenceTypes()).thenReturn(result);
		when(result.first()).thenReturn(opt);
		handler.onCreateReferenceType(referenceTypes);
	}

	@Test
	public void testAfterUpdateMapping() {
		referenceTypes.setId("1e93d863-5b9d-4aae-9a96-375aab27270b");
		referenceTypes.setCode("RRR");
		handler.afterUpdateReferenceType(referenceTypes);
	}
}