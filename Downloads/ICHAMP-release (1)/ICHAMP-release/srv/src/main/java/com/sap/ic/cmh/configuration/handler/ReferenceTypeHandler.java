package com.sap.ic.cmh.configuration.handler;

import java.util.Optional;
import java.util.stream.Stream;

import com.sap.cds.services.auditlog.Action;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.cds.services.cds.CdsReadEventContext;
import com.sap.cds.services.handler.annotations.After;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.sap.cds.Row;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.Before;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.configuration.service.ReferenceTypeService;
import com.sap.ic.cmh.configuration.validations.ReferenceTypeValidation;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.configurationservice.ReferenceTypes;
import cds.gen.configurationservice.ReferenceTypes_;

@Component
@ServiceName("ConfigurationService")
public class ReferenceTypeHandler implements EventHandler {


	@Autowired
	Messages messages;

	@Autowired
	ReferenceTypeValidation validation;

	@Autowired
	ReferenceTypeService service;

	@Autowired
	private AuditLogHelper<ReferenceTypes> auditLogHelper;

	private static final Logger logger = LoggerFactory.getLogger(ReferenceTypeHandler.class);
	private static final String REFERENCE_TYPE_HANDLER = "ReferenceTypeHandler";

	/**
	 * Before create event is performing input validation
	 *
	 * @param {@link ReferenceTypes} referenceType
	 *
	 * @public
	 */
	@Before(event = {CdsService.EVENT_CREATE}, entity = ReferenceTypes_.CDS_NAME)
	public void beforeCreateReferenceType(ReferenceTypes referenceType) {
		LoggerHelper.logMethodEntry(logger, REFERENCE_TYPE_HANDLER,"beforeCreateReferenceType");
		validation.validateReferenceType(referenceType);
		messages.throwIfError();
		LoggerHelper.logMethodExit(logger, REFERENCE_TYPE_HANDLER,"beforeCreateReferenceType");
	}

	/**
	 * On create event is performing sequence number generation
	 *
	 * @param {@link ReferenceTypes} referenceType
	 *
	 * @public
	 */
	@On(event = {CdsService.EVENT_CREATE}, entity = ReferenceTypes_.CDS_NAME)
	public void onCreateReferenceType(ReferenceTypes referenceType) {
		LoggerHelper.logMethodEntry(logger, REFERENCE_TYPE_HANDLER,"onCreateReferenceType");
		Optional<Row> referenceTypeFirst=service.getReferenceTypes().first();
		Integer sequenceNumber = (referenceTypeFirst.isPresent()&&null!=referenceTypeFirst.get().get("identifier")) ? Integer.parseInt(referenceTypeFirst.get().get("identifier").toString()) + 1 : 1;
		referenceType.setIdentifier(sequenceNumber);
		LoggerHelper.logMethodExit(logger, REFERENCE_TYPE_HANDLER,"onCreateReferenceType");
	}

	/**
	 * Update create event is performing input validation on update
	 *
	 * @param {@link ReferenceTypes} referenceType
	 *
	 * @public
	 */
	@Before(event = {CdsService.EVENT_UPDATE}, entity = ReferenceTypes_.CDS_NAME)
	public void beforeUpdateReferenceType(ReferenceTypes referenceType) {
		LoggerHelper.logMethodEntry(logger, REFERENCE_TYPE_HANDLER,"beforeUpdateReferenceType");
		validation.validateReferenceType(referenceType);
		messages.throwIfError();
		setOldAuditData(referenceType);
		LoggerHelper.logMethodExit(logger, REFERENCE_TYPE_HANDLER,"beforeUpdateReferenceType");
	}
	@After(event = CdsService.EVENT_READ, entity = ReferenceTypes_.CDS_NAME)
	public void afterReferenceType(CdsReadEventContext context, Stream<ReferenceTypes> refType) {
		LoggerHelper.logMethodEntry(logger, REFERENCE_TYPE_HANDLER, "afterReferenceType");
		refType.forEach(b -> {
			if(Boolean.FALSE.equals(b.getIsActiveEntity()))
				b.setIsInActive(true);
			if (b.getIsActiveEntity() != null && b.getHasDraftEntity() != null && b.getIsActive() != null && b.getIsActiveEntity() && !b.getHasDraftEntity()) {
				b.setIsInActive(!b.getIsActive());
			}
		});
		LoggerHelper.logMethodExit(logger, REFERENCE_TYPE_HANDLER, "afterReferenceType");
	}

	@After(event = {CdsService.EVENT_UPDATE}, entity = ReferenceTypes_.CDS_NAME)
	public void afterUpdateReferenceType(ReferenceTypes referenceType) {
		LoggerHelper.logMethodEntry(logger, REFERENCE_TYPE_HANDLER,"afterUpdateReferenceType");
		logUpsert(Action.UPDATE, referenceType);
		LoggerHelper.logMethodExit(logger, REFERENCE_TYPE_HANDLER,"afterUpdateReferenceType");
	}

	@After(event = {CdsService.EVENT_CREATE}, entity = ReferenceTypes_.CDS_NAME)
	public void afterCreateReferenceType(ReferenceTypes referenceType) {
		LoggerHelper.logMethodEntry(logger, REFERENCE_TYPE_HANDLER,"afterCreateReferenceType");
		logUpsert(Action.CREATE, referenceType);
		LoggerHelper.logMethodExit(logger, REFERENCE_TYPE_HANDLER,"afterCreateReferenceType");
	}

	public void logUpsert(Action action, ReferenceTypes newData) {
		auditLogHelper.logConfigChange(ReferenceTypes_.CDS_NAME, action, newData);
	}

	public void setOldAuditData(ReferenceTypes referenceType) {
		ReferenceTypes oldData = service.getReferenceTypesDetails(referenceType.getId());
		auditLogHelper.setOldData(oldData);
	}
}