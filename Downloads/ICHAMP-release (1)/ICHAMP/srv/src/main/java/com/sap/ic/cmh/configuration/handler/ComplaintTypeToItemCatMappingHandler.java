package com.sap.ic.cmh.configuration.handler;

import java.util.Optional;
import java.util.stream.Stream;
import com.sap.cds.services.auditlog.Action;
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
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.configuration.service.ComplaintTypeToItemCatMappingService;
import com.sap.ic.cmh.configuration.validations.ComplaintTypeToItemCatMappingValidation;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.configurationservice.ComplaintTypeToItemCategoryMappings;
import cds.gen.configurationservice.ComplaintTypeToItemCategoryMappings_;

@Component
@ServiceName("ConfigurationService")
public class ComplaintTypeToItemCatMappingHandler implements EventHandler {

	@Autowired
	Messages messages;
	@Autowired
	ComplaintTypeToItemCatMappingValidation complaintTypeToItemCatMappingValidation;
	@Autowired
	ComplaintTypeToItemCatMappingService complaintTypeToItemCatMappingService;
    @Autowired
	private AuditLogHelper<ComplaintTypeToItemCategoryMappings> auditLogHelper;

	private static final Logger logger = LoggerFactory.getLogger(ComplaintTypeToItemCatMappingHandler.class);
	private static final String COMPLAINT_TYPE_ITEM_CAT_MAPPING_HANDLER = "ComplaintTypeToItemCatMappingHandler";
   
	/**
	 * Perform input validation and unique combination check for ComplaintTypeToItemCategoryMappings before creating
	 * @param complaintTypeToItemCategoryMappings
	 */
	@Before(event = { CdsService.EVENT_CREATE }, entity = ComplaintTypeToItemCategoryMappings_.CDS_NAME)
	public void beforeComplaintTypeToItemCategoryMappingCreate(
			ComplaintTypeToItemCategoryMappings complaintTypeToItemCategoryMappings) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_TYPE_ITEM_CAT_MAPPING_HANDLER,
				"beforeComplaintTypeToItemCategoryMappingCreate");
		complaintTypeToItemCatMappingValidation
				.validateComplaintTypeToItemCategoryMappings(complaintTypeToItemCategoryMappings);
		messages.throwIfError();
		LoggerHelper.logMethodExit(logger, COMPLAINT_TYPE_ITEM_CAT_MAPPING_HANDLER,
				"beforeComplaintTypeToItemCategoryMappingCreate");
	}

	/**
	 * Set sequence number for complaintTypeToItemCategoryMappings
	 * @param complaintTypeToItemCategoryMappings
	 */
	@On(event = { CdsService.EVENT_CREATE }, entity = ComplaintTypeToItemCategoryMappings_.CDS_NAME)
	public void onComplaintTypeToItemCategoryMappingCreate(ComplaintTypeToItemCategoryMappings complaintTypeToItemCategoryMappings) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_TYPE_ITEM_CAT_MAPPING_HANDLER, "onComplaintTypeToItemCategoryMappingCreate");
		Optional<Row> complaintTypetoItemCatMappingFirst = complaintTypeToItemCatMappingService.getComplaintTypeToItemCatMapping().first();
		Integer sequenceNumber = (complaintTypetoItemCatMappingFirst.isPresent() && null != complaintTypetoItemCatMappingFirst.get().get("identifier"))
				? Integer.parseInt(complaintTypetoItemCatMappingFirst.get().get("identifier").toString()) + 1
				: 1;
		complaintTypeToItemCategoryMappings.setIdentifier(sequenceNumber);
        logUpsert(Action.CREATE, complaintTypeToItemCategoryMappings);
		LoggerHelper.logMethodExit(logger, COMPLAINT_TYPE_ITEM_CAT_MAPPING_HANDLER, "onComplaintTypeToItemCategoryMappingCreate");
	}
	
	/**
	 * Perform input validation and unique combination check for ComplaintTypeToItemCategoryMappings before updating
	 * @param complaintTypeToItemCategoryMappings
	 */
	@Before(event = { CdsService.EVENT_UPDATE }, entity = ComplaintTypeToItemCategoryMappings_.CDS_NAME)
	public void beforeComplaintTypeToItemCategoryMappingUpdate(
			ComplaintTypeToItemCategoryMappings complaintTypeToItemCategoryMappings) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_TYPE_ITEM_CAT_MAPPING_HANDLER,
				"beforeComplaintTypeToItemCategoryMappingUpdate");
		complaintTypeToItemCatMappingValidation
				.validateComplaintTypeToItemCategoryMappings(complaintTypeToItemCategoryMappings);
		messages.throwIfError();
        setOldAuditData(complaintTypeToItemCategoryMappings);
		LoggerHelper.logMethodExit(logger, COMPLAINT_TYPE_ITEM_CAT_MAPPING_HANDLER,
				"beforeComplaintTypeToItemCategoryMappingUpdate");
	}
	@After(event = CdsService.EVENT_READ, entity = ComplaintTypeToItemCategoryMappings_.CDS_NAME)
	public void afterComplaintTypeToItemCatMapRead(CdsReadEventContext context, Stream<ComplaintTypeToItemCategoryMappings> compRsnType) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_TYPE_ITEM_CAT_MAPPING_HANDLER,"afterComplaintTypeToItemCatMapRead");
		compRsnType.forEach(b -> {
				b.setIsInActive(true);
			if (b.getIsActiveEntity() != null && b.getHasDraftEntity() != null && b.getIsActive() != null &&
					Boolean.TRUE.equals(b.getIsActiveEntity()) && Boolean.FALSE.equals(b.getHasDraftEntity())) {
				b.setIsInActive(!b.getIsActive());
			}
		});
		LoggerHelper.logMethodExit(logger, COMPLAINT_TYPE_ITEM_CAT_MAPPING_HANDLER, "afterComplaintTypeToItemCatMapRead");

	}
	/**
	 * After Create event of ComplaintTypeToItemCatMappingHandler perform validations
	 * @param {@link ComplaintTypeToItemCategoryMappings} compTypeToItemCatMap
	 * @public
	 */
    @After(event = {CdsService.EVENT_UPDATE}, entity = ComplaintTypeToItemCategoryMappings_.CDS_NAME)
	public void afterUpdateCompTypeToItemCatMap(ComplaintTypeToItemCategoryMappings compTypeToItemCatMap) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_TYPE_ITEM_CAT_MAPPING_HANDLER,"afterUpdateCompTypeToItemCatMap");
		logUpsert(Action.UPDATE, compTypeToItemCatMap);
		LoggerHelper.logMethodExit(logger, COMPLAINT_TYPE_ITEM_CAT_MAPPING_HANDLER,"afterUpdateCompTypeToItemCatMap");
	}
	/**
	 * After Create event of ComplaintTypeToItemCatMappingHandler perform validations
	 *
	 * @param {@link ComplaintTypeToItemCategoryMappings} compTypeToItemCatMap
	 *
	 * @public
	 */
	@After(event = {CdsService.EVENT_CREATE}, entity = ComplaintTypeToItemCategoryMappings_.CDS_NAME)
	public void afterCreateCompTypeToItemCatMap(ComplaintTypeToItemCategoryMappings compTypeToItemCatMap) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_TYPE_ITEM_CAT_MAPPING_HANDLER,"afterCreateCompTypeToItemCatMap");
		logUpsert(Action.CREATE, compTypeToItemCatMap);
		LoggerHelper.logMethodExit(logger, COMPLAINT_TYPE_ITEM_CAT_MAPPING_HANDLER,"afterCreateCompTypeToItemCatMap");
	}
	/**
	 * @param action
	 * @param newData
	 */
	public void logUpsert(Action action, ComplaintTypeToItemCategoryMappings newData) {
		auditLogHelper.logConfigChange(ComplaintTypeToItemCategoryMappings_.CDS_NAME, action, newData);
	}
	/**
	 * Performs setting old Auditlog Data
	 * @param compTypeToItemCatMap
	 */
	public void setOldAuditData(ComplaintTypeToItemCategoryMappings compTypeToItemCatMap) {
		ComplaintTypeToItemCategoryMappings oldData = complaintTypeToItemCatMappingService.getComplaintTypeToItemCatMappingsDetails(compTypeToItemCatMap.getId());
		auditLogHelper.setOldData(oldData);
	}
}
