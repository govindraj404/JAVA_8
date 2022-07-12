package com.sap.ic.cmh.configuration.action.handler;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import com.sap.cds.Struct;
import com.sap.cds.ql.Insert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.draft.DraftService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.configurationservice.ComplaintTypeToItemCategoryMappings;
import cds.gen.configurationservice.ComplaintTypeToItemCategoryMappings_;
import cds.gen.configurationservice.CopyComplaintTypeToItemCategoryMappingsContext;

@Component
@ServiceName("ConfigurationService")
public class CopyComplaintTypeToItemCatMappingHandler implements EventHandler {

	@Autowired
	Messages messages;

	@Qualifier("ConfigurationService")
	@Autowired
	DraftService draftService;

	private static final Logger logger = LoggerFactory.getLogger(CopyComplaintTypeToItemCatMappingHandler.class);
	private static final String COPY_COMPLAINT_TYPE_CONFIG_HANDLER = "CopyComplaintTypeToItemCatMappingHandler";

	/**
	 * Perform Copy of Complaint Type To Item Category Mapping
	 * 
	 * @param context
	 */
	@On(entity = ComplaintTypeToItemCategoryMappings_.CDS_NAME)
	public void copyComplaintTypeToItemCategoryMapping(final CopyComplaintTypeToItemCategoryMappingsContext context) {
		LoggerHelper.logMethodEntry(logger, COPY_COMPLAINT_TYPE_CONFIG_HANDLER,
				"copyComplaintTypeToItemCategoryMapping");
		CqnSelect select = context.getCqn();
		CdsService service = (CdsService) context.getService();
		ComplaintTypeToItemCategoryMappings complaintTypeToItemCategoryMapping = (service).run(select)
				.single(ComplaintTypeToItemCategoryMappings.class);
		ComplaintTypeToItemCategoryMappings copyComplaintTypeToItemCategoryMapping = Struct
				.create(ComplaintTypeToItemCategoryMappings.class);
		copyComplaintTypeToItemCategoryMapping
				.setComplaintTypeId(complaintTypeToItemCategoryMapping.getComplaintTypeId());
		copyComplaintTypeToItemCategoryMapping
				.setItemCategoryId(complaintTypeToItemCategoryMapping.getItemCategoryId());
		copyComplaintTypeToItemCategoryMapping
				.setSalesOrganizationId(complaintTypeToItemCategoryMapping.getSalesOrganizationId());
		copyComplaintTypeToItemCategoryMapping
				.setDistributionChannelId(complaintTypeToItemCategoryMapping.getDistributionChannelId());
		copyComplaintTypeToItemCategoryMapping.setDivisionId(complaintTypeToItemCategoryMapping.getDivisionId());
		context.setResult(draftService
				.newDraft(Insert.into(ComplaintTypeToItemCategoryMappings_.class)
						.entry(copyComplaintTypeToItemCategoryMapping))
				.single(ComplaintTypeToItemCategoryMappings.class));
		messages.success(MessageKeys.COPY_COMPLAINT_TYPE_ITEM_CAT_MAPPING_ACTION_SUCCESSFULLY_PERFORMED);
		context.setCompleted();
		LoggerHelper.logMethodExit(logger, COPY_COMPLAINT_TYPE_CONFIG_HANDLER,
				"copyComplaintTypeToItemCategoryMapping");
	}

}
