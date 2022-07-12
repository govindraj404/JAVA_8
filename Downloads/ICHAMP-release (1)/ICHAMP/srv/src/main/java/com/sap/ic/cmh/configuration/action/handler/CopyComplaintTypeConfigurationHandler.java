package com.sap.ic.cmh.configuration.action.handler;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.Insert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.draft.DraftService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.configuration.persistency.ComplaintTypeToSalesAreaMappingDao;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.configurationservice.ComplaintTypeConfigurations;
import cds.gen.configurationservice.ComplaintTypeConfigurations_;
import cds.gen.configurationservice.ComplaintTypeToSalesAreaMappings;
import cds.gen.configurationservice.CopyComplaintTypeConfigurationsContext;

@Component
@ServiceName("ConfigurationService")
public class CopyComplaintTypeConfigurationHandler implements EventHandler {

	@Autowired
	Messages messages;

	@Autowired
	ComplaintTypeToSalesAreaMappingDao complaintTypeToSalesAreaMappingDao;
   
	@Qualifier("ConfigurationService")
	@Autowired
	DraftService draftService;
   
	private static final Logger logger = LoggerFactory.getLogger(CopyComplaintTypeConfigurationHandler.class);
	private static final String COPY_COMPLAINT_TYPE_CONFIG_HANDLER = "CopyComplaintTypeConfigurationHandler";

	/**
	 * copy action on complaint type
	 * 
	 * @param {@link CopyComplaintTypeConfigurationsContext} context
	 * 
	 * @public
	 */
	@On(entity = ComplaintTypeConfigurations_.CDS_NAME)
	public void copyComplaintTypeConfiguration(final CopyComplaintTypeConfigurationsContext context) {
		LoggerHelper.logMethodEntry(logger, COPY_COMPLAINT_TYPE_CONFIG_HANDLER, "copyComplaintTypeConfiguration");
		CqnSelect select = context.getCqn();
		CdsService service = (CdsService)context.getService();
		ComplaintTypeConfigurations complaintTypeConfig = (service).run(select).single(ComplaintTypeConfigurations.class);
		ComplaintTypeConfigurations copyComplaintTypeConfig = Struct.create(ComplaintTypeConfigurations.class);
		copyComplaintTypeConfig.setComplaintCategoryCode(complaintTypeConfig.getComplaintCategoryCode());
		copyComplaintTypeConfig.setCode(complaintTypeConfig.getCode());
		copyComplaintTypeConfig.setDescription(complaintTypeConfig.getDescription());
		copyComplaintTypeConfig.setIndividualComplaintType(complaintTypeConfig.getIndividualComplaintType());
		copyComplaintTypeConfig.setItemCategoryId(complaintTypeConfig.getItemCategoryId());
		copyComplaintTypeConfig.setIsFieldControlIndividualComplaintType(complaintTypeConfig.getIsFieldControlIndividualComplaintType());

		Result salesAreaMappingResult = complaintTypeToSalesAreaMappingDao
				.getSalesAreaMappingBasedOnComplaintTypeConfig(complaintTypeConfig.getId());

		copyComplaintTypeConfig.setComplaintTypeToSalesAreaMappings(
				salesAreaMappingResult.listOf(ComplaintTypeToSalesAreaMappings.class));
		context.setResult(
				draftService.newDraft(Insert.into(ComplaintTypeConfigurations_.class).entry(copyComplaintTypeConfig))
						.single(ComplaintTypeConfigurations.class));
		messages.success(MessageKeys.COPY_COMPLAINT_TYPE_CONFIG_ACTION_SUCCESSFULLY_PERFORMED);
		context.setCompleted();
		LoggerHelper.logMethodExit(logger, COPY_COMPLAINT_TYPE_CONFIG_HANDLER, "copyComplaintTypeConfiguration");
	}
}
