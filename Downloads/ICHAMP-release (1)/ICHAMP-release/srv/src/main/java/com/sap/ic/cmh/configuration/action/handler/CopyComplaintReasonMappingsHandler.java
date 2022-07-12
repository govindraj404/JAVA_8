package com.sap.ic.cmh.configuration.action.handler;

import cds.gen.configurationservice.ComplaintReasonMappings;
import cds.gen.configurationservice.ComplaintReasonMappings_;
import com.sap.ic.cmh.gen.MessageKeys;

import com.sap.cds.Struct;
import com.sap.cds.ql.Insert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.draft.DraftService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import cds.gen.configurationservice.CopyComplaintReasonMappingsContext;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import org.slf4j.Logger;

@Component
@ServiceName("ConfigurationService")
public class CopyComplaintReasonMappingsHandler implements EventHandler {

    public static final Logger logger = LoggerHelper.getLogger(CopyComplaintReasonMappingsHandler.class);
    private static final String COPY_COMPLAINT_REASON_MAPPINGS_HANDLER = "CopyComplaintReasonMappingsHandler";
    @Autowired
    Messages messages;

    @Qualifier("ConfigurationService")
    @Autowired
    DraftService draftService;

    /**
     * Copy event of CopyComplaintReasonMappingsHandler.
     *
     * @public
     */
    @On(entity = ComplaintReasonMappings_.CDS_NAME)
    public void copyComplaintReasonMappings(final CopyComplaintReasonMappingsContext context) {
        LoggerHelper.logMethodEntry(logger, COPY_COMPLAINT_REASON_MAPPINGS_HANDLER, "copyComplaintReasonMappingsContext");
        CqnSelect select = context.getCqn();
        ComplaintReasonMappings salesComplaintItemReasons =
                ((CdsService) context.getService()).run(select)
                        .single(ComplaintReasonMappings.class);
        logger.info("Copy Complaint Reason Mappings Context create starting");
        ComplaintReasonMappings copySalesComplaintItemReasons = Struct.create(ComplaintReasonMappings.class);

        copySalesComplaintItemReasons.setSalesOrganizationId(salesComplaintItemReasons.getSalesOrganizationId());
        copySalesComplaintItemReasons.setDistributionChannelId(salesComplaintItemReasons.getDistributionChannelId());
        copySalesComplaintItemReasons.setDivisionId(salesComplaintItemReasons.getDivisionId());
        copySalesComplaintItemReasons.setItemCategoryId(salesComplaintItemReasons.getItemCategoryId());
        copySalesComplaintItemReasons.setComplaintReasonId(salesComplaintItemReasons.getComplaintReasonId());
        logger.info("Copy Complaint Reason Mappings setting result into context");

        context.setResult(
                draftService.newDraft(Insert.into(ComplaintReasonMappings_.class).entry(copySalesComplaintItemReasons))
                        .single(ComplaintReasonMappings.class));

        messages.success( MessageKeys.COMPLAINT_REASON_MAP_COPIED, ComplaintReasonMappings_.class);

        context.setCompleted();
        LoggerHelper.logMethodExit(logger, COPY_COMPLAINT_REASON_MAPPINGS_HANDLER, "copyComplaintReasonMappingsContext");
    }
}

