package com.sap.ic.cmh.configuration.action.handler;

import cds.gen.configurationservice.CopySourceReferenceTypeMappingsContext;
import cds.gen.configurationservice.SourceReferenceTypeMappings;
import cds.gen.configurationservice.SourceReferenceTypeMappings_;
import cds.gen.configurationservice.TargetTypes_;
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
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

@Component
@ServiceName("ConfigurationService")
public class CopySourceReferenceTypeMappingHandler implements EventHandler {

    public static final Logger logger = LoggerHelper.getLogger(CopySourceReferenceTypeMappingHandler.class);
    private static final String COPY_SOURCE_REFERNCE_TYPE_MAPPING = "CopySourceReferenceTypeMappingHandler";

    @Autowired
    Messages msg;

    @Qualifier("ConfigurationService")
    @Autowired
    DraftService draftService;

    @On(entity = SourceReferenceTypeMappings_.CDS_NAME)
    public void copySourceReferenceTypeMappings(final CopySourceReferenceTypeMappingsContext context) {
        LoggerHelper.logMethodEntry(logger, COPY_SOURCE_REFERNCE_TYPE_MAPPING, "copySourceReferenceTypeMappings");
        CqnSelect select = context.getCqn();
        SourceReferenceTypeMappings sourceReferenceTypeMappings =
                ((CdsService) context.getService()).run(select)
                        .single(SourceReferenceTypeMappings.class);
        logger.info("Copy Source Reference Type Mapping Struct create starting");
        SourceReferenceTypeMappings copySourceReferenceTypeMappings = Struct.create(SourceReferenceTypeMappings.class);

        copySourceReferenceTypeMappings.setComplaintTypeId(sourceReferenceTypeMappings.getComplaintTypeId());
        copySourceReferenceTypeMappings.setDistributionChannelId(sourceReferenceTypeMappings.getDistributionChannelId());
        copySourceReferenceTypeMappings.setDivisionId(sourceReferenceTypeMappings.getDivisionId());
        copySourceReferenceTypeMappings.setSalesOrganizationId(sourceReferenceTypeMappings.getSalesOrganizationId());
        copySourceReferenceTypeMappings.setItemCategoryId(sourceReferenceTypeMappings.getItemCategoryId());
        copySourceReferenceTypeMappings.setReferenceTypes(sourceReferenceTypeMappings.getReferenceTypes());
        logger.info("Copy Source Reference Type Mapping result into context");

        context.setResult(
                draftService.newDraft(Insert.into(SourceReferenceTypeMappings_.class).entry(copySourceReferenceTypeMappings))
                        .single(SourceReferenceTypeMappings.class));

        msg.success(MessageKeys.SOURCE_REFERENCE_TYPE_MAPPING_OBJECT_COPIED, TargetTypes_.class);
        context.setCompleted();
        LoggerHelper.logMethodExit(logger, COPY_SOURCE_REFERNCE_TYPE_MAPPING, "copySourceReferenceTypeMappings");
    }
}

