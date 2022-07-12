package com.sap.ic.cmh.configuration.action.handler;

import cds.gen.configurationservice.CopyTargetTypesContext;
import cds.gen.configurationservice.TargetTypes;
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
public class CopyTargetTypeConfigurationHandler implements EventHandler {

    public static final Logger logger = LoggerHelper.getLogger(CopyTargetTypeConfigurationHandler.class);
    private static final String COPY_TARGET_TYPE_CONFIGURATION = "CopyTargetTypeConfigurationHandler";

    @Autowired
    Messages msg;

    @Qualifier("ConfigurationService")
    @Autowired
    DraftService draftService;

    @On(entity = TargetTypes_.CDS_NAME)
    public void copyTargetTypes(final CopyTargetTypesContext context) {
        LoggerHelper.logMethodEntry(logger, COPY_TARGET_TYPE_CONFIGURATION, "copyTargetTypeConfiguration");
        CqnSelect select = context.getCqn();
        TargetTypes targetTypes =
                ((CdsService) context.getService()).run(select)
                        .single(TargetTypes.class);
        logger.info("Copy Target Type Configuration Struct create starting");
        TargetTypes copyTargetTypes = Struct.create(TargetTypes.class);

        copyTargetTypes.setCode(targetTypes.getCode());
        copyTargetTypes.setDescription(targetTypes.getDescription());
        copyTargetTypes.setTargetDocumentCategoryCode(targetTypes.getTargetDocumentCategoryCode());
        logger.info("Copy Target Type Configuration setting result into context");

        context.setResult(
                draftService.newDraft(Insert.into(TargetTypes_.class).entry(copyTargetTypes))
                        .single(TargetTypes.class));

        msg.success(MessageKeys.TARGET_TYPE_OBJECT_COPIED, TargetTypes_.class);
        context.setCompleted();
        LoggerHelper.logMethodExit(logger, COPY_TARGET_TYPE_CONFIGURATION, "copyTargetTypeConfiguration");
    }
}
