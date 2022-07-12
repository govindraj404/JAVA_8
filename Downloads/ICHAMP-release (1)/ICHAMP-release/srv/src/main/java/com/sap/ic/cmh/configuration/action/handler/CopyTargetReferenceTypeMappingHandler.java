package com.sap.ic.cmh.configuration.action.handler;

import cds.gen.configurationservice.CopyTargetReferenceTypeMappingsContext;
import cds.gen.configurationservice.TargetReferenceTypeMappings;
import cds.gen.configurationservice.TargetReferenceTypeMappings_;
import cds.gen.configurationservice.TargetReferenceTypes;
import cds.gen.configurationservice.TargetTypes_;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;
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
import com.sap.ic.cmh.configuration.persistency.TargetReferenceTypeDao;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

@Component
@ServiceName("ConfigurationService")
public class CopyTargetReferenceTypeMappingHandler implements EventHandler {

    public static final Logger logger = LoggerHelper.getLogger(CopyTargetReferenceTypeMappingHandler.class);
    private static final String COPY_TARGET_REFERNCE_TYPE_MAPPING = "CopyTargetReferenceTypeMappingHandler";

    @Autowired
    Messages msg;

    @Autowired
    TargetReferenceTypeDao targetReferenceTypeDao;

    @Qualifier("ConfigurationService")
    @Autowired
    DraftService draftService;

    @On(entity = TargetReferenceTypeMappings_.CDS_NAME)
    public void copyTargetReferenceTypeMappings(final CopyTargetReferenceTypeMappingsContext context) {
        LoggerHelper.logMethodEntry(logger, COPY_TARGET_REFERNCE_TYPE_MAPPING, "copyTargetReferenceTypeMappings");
        CqnSelect select = context.getCqn();
        TargetReferenceTypeMappings targetReferenceTypeMappings =
                ((CdsService) context.getService()).run(select)
                        .single(TargetReferenceTypeMappings.class);
        logger.info("Copy Target Reference Type Mapping Struct create starting");
        TargetReferenceTypeMappings copyTargetReferenceTypeMappings = Struct.create(TargetReferenceTypeMappings.class);

        copyTargetReferenceTypeMappings.setComplaintTypeId(targetReferenceTypeMappings.getComplaintTypeId());
        copyTargetReferenceTypeMappings.setDistributionChannelId(targetReferenceTypeMappings.getDistributionChannelId());
        copyTargetReferenceTypeMappings.setDivisionId(targetReferenceTypeMappings.getDivisionId());
        copyTargetReferenceTypeMappings.setSalesOrganizationId(targetReferenceTypeMappings.getSalesOrganizationId());
        copyTargetReferenceTypeMappings.setItemCategoryId(targetReferenceTypeMappings.getItemCategoryId());
        copyTargetReferenceTypeMappings.setTargetTypes(targetReferenceTypeMappings.getTargetTypes());
        Result targetReferenceTypeResult = targetReferenceTypeDao.getTargetReferenceTypesBasedOnTargetMapping(targetReferenceTypeMappings.getId());
        if (targetReferenceTypeResult.rowCount() > 0) {
            List<Map<String, Object>> targetReferenceTypeList = new ArrayList<>();
            Stream<TargetReferenceTypes> targetReferenceTypeStream = targetReferenceTypeResult.streamOf(TargetReferenceTypes.class);
			targetReferenceTypeStream.forEach(b -> {
                Map<String, Object> copiedTargetReferenceTypes = new HashMap<>();
                copiedTargetReferenceTypes.put("targetType_ID",b.getTargetTypeId());
                copiedTargetReferenceTypes.put("destinationSystem",b.getDestinationSystem());
                targetReferenceTypeList.add(copiedTargetReferenceTypes);
            });
            copyTargetReferenceTypeMappings.setTargetTypes(targetReferenceTypeList);
        }
        logger.info("Copy Target Reference Type Mapping result into context");

        context.setResult(
                draftService.newDraft(Insert.into(TargetReferenceTypeMappings_.class).entry(copyTargetReferenceTypeMappings))
                        .single(TargetReferenceTypeMappings.class));

        msg.success(MessageKeys.TARGET_REFERENCE_TYPE_MAPPING_OBJECT_COPIED, TargetTypes_.class);
        context.setCompleted();
        LoggerHelper.logMethodExit(logger, COPY_TARGET_REFERNCE_TYPE_MAPPING, "copyTargetReferenceTypeMappings");
    }
}

