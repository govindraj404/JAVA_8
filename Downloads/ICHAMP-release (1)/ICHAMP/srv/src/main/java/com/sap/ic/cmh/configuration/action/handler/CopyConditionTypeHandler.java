package com.sap.ic.cmh.configuration.action.handler;

import cds.gen.configurationservice.ConditionTypes;
import cds.gen.configurationservice.ConditionTypes_;
import com.sap.cds.Struct;
import com.sap.cds.ql.Insert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import org.springframework.stereotype.Component;
import com.sap.cds.services.draft.DraftService;
import org.springframework.beans.factory.annotation.Qualifier;
import com.sap.ic.cmh.configuration.action.context.CopyConditionTypeContext;

@Component
@ServiceName("ConfigurationService")
public class CopyConditionTypeHandler implements EventHandler {

    private final DraftService draftService;

    CopyConditionTypeHandler(@Qualifier("ConfigurationService") DraftService draftService) {
        this.draftService = draftService;
    }

    @On(entity = ConditionTypes_.CDS_NAME)
    public void copyConditionType(final CopyConditionTypeContext context) {

        CqnSelect select = context.getCqn();
        CdsService service = context.getService();
        ConditionTypes conditionTypes = (service).run(select).single(ConditionTypes.class);
        ConditionTypes copyConditionType = Struct.create(ConditionTypes.class);
        copyConditionType.setItemTypeCode(conditionTypes.getItemTypeCode());
        copyConditionType.setConditionType(conditionTypes.getConditionType());
        copyConditionType.setDestination(conditionTypes.getDestination());
        copyConditionType.setDescription(conditionTypes.getDescription());
        context.setResult(draftService.newDraft(Insert.into(ConditionTypes_.class).entry(copyConditionType))
                .single(ConditionTypes.class));
        context.setCompleted();
    }
}