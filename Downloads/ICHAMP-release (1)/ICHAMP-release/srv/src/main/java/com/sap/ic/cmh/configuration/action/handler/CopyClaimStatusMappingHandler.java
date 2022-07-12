package com.sap.ic.cmh.configuration.action.handler;

import cds.gen.configurationservice.ClaimStatusMappings;
import cds.gen.configurationservice.ClaimStatusMappings_;
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
import com.sap.ic.cmh.configuration.action.context.CopyClaimStatusMappingContext;

@Component
@ServiceName("ConfigurationService")
public class CopyClaimStatusMappingHandler implements EventHandler {

    private final DraftService draftService;

    CopyClaimStatusMappingHandler(@Qualifier("ConfigurationService") DraftService draftService) {
        this.draftService = draftService;
    }

    @On(entity = ClaimStatusMappings_.CDS_NAME)
    public void copyClaimStatusMapping(final CopyClaimStatusMappingContext context) {

        CqnSelect select = context.getCqn();
        CdsService service = context.getService();
      ClaimStatusMappings claimStatusMappings = (service).run(select)
      .single(ClaimStatusMappings.class);

        ClaimStatusMappings copyClaimStatusMapping = Struct.create(ClaimStatusMappings.class);

        copyClaimStatusMapping.setCode(claimStatusMappings.getCode());
        copyClaimStatusMapping.setStatusCode(claimStatusMappings.getStatusCode());
        copyClaimStatusMapping.setName(claimStatusMappings.getName());

        context.setResult(draftService.newDraft(Insert.into(ClaimStatusMappings_.class).entry(copyClaimStatusMapping))
                .single(ClaimStatusMappings.class));
        context.setCompleted();
    }
}