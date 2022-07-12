package com.sap.ic.cmh.configuration.action.handler;

import cds.gen.configurationservice.DestinationConfigurations;
import cds.gen.configurationservice.DestinationConfigurations_;
import com.sap.cds.Struct;
import com.sap.cds.ql.Insert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.ic.cmh.network.service.DestinationService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import com.sap.cds.services.draft.DraftService;
import org.springframework.beans.factory.annotation.Qualifier;
import com.sap.ic.cmh.configuration.action.context.CopyDestinationConfigurationContext;

@Component
@ServiceName("ConfigurationService")
public class CopyDestinationConfigurationHandler implements EventHandler {

    @Autowired
    DestinationService destinationService;

    private final DraftService draftService;

    CopyDestinationConfigurationHandler(@Qualifier("ConfigurationService") DraftService draftService) {
        this.draftService = draftService;
    }

    @On(entity = DestinationConfigurations_.CDS_NAME)
    public void copyDestinationConfigurations(final CopyDestinationConfigurationContext context) {

        CqnSelect select = context.getCqn();
        CdsService service = context.getService();
        DestinationConfigurations destinationConfigurations = (service).run(select)
                .single(DestinationConfigurations.class);

        DestinationConfigurations copyDestinationConfiguration = Struct.create(DestinationConfigurations.class);
        copyDestinationConfiguration.setCompanyCodeId(destinationConfigurations.getCompanyCodeId());
        copyDestinationConfiguration.setBusinessObjectTypeCode(destinationConfigurations.getBusinessObjectTypeCode());
        copyDestinationConfiguration.setDestination(destinationConfigurations.getDestination());
        copyDestinationConfiguration.setDescription(destinationConfigurations.getDescription());
        copyDestinationConfiguration.setNavigationDestination(destinationConfigurations.getNavigationDestination());
        context.setResult(
                draftService.newDraft(Insert.into(DestinationConfigurations_.class).entry(copyDestinationConfiguration))
                        .single(DestinationConfigurations.class));
        context.setCompleted();
    }
}
