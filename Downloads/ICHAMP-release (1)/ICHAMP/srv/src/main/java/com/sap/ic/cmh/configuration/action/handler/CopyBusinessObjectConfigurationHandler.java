package com.sap.ic.cmh.configuration.action.handler;

import cds.gen.configurationservice.BusinessObjectConfigurations;
import cds.gen.configurationservice.BusinessObjectConfigurations_;
import com.sap.cds.ql.Insert;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import org.springframework.stereotype.Component;
import com.sap.cds.services.draft.DraftService;
import org.springframework.beans.factory.annotation.Qualifier;
import com.sap.ic.cmh.configuration.action.context.CopyBusinessObjectConfigurationContext;

@Component
@ServiceName("ConfigurationService")
public class CopyBusinessObjectConfigurationHandler implements EventHandler {

        private final DraftService draftService;

        CopyBusinessObjectConfigurationHandler(@Qualifier("ConfigurationService") DraftService draftService) {
                this.draftService = draftService;
        }

        @On(entity = BusinessObjectConfigurations_.CDS_NAME)
        public void copyBusinessObjectConfiguration(final CopyBusinessObjectConfigurationContext context) {

                CqnSelect select = context.getCqn();
                CdsService service = context.getService();
                BusinessObjectConfigurations businessObjectConfigurations = (service)
                                .run(select).single(BusinessObjectConfigurations.class);

                BusinessObjectConfigurations copyBusinessObjectConfiguration = Struct
                                .create(BusinessObjectConfigurations.class);
                copyBusinessObjectConfiguration
                                .setComplaintTypeCode(businessObjectConfigurations.getComplaintTypeCode());
                copyBusinessObjectConfiguration
                                .setBusinessObjectTypeCode(businessObjectConfigurations.getBusinessObjectTypeCode());
                copyBusinessObjectConfiguration.setBusinessObjectAttributeCode(
                                businessObjectConfigurations.getBusinessObjectAttributeCode());
                copyBusinessObjectConfiguration.setDestination(businessObjectConfigurations.getDestination());
                copyBusinessObjectConfiguration
                                .setBusinessObjectValue(businessObjectConfigurations.getBusinessObjectValue());

                context.setResult(draftService
                                .newDraft(Insert.into(BusinessObjectConfigurations_.class)
                                                .entry(copyBusinessObjectConfiguration))
                                .single(BusinessObjectConfigurations.class));
                context.setCompleted();
        }
}