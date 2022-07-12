package com.sap.ic.cmh.configuration.action.context;

import cds.gen.configurationservice.BusinessObjectConfigurations;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.EventContext;
import com.sap.cds.services.EventName;
import com.sap.cds.services.cds.CdsService;

@EventName("CopyBusinessObjectConfigurations")
public interface CopyBusinessObjectConfigurationContext extends EventContext {
    CqnSelect getCqn();

    CdsService getService();

    void setResult(BusinessObjectConfigurations businessObjectConfigurations);

    void setCompleted();

}