package com.sap.ic.cmh.configuration.action.context;

import cds.gen.configurationservice.ClaimStatusMappings;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.EventContext;
import com.sap.cds.services.EventName;
import com.sap.cds.services.cds.CdsService;

@EventName("CopyClaimStatusMappings")
public interface CopyClaimStatusMappingContext extends EventContext {

    CqnSelect getCqn();

    CdsService getService();

    void setResult(ClaimStatusMappings claimStatusMappings);

    void setCompleted();
}