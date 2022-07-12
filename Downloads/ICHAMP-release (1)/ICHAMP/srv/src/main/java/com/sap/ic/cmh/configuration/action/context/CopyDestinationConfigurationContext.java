package com.sap.ic.cmh.configuration.action.context;

import cds.gen.configurationservice.DestinationConfigurations;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.EventContext;
import com.sap.cds.services.EventName;
import com.sap.cds.services.cds.CdsService;

@EventName("CopyDestinationConfigurations")
public interface CopyDestinationConfigurationContext extends EventContext {

	CqnSelect getCqn();

	CdsService getService();

	void setResult(DestinationConfigurations destinationConfigurations);

	void setCompleted();
}