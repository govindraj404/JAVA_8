package com.sap.ic.cmh.configuration.action.context;

import cds.gen.configurationservice.ServiceMaterials;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.EventContext;
import com.sap.cds.services.EventName;
import com.sap.cds.services.cds.CdsService;

@EventName("CopyServiceMaterials")
public interface CopyServiceMaterialContext extends EventContext {
	CqnSelect getCqn();

	CdsService getService();

	void setResult(ServiceMaterials serviceMaterials);

	void setCompleted();

}