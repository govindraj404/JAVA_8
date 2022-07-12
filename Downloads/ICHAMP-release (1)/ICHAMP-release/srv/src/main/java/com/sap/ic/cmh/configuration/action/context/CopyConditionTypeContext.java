package com.sap.ic.cmh.configuration.action.context;

import com.sap.cds.services.EventContext;
import com.sap.cds.services.EventName;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import cds.gen.configurationservice.ConditionTypes;

@EventName("CopyConditionTypes")
public interface CopyConditionTypeContext extends EventContext {

	CqnSelect getCqn();

	CdsService getService();

	void setResult(ConditionTypes conditionTypes);

	void setCompleted();
}