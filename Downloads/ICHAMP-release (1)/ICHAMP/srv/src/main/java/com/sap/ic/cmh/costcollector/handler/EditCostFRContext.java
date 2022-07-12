package com.sap.ic.cmh.costcollector.handler;



import com.sap.cds.services.EventContext;
import com.sap.cds.services.EventName;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import cds.gen.costcollectorservice.CostCollectors;



@EventName("EditFR")
public interface EditCostFRContext extends EventContext {

    CqnSelect getCqn();

    CdsService getService();

    void setResult(CostCollectors cost);

    void setCompleted();

}
