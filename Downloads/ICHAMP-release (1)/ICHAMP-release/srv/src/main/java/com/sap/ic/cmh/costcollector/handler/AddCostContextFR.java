package com.sap.ic.cmh.costcollector.handler;



import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.EventContext;
import com.sap.cds.services.EventName;
import com.sap.cds.services.cds.CdsService;
import cds.gen.costcollectorservice.Complaints;



@EventName("AddFR")
public interface AddCostContextFR extends EventContext {

    CqnSelect getCqn();

    CdsService getService();

    void setResult(Complaints cost);

    void setCompleted();

}
