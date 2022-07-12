package com.sap.ic.cmh.costcollector.validations;

import cds.gen.costcollectorservice.CostCollectors;

public interface CostCollectorValidation {

    public void validateCostCollectorFRAttributes(CostCollectors costCollector);

    public void validateCostCollectorSUBLAttributes(CostCollectors costCollector);

}
