package com.sap.ic.cmh.complaint.validation;

import cds.gen.complaintservice.BusinessObjects;
import cds.gen.complaintservice.Streams;

public interface StreamValidation {

    public void validateStreamEdit(Streams streams);

    public void validateBusinessObjectsEdit(BusinessObjects eachBo);


}
