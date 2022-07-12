package com.sap.ic.cmh.complaint.validation;

import cds.gen.complaintservice.Complaints;

public interface ComplaintValidation {
    void validateComplaintBeforeCreate(Complaints complaint);

    void validateComplaintFreeTextFields(Complaints complaint);

    void validateComplaintAttributes(Complaints complaint);

    void validatePlant(String plantId);

    boolean validateResponsiblePerson(String personResponsible);
}
