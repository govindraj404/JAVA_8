package com.sap.ic.cmh.supplierissueprocess.service;

import cds.gen.supplierissueprocessservice.Supplier8DProcesses;

public interface EightDService {

    void create8D(Supplier8DProcesses eightD, String userId);

    Supplier8DProcesses getDraftEightDByComplaintID(String complaintId);

    void deleteDraftEightDByID(String eightDId);

    void setConfiguredValues(Supplier8DProcesses qn, String boType, String complaintTypeCode);

    void validate8DDetails(Supplier8DProcesses eightD);

    Supplier8DProcesses getEightDDetails(String eightDId);

    Supplier8DProcesses getEightDBasedOnId(String eightDId);

    void validateIf8DExistsForComplaint(String complaintId);

    Supplier8DProcesses getEightDDetailsBasedOnComplaintId(String complaintId);

    void validateIf8DExists(Supplier8DProcesses eightD);

    void mapEightDStatus();
}
