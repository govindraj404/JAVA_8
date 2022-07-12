package com.sap.ic.cmh.complaint.service;

import cds.gen.complaintservice.BTPUsers;
import com.sap.cds.Result;

import cds.gen.complaintservice.ComplaintStatuses;
import cds.gen.complaintservice.ComplaintCategories;
import cds.gen.complaintservice.Complaints;

import java.math.BigDecimal;
import java.util.List;

public interface ComplaintService {

    public void updateComplaintWithCost(String comp, BigDecimal cost);

    public void updateComplaintWithQuantity(String comp, BigDecimal quantity);

    public ComplaintStatuses getDefaultStatus();

    public ComplaintCategories getDefaultType();

    public void updateComplaintStatus(String id, String status);

    public Complaints getComplaintDetails(String id);

    public Result getAllComplaints();

    public void checkActiveComplaintsForDeleteMasterData(Complaints complaint);

    public void validateComplaintAttributes(Complaints complaint);

    public List<BTPUsers> getAllResponsiblePerson();

    public void deleteBusinessPartnerWhenComplaintClosed(Complaints complaint);

    public Boolean getIsComplaintStatusClosedBasedOnBusinessPartner(String businessPartnerId);

    public void deleteComplaints(String complaintId);

    public Complaints getMasterDataFromComplaints(String complaintId);

    public void validateComplaintStatus(String complaintId);

    public Complaints getComplaintCreationTypeAndCompanyCode(String complaintId);
}
