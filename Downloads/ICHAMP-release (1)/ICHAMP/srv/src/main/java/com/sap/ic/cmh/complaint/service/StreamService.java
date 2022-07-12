package com.sap.ic.cmh.complaint.service;

import com.sap.cds.Result;
import cds.gen.complaintservice.Streams;

public interface StreamService {

  void createAllStreams(String complaintId, String complaintType);

  Result getBusinessObject(String businessObjectID);

  Result getBusinessObjectsWithStatus(String complaint, String string, String string2);

  Result getStream(String id);

  void updateStreamStatus(String businessObjectId, String boType, boolean isActive);

  void getCurrentBOStatus(String complaintId, String boType, String streamId,
      String businessObjectId,boolean isActive);
    
  void updateComplaintStatusBasedOnAllStreamStatus(String complaintId);

  boolean checkIfAllRelevantStreamsClosed(String complaintId);

  String updateQualityStreamStatus(Streams stream);
}
