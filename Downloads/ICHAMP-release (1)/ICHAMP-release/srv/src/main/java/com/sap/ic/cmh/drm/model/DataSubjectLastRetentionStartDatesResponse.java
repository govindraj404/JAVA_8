package com.sap.ic.cmh.drm.model;

import java.util.Objects;

public class DataSubjectLastRetentionStartDatesResponse {
  private String retentionID;

  private String retentionStartDate;

  
  public DataSubjectLastRetentionStartDatesResponse(String retentionID, String retentionStartDate) {
	super();
	this.retentionID = retentionID;
	this.retentionStartDate = retentionStartDate;
  }

public String getRetentionID() {
    return retentionID;
  }

  public void setRetentionID(String retentionID) {
    this.retentionID = retentionID;
  }

  public String getRetentionStartDate() {
    return retentionStartDate;
  }

  public void setRetentionStartDate(String retentionStartDate) {
    this.retentionStartDate = retentionStartDate;
  }

  @Override
  public int hashCode() {
    return Objects.hash(retentionID, retentionStartDate);
  }

  @Override
  public boolean equals(Object obj) {
    if (obj == null) {
      return false;
    }
    if (this == obj) {
      return true;
    }
    if (!(obj instanceof DataSubjectLastRetentionStartDatesResponse)) {
      return false;
    }
    DataSubjectLastRetentionStartDatesResponse other = (DataSubjectLastRetentionStartDatesResponse) obj;
    return Objects.equals(retentionID, other.retentionID)
        && Objects.equals(retentionStartDate, other.retentionStartDate);
  }

  @Override
  public String toString() {
    return "Retention [retentionID=" + retentionID + ", retentionStartDate=" + retentionStartDate + "]";
  }

}
