package com.sap.ic.cmh.drm.model;

import java.util.Objects;


public class DataSubjectEndOfBusinessResponse {

    private Boolean dataSubjectExpired;

  private String dataSubjectNotExpiredReason;

  
  public DataSubjectEndOfBusinessResponse(Boolean dataSubjectExpired, String dataSubjectNotExpiredReason) {
	super();
	this.dataSubjectExpired = dataSubjectExpired;
	this.dataSubjectNotExpiredReason = dataSubjectNotExpiredReason;
}

public Boolean getDataSubjectExpired() {
    return dataSubjectExpired;
  }

  public void setDataSubjectExpired(Boolean dataSubjectExpired) {
    this.dataSubjectExpired = dataSubjectExpired;
  }

  public String getDataSubjectNotExpiredReason() {
    return dataSubjectNotExpiredReason;
  }

  public void setDataSubjectNotExpiredReason(String dataSubjectNotExpiredReason) {
    this.dataSubjectNotExpiredReason = dataSubjectNotExpiredReason;
  }
  

  @Override
  public int hashCode() {
    return Objects.hash(dataSubjectExpired, dataSubjectNotExpiredReason);
  }

  @Override
  public boolean equals(Object obj) {
    if (obj == null) {
      return false;
    }
    if (this == obj) {
      return true;
    }
    if (!(obj instanceof DataSubjectEndOfBusinessResponse)) {
      return false;
    }
    DataSubjectEndOfBusinessResponse other = (DataSubjectEndOfBusinessResponse) obj;
    return Objects.equals(dataSubjectExpired, other.dataSubjectExpired)
        && Objects.equals(dataSubjectNotExpiredReason, other.dataSubjectNotExpiredReason);
  }

  @Override
  public String toString() {
    return "DataSubjectEndOfBusiness [dataSubjectExpired=" + dataSubjectExpired + ", dataSubjectNotExpiredReason="
        + dataSubjectNotExpiredReason + "]";
  }

    
}
