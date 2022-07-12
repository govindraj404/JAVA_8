package com.sap.ic.cmh.drm.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

@JsonInclude(Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public class DataSubject {

     public DataSubject(String dataSubjectID) {
    super();
    this.dataSubjectID = dataSubjectID;
  }

  public DataSubject() {
    super();
  }

  private String dataSubjectID;

  private String dataSubjectRole;

  private String legalGround;

  public String getDataSubjectID() {
    return dataSubjectID;
  }

  public void setDataSubjectID(String dataSubjectID) {
    this.dataSubjectID = dataSubjectID;
  }

  public String getDataSubjectRole() {
    return dataSubjectRole;
  }

  public void setDataSubjectRole(String dataSubjectRole) {
    this.dataSubjectRole = dataSubjectRole;
  }

  public String getLegalGround() {
    return legalGround;
  }

  public void setLegalGround(String legalGround) {
    this.legalGround = legalGround;
  }

  @Override
  public int hashCode() {
    return Objects.hash(dataSubjectID, dataSubjectRole, legalGround);
  }

  @Override
  public boolean equals(Object obj) {
    if (obj == null) {
      return false;
    }
    if (this == obj) {
      return true;
    }
    if (!(obj instanceof DataSubject)) {
      return false;
    }
    DataSubject other = (DataSubject) obj;
    return Objects.equals(dataSubjectID, other.dataSubjectID) && Objects.equals(dataSubjectRole, other.dataSubjectRole)
        && Objects.equals(legalGround, other.legalGround);
  }

  @Override
  public String toString() {
    return "DataSubject [dataSubjectID=" + dataSubjectID + ", dataSubjectRole=" + dataSubjectRole + ", legalGround="
        + legalGround + "]";
  }


    
}
