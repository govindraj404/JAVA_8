package com.sap.ic.cmh.drm.model;

import java.util.List;
import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class DataSubjectLegalGroundDeletionRequest {

  private String legalGround;

  private String dataSubjectRole;

  private String dataSubjectID;

  private String startTime;

  private String maxDeletionDate;

  private List<RetentionRule> retentionRules;

  public String getLegalGround() {
    return legalGround;
  }

  public void setLegalGround(String legalGround) {
    this.legalGround = legalGround;
  }

  public String getDataSubjectRole() {
    return dataSubjectRole;
  }

  public void setDataSubjectRole(String dataSubjectRole) {
    this.dataSubjectRole = dataSubjectRole;
  }

  public String getDataSubjectID() {
    return dataSubjectID;
  }

  public void setDataSubjectID(String dataSubjectID) {
    this.dataSubjectID = dataSubjectID;
  }

  public String getStartTime() {
    return startTime;
  }

  public void setStartTime(String startTime) {
    this.startTime = startTime;
  }

  public String getMaxDeletionDate() {
    return maxDeletionDate;
  }

  public void setMaxDeletionDate(String maxDeletionDate) {
    this.maxDeletionDate = maxDeletionDate;
  }

  public List<RetentionRule> getRetentionRules() {
    return retentionRules;
  }

  public void setRetentionRules(List<RetentionRule> retentionRules) {
    this.retentionRules = retentionRules;
  }

  @Override
  public int hashCode() {
    return Objects.hash(dataSubjectID, dataSubjectRole, legalGround, maxDeletionDate, retentionRules, startTime);
  }

  @Override
  public boolean equals(Object obj) {
    if (obj == null) {
      return false;
    }
    if (this == obj) {
      return true;
    }
    if (!(obj instanceof DataSubjectLegalGroundDeletionRequest)) {
      return false;
    }
    DataSubjectLegalGroundDeletionRequest other = (DataSubjectLegalGroundDeletionRequest) obj;
    return Objects.equals(dataSubjectID, other.dataSubjectID) && Objects.equals(dataSubjectRole, other.dataSubjectRole)
        && Objects.equals(legalGround, other.legalGround) && Objects.equals(maxDeletionDate, other.maxDeletionDate)
        && Objects.equals(retentionRules, other.retentionRules) && Objects.equals(startTime, other.startTime);
  }

  @Override
  public String toString() {
    return "LegalGroundDeletion [legalGround=" + legalGround + ", dataSubjectRole=" + dataSubjectRole
        + ", dataSubjectID=" + dataSubjectID + ", startTime=" + startTime + ", maxDeletionDate=" + maxDeletionDate
        + ", retentionRules=" + retentionRules + "]";
  }

}
