package com.sap.ic.cmh.drm.model;

import java.util.List;
import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class DataSubjectLastRetentionStartDatesRequest {

     private String legalGround;

  private String dataSubjectRole;

  private String dataSubjectID;

  private String legalEntity;

  private String startTime;

  private List<RulesCondition> rulesConditionSet;

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

  public String getLegalEntity() {
    return legalEntity;
  }

  public void setLegalEntity(String legalEntity) {
    this.legalEntity = legalEntity;
  }

  public String getStartTime() {
    return startTime;
  }

  public void setStartTime(String startTime) {
    this.startTime = startTime;
  }

  public List<RulesCondition> getRulesConditionSet() {
    return rulesConditionSet;
  }

  public void setRulesConditionSet(List<RulesCondition> rulesConditionSet) {
    this.rulesConditionSet = rulesConditionSet;
  }

  @Override
  public int hashCode() {
    return Objects.hash(dataSubjectID, dataSubjectRole, legalEntity, legalGround, rulesConditionSet, startTime);
  }

  @Override
  public boolean equals(Object obj) {
    if (obj == null) {
      return false;
    }
    if (this == obj) {
      return true;
    }
    if (!(obj instanceof DataSubjectLastRetentionStartDatesRequest)) {
      return false;
    }
    DataSubjectLastRetentionStartDatesRequest other = (DataSubjectLastRetentionStartDatesRequest) obj;
    return Objects.equals(dataSubjectID, other.dataSubjectID) && Objects.equals(dataSubjectRole, other.dataSubjectRole)
        && Objects.equals(legalEntity, other.legalEntity) && Objects.equals(legalGround, other.legalGround)
        && Objects.equals(rulesConditionSet, other.rulesConditionSet) && Objects.equals(startTime, other.startTime);
  }

  @Override
  public String toString() {
    return "LastRetentionStartDateRequest [legalGround=" + legalGround + ", dataSubjectRole=" + dataSubjectRole
        + ", dataSubjectID=" + dataSubjectID + ", legalEntity=" + legalEntity + ", startTime=" + startTime
        + ", rulesConditionSet=" + rulesConditionSet + "]";
  }
    
}
