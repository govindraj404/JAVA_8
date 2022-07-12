package com.sap.ic.cmh.drm.model;

public class RulesCondition {

  private String retentionID;

  public String getRetentionID() {
    return retentionID;
  }

  public void setRetentionID(String retentionID) {
    this.retentionID = retentionID;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((retentionID == null) ? 0 : retentionID.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    RulesCondition other = (RulesCondition) obj;
    if (retentionID == null) {
      if (other.retentionID != null) {
        return false;
      }
    } else if (!retentionID.equals(other.retentionID)) {
      return false;
    }
    return true;
  }

  @Override
  public String toString() {
    return "RulesCondition [retentionID=" + retentionID + "]";
  }

}
