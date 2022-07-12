package com.sap.ic.cmh.drm.model;

public class RetentionRule {

  private String legalEntity;

  private Integer retentionPeriod;

  private String retentionUnit;

  public String getLegalEntity() {
    return legalEntity;
  }

  public void setLegalEntity(String legalEntity) {
    this.legalEntity = legalEntity;
  }

  public Integer getRetentionPeriod() {
    return retentionPeriod;
  }

  public void setRetentionPeriod(Integer retentionPeriod) {
    this.retentionPeriod = retentionPeriod;
  }

  public String getRetentionUnit() {
    return retentionUnit;
  }

  public void setRetentionUnit(String retentionUnit) {
    this.retentionUnit = retentionUnit;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((legalEntity == null) ? 0 : legalEntity.hashCode());
    result = prime * result + ((retentionPeriod == null) ? 0 : retentionPeriod.hashCode());
    result = prime * result + ((retentionUnit == null) ? 0 : retentionUnit.hashCode());
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
    RetentionRule other = (RetentionRule) obj;
    if (legalEntity == null) {
      if (other.legalEntity != null) {
        return false;
      }
    } else if (!legalEntity.equals(other.legalEntity)) {
      return false;
    }
    if (retentionPeriod == null) {
      if (other.retentionPeriod != null) {
        return false;
      }
    } else if (!retentionPeriod.equals(other.retentionPeriod)) {
      return false;
    }
    if (retentionUnit == null) {
      if (other.retentionUnit != null) {
        return false;
      }
    } else if (!retentionUnit.equals(other.retentionUnit)) {
      return false;
    }
    return true;
  }

  @Override
  public String toString() {
    return "RetentionRule [legalEntity=" + legalEntity + ", retentionPeriod=" + retentionPeriod + ", retentionUnit="
        + retentionUnit + "]";
  }

}
