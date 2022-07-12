package com.sap.ic.cmh.drm.model;

public class LegalEntity {

  private String legalEntityCode;

  public LegalEntity(String legalEntity) {
    super();
    this.legalEntityCode = legalEntity;
  }

  public LegalEntity() {
    super();
  }

  public String getLegalEntity() {
    return legalEntityCode;
  }

  public void setLegalEntity(String legalEntity) {
    this.legalEntityCode = legalEntity;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((legalEntityCode == null) ? 0 : legalEntityCode.hashCode());
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
    LegalEntity other = (LegalEntity) obj;
    if (legalEntityCode == null) {
      if (other.legalEntityCode != null) {
        return false;
      }
    } else if (!legalEntityCode.equals(other.legalEntityCode)) {
      return false;
    }
    return true;
  }

  @Override
  public String toString() {
    return "LegalEntity [legalEntity=" + legalEntityCode + "]";
  }

}
