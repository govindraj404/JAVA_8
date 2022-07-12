package com.sap.ic.cmh.drm.model;

import java.util.List;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class DataSubjectsEndofResidenceRequest {
  private String legalGround;

  @JsonProperty("legalEntitiesResidenceRules")
  private List<LegalGroundResidenceRules> legalGroundResidenceRules;

/**
 * @return the legalGround
 */
public String getLegalGround() {
    return legalGround;
}

/**
 * @param legalGround the legalGround to set
 */
public void setLegalGround(String legalGround) {
    this.legalGround = legalGround;
}

/**
 * @return the legalGroundResidenceRules
 */
public List<LegalGroundResidenceRules> getLegalGroundResidenceRules() {
    return legalGroundResidenceRules;
}

/**
 * @param legalGroundResidenceRules the legalGroundResidenceRules to set
 */
public void setLegalGroundResidenceRules(
        List<LegalGroundResidenceRules> legalGroundResidenceRules) {
    this.legalGroundResidenceRules = legalGroundResidenceRules;
}

  

}
