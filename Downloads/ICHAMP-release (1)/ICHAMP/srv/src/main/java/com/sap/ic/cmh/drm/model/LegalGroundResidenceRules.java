package com.sap.ic.cmh.drm.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;


@AllArgsConstructor
@NoArgsConstructor
public class LegalGroundResidenceRules {
	private String legalEntity;
	
	@JsonProperty("residenceRules")
    private List<ResidenceRuleConditionSet> ruleConditionSet;

    /**
     * @return the legalEntity
     */
    public String getLegalEntity() {
        return legalEntity;
    }

    /**
     * @param legalEntity the legalEntity to set
     */
    public void setLegalEntity(String legalEntity) {
        this.legalEntity = legalEntity;
    }

    /**
     * @return the ruleConditionSet
     */
    public List<ResidenceRuleConditionSet> getRuleConditionSet() {
        return ruleConditionSet;
    }

    /**
     * @param ruleConditionSet the ruleConditionSet to set
     */
    public void setRuleConditionSet(List<ResidenceRuleConditionSet> ruleConditionSet) {
        this.ruleConditionSet = ruleConditionSet;
    }
    
    
}