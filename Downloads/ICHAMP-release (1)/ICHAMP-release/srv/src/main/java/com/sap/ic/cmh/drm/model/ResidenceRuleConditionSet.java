package com.sap.ic.cmh.drm.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;


@AllArgsConstructor
@NoArgsConstructor
public class ResidenceRuleConditionSet {
	private String residenceDate;
	
	@JsonProperty("conditionSet")
    private List<RuleCondition> conditionSet;

    /**
     * @return the residenceDate
     */
    public String getResidenceDate() {
        return residenceDate;
    }

    /**
     * @param residenceDate the residenceDate to set
     */
    public void setResidenceDate(String residenceDate) {
        this.residenceDate = residenceDate;
    }

    /**
     * @return the conditionSet
     */
    public List<RuleCondition> getConditionSet() {
        return conditionSet;
    }

    /**
     * @param conditionSet the conditionSet to set
     */
    public void setConditionSet(List<RuleCondition> conditionSet) {
        this.conditionSet = conditionSet;
    }
    
    
}