package com.sap.ic.cmh.drm.model;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
public class RuleCondition {
    private String conditionFieldValue;

    /**
     * @return the conditionFieldValue
     */
    public String getConditionFieldValue() {
        return conditionFieldValue;
    }

    /**
     * @param conditionFieldValue the conditionFieldValue to set
     */
    public void setConditionFieldValue(String conditionFieldValue) {
        this.conditionFieldValue = conditionFieldValue;
    }
    
    
}