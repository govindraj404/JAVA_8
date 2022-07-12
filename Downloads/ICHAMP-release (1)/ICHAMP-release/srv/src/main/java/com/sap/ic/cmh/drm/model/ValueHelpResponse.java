package com.sap.ic.cmh.drm.model;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ValueHelpResponse {
	
	private String value;
    private String valueDesc;
    /**
     * @return the value
     */
    public String getValue() {
        return value;
    }
    /**
     * @param value the value to set
     */
    public void setValue(String value) {
        this.value = value;
    }
    /**
     * @return the valueDesc
     */
    public String getValueDesc() {
        return valueDesc;
    }
    /**
     * @param valueDesc the valueDesc to set
     */
    public void setValueDesc(String valueDesc) {
        this.valueDesc = valueDesc;
    }
    
    
}
