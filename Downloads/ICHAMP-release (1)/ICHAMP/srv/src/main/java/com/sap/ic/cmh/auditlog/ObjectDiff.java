package com.sap.ic.cmh.auditlog;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import lombok.ToString;

@ToString
@AllArgsConstructor
@NoArgsConstructor
public class ObjectDiff {
    private String attributeName; 

    private String oldValue; 

    private String newValue;

    public String getAttributeName() {
        return attributeName;
    }


    public void setAttributeName(String attributeName) {
        this.attributeName = attributeName;
    }

    public String getOldValue() {
        return oldValue;
    }

    public void setOldValue(String oldValue) {
        this.oldValue = oldValue;
    }


    public String getNewValue() {
        return newValue;
    }

    public void setNewValue(String newValue) {
        this.newValue = newValue;
    }
    
    
    
}
