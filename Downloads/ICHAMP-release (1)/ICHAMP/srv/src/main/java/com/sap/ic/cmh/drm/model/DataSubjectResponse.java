package com.sap.ic.cmh.drm.model;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;



@AllArgsConstructor
@NoArgsConstructor
@EqualsAndHashCode
public class DataSubjectResponse {
    private String dataSubjectID;

    /**
     * @return the dataSubjectID
     */
    public String getDataSubjectID() {
        return dataSubjectID;
    }

    /**
     * @param dataSubjectID the dataSubjectID to set
     */
    public void setDataSubjectID(String dataSubjectID) {
        this.dataSubjectID = dataSubjectID;
    }
  
    

}
