package com.sap.ic.cmh.drm.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class DataSubjectRequest {
	
	private String dataSubjectID;
    private String maxDeletionDate;
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
    /**
     * @return the maxDeletionDate
     */
    public String getMaxDeletionDate() {
        return maxDeletionDate;
    }
    /**
     * @param maxDeletionDate the maxDeletionDate to set
     */
    public void setMaxDeletionDate(String maxDeletionDate) {
        this.maxDeletionDate = maxDeletionDate;
    }
    
    
}
