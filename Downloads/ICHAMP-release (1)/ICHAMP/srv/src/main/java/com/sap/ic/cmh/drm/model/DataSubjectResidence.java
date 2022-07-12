package com.sap.ic.cmh.drm.model;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;


@AllArgsConstructor
@NoArgsConstructor
@EqualsAndHashCode
public class DataSubjectResidence {
	private String dataSubjectId;
    private String dataSubjectReferenceDate;
    /**
     * @return the dataSubjectId
     */
    public String getDataSubjectId() {
        return dataSubjectId;
    }
    /**
     * @param dataSubjectId the dataSubjectId to set
     */
    public void setDataSubjectId(String dataSubjectId) {
        this.dataSubjectId = dataSubjectId;
    }
    /**
     * @return the dataSubjectReferenceDate
     */
    public String getDataSubjectReferenceDate() {
        return dataSubjectReferenceDate;
    }
    /**
     * @param dataSubjectReferenceDate the dataSubjectReferenceDate to set
     */
    public void setDataSubjectReferenceDate(String dataSubjectReferenceDate) {
        this.dataSubjectReferenceDate = dataSubjectReferenceDate;
    }
    

    
}
