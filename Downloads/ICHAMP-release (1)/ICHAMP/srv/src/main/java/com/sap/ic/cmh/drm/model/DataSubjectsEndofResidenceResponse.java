package com.sap.ic.cmh.drm.model;

import java.util.Set;

import com.fasterxml.jackson.annotation.JsonProperty;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;


@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class DataSubjectsEndofResidenceResponse {
	
	@JsonProperty("success")
	private Set<DataSubjectResponse> expiredDataSubjects;

	@JsonProperty("nonConfirmCondition")
    private Set<DataSubjectResponse> dataSubjectBadRequests;

    /**
     * @return the expiredDataSubjects
     */
    public Set<DataSubjectResponse> getExpiredDataSubjects() {
        return expiredDataSubjects;
    }

    /**
     * @param expiredDataSubjects the expiredDataSubjects to set
     */
    public void setExpiredDataSubjects(Set<DataSubjectResponse> expiredDataSubjects) {
        this.expiredDataSubjects = expiredDataSubjects;
    }

    /**
     * @return the dataSubjectBadRequests
     */
    public Set<DataSubjectResponse> getDataSubjectBadRequests() {
        return dataSubjectBadRequests;
    }

    /**
     * @param dataSubjectBadRequests the dataSubjectBadRequests to set
     */
    public void setDataSubjectBadRequests(Set<DataSubjectResponse> dataSubjectBadRequests) {
        this.dataSubjectBadRequests = dataSubjectBadRequests;
    }
    
    
    


	
}
