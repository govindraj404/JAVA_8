package com.sap.ic.cmh.drm.service;

import java.text.ParseException;
import java.util.List;

import com.sap.ic.cmh.drm.model.DataSubjectLastRetentionStartDatesRequest;
import com.sap.ic.cmh.drm.model.DataSubjectLastRetentionStartDatesResponse;
import com.sap.ic.cmh.drm.model.DataSubjectRequest;
import com.sap.ic.cmh.drm.model.DataSubjectsEndofResidenceRequest;
import com.sap.ic.cmh.drm.model.DataSubjectsEndofResidenceResponse;
import com.sap.ic.cmh.drm.model.ValueHelpResponse;

public interface RetentionManagerService {

	public List<ValueHelpResponse> getLegalEntities(String dataSubjectRole);

	public DataSubjectsEndofResidenceResponse getDataSubjectEndOfResidence(
			DataSubjectsEndofResidenceRequest dataSubjectsEndofResidenceRequest);

	public List<DataSubjectLastRetentionStartDatesResponse> getDataSubjectLastRetentionStartDates(
			DataSubjectLastRetentionStartDatesRequest dataSubjectLastRetentionStartDatesRequest);

	public void deleteDataSubject(DataSubjectRequest dataSubjectRequest) throws ParseException;
}
