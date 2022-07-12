package com.sap.ic.cmh.drm.controller;

import java.text.ParseException;
import java.util.Arrays;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.sap.ic.cmh.drm.exceptions.InputValidationException;
import com.sap.ic.cmh.drm.exceptions.NoContentException;
import com.sap.ic.cmh.drm.model.DataSubject;
import com.sap.ic.cmh.drm.model.DataSubjectEndOfBusinessResponse;
import com.sap.ic.cmh.drm.model.DataSubjectLastRetentionStartDatesRequest;
import com.sap.ic.cmh.drm.model.DataSubjectLastRetentionStartDatesResponse;
import com.sap.ic.cmh.drm.model.DataSubjectLegalGroundDeletionRequest;
import com.sap.ic.cmh.drm.model.DataSubjectRequest;
import com.sap.ic.cmh.drm.model.DataSubjectsEndofResidenceRequest;
import com.sap.ic.cmh.drm.model.DataSubjectsEndofResidenceResponse;
import com.sap.ic.cmh.drm.model.ValueHelpResponse;
import com.sap.ic.cmh.drm.service.RetentionManagerService;
import com.sap.ic.cmh.utils.GenericUtils;


@RestController
@RequestMapping("/drm")
public class RetentionManagerController {
	
	private static final Logger logger = LoggerFactory.getLogger(RetentionManagerController.class);
	
	@Autowired
	private RetentionManagerService retentionManagerService;
	
	@PostMapping("/deleteDataSubject")
	public ResponseEntity<String> deleteDataSubject(@RequestBody DataSubjectRequest dataSubjectRequest){
		try {
			retentionManagerService.deleteDataSubject(dataSubjectRequest);
		} catch (ParseException e) {
			logger.error("Exception occurred while parsinng the retention date");
			return new ResponseEntity<>(HttpStatus.INTERNAL_SERVER_ERROR);
		}
		return new ResponseEntity<>(HttpStatus.OK);
	}
	
    @GetMapping("/legalEntities/{dataSubjectRole}")
    //dataSubjectRole - SUP,SUPCON, PERRES
	public ResponseEntity<List<ValueHelpResponse>> getLegalEntity(@PathVariable String dataSubjectRole) {
		List<ValueHelpResponse> valueHelpResponse;
		if(dataSubjectRole != null) {
			valueHelpResponse = retentionManagerService.getLegalEntities(dataSubjectRole);
			return new ResponseEntity<>(valueHelpResponse, HttpStatus.OK);
		}else {
			return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
		}
	}
	
    @PostMapping("/dataSubjectEndOfBusiness")
    //To check from application end
	public ResponseEntity<DataSubjectEndOfBusinessResponse> dataSubjectEndOfBusiness(
	      @RequestBody DataSubject dataSubjectEndOfBusinessRequest) throws InputValidationException, NoContentException {
		return ResponseEntity.status(HttpStatus.OK).body(new DataSubjectEndOfBusinessResponse(true, null));
	}
	
	@PostMapping("/dataSubjectLegalEntities")
	  public ResponseEntity<List<String>> dataSubjectLegalEntities(
	      @RequestBody DataSubject dataSubjectLegalEntitiesRequest) throws InputValidationException, NoContentException {
	    if(dataSubjectLegalEntitiesRequest.getDataSubjectID() != null) {
	    	return ResponseEntity.status(HttpStatus.OK).body(Arrays.asList(GenericUtils.sanitize(dataSubjectLegalEntitiesRequest.getDataSubjectID())));
	    }else {
	    	return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
	    }
	  }
	
	@PostMapping("/dataSubjectsEndofResidence")
	  public ResponseEntity<DataSubjectsEndofResidenceResponse> dataSubjectEndOfResidence(
	      @RequestBody DataSubjectsEndofResidenceRequest dataSubjectsEndofResidenceRequest)
	      throws InputValidationException {
		
		logger.info("Executing method : dataSubjectEndOfResidence()");
		
		DataSubjectsEndofResidenceResponse dataSubjectEndOfResidenceResponse = retentionManagerService.getDataSubjectEndOfResidence(dataSubjectsEndofResidenceRequest);
		
	    return new ResponseEntity<>(dataSubjectEndOfResidenceResponse, HttpStatus.OK);
	  }
	
	
	  @PostMapping("/deleteLegalGroundInstances")
	  public ResponseEntity<String> deleteLegalGroundInstances(
	      @RequestBody DataSubjectLegalGroundDeletionRequest dataSubjectLegalGroundDeletionRequest)
	      throws InputValidationException, NoContentException {
		  return new ResponseEntity<>(HttpStatus.OK);
	  }
	  
	  @PostMapping("/dataSubjectRetentionStartDate")
	  public ResponseEntity<List<DataSubjectLastRetentionStartDatesResponse>> dataSubjectLastRetentionStartDates(
	      @RequestBody DataSubjectLastRetentionStartDatesRequest dataSubjectLastRetentionStartDatesRequest){
		  
		  logger.info("Executing dataSubjectLastRetentionStartDates() ");
		  List<DataSubjectLastRetentionStartDatesResponse> dataSubjectLastRetentionStartDatesResponseList = retentionManagerService.getDataSubjectLastRetentionStartDates(dataSubjectLastRetentionStartDatesRequest);
		  if(!dataSubjectLastRetentionStartDatesResponseList.isEmpty()) {
			  return new ResponseEntity<>(dataSubjectLastRetentionStartDatesResponseList,HttpStatus.OK);
		  }else {
			  return new ResponseEntity<>(null, HttpStatus.INTERNAL_SERVER_ERROR);
		  }
		  
		  
	  }
	
}
