package com.sap.ic.cmh.drm.controller;

import com.sap.ic.cmh.costcollector.handler.AddCostContextFR;
import com.sap.ic.cmh.costcollector.handler.AddCostFRHandler;
import com.sap.ic.cmh.drm.exceptions.InputValidationException;
import com.sap.ic.cmh.drm.exceptions.NoContentException;
import com.sap.ic.cmh.drm.model.*;
import com.sap.ic.cmh.drm.service.RetentionManagerService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.when;

public class RetentionManagerControllerTest {
    @InjectMocks
    @Autowired
    RetentionManagerController controller;

    @Mock
    private RetentionManagerService retentionManagerService;
    @Mock
    DataSubjectRequest dataSubjectRequest;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void deleteDataSubjectTest(){
        controller.deleteDataSubject(dataSubjectRequest);
    }

    @Test
    public void deleteDataSubjectExpTest() throws ParseException {
        dataSubjectRequest=new DataSubjectRequest();
        dataSubjectRequest.setDataSubjectID(")(%#$%#$");
        dataSubjectRequest.setMaxDeletionDate("3#@#$@$@#");
        doThrow(new ParseException("test",0)).when(retentionManagerService).deleteDataSubject(dataSubjectRequest);
        controller.deleteDataSubject(dataSubjectRequest);
    }

    @Test
    public void getLegalEntityTest()  {
             controller.getLegalEntity("dataSubjectRequest");
    }

    @Test
    public void getLegalEntityNullTest()  {
        controller.getLegalEntity(null);
    }
    @Test
    public void dataSubjectEndOfBusinessTest() throws InputValidationException, NoContentException {
        DataSubject dataSubject=new DataSubject();
        controller.dataSubjectEndOfBusiness(dataSubject);
    }


    @Test
    public void dataSubjectLegalEntitiesTest() throws InputValidationException, NoContentException {
        DataSubject dataSubject=new DataSubject();
        dataSubject.setDataSubjectID("34534");
        controller.dataSubjectLegalEntities(dataSubject);
    }

    @Test
    public void dataSubjectLegalEntitiesNullTest() throws InputValidationException, NoContentException {
        DataSubject dataSubject=new DataSubject();
        controller.dataSubjectLegalEntities(dataSubject);
    }

    @Test
    public void deleteLegalGroundInstancesTest() throws InputValidationException, NoContentException {
        DataSubjectLegalGroundDeletionRequest dataSubject=new DataSubjectLegalGroundDeletionRequest();
        // dataSubject.setDataSubjectID("34534");
        controller.deleteLegalGroundInstances(dataSubject);
    }

    @Test
    public void dataSubjectLastRetentionStartDatesTest() throws InputValidationException, NoContentException {
        DataSubjectLastRetentionStartDatesRequest dataSubject=new DataSubjectLastRetentionStartDatesRequest();
            List<DataSubjectLastRetentionStartDatesResponse> list=new ArrayList<>();
        when(retentionManagerService.getDataSubjectLastRetentionStartDates(any())).thenReturn(list);
        controller.dataSubjectLastRetentionStartDates(dataSubject);
    }

    @Test
    public void dataSubjectLastRetentionStartDatesELSeTest() throws InputValidationException, NoContentException {
        DataSubjectLastRetentionStartDatesRequest dataSubject=new DataSubjectLastRetentionStartDatesRequest();
        dataSubject.setDataSubjectID("34534");
        DataSubjectLastRetentionStartDatesResponse response=new DataSubjectLastRetentionStartDatesResponse("","");
        response.setRetentionID("234u2309u423");
        List<DataSubjectLastRetentionStartDatesResponse> list=new ArrayList<>();
        list.add(response);
        when(retentionManagerService.getDataSubjectLastRetentionStartDates(any())).thenReturn(list);
        controller.dataSubjectLastRetentionStartDates(dataSubject);
    }


    @Test
    public void dataSubjectEndOfResidenceTest() throws InputValidationException, NoContentException {
        DataSubjectsEndofResidenceRequest dataSubject=new DataSubjectsEndofResidenceRequest();
        dataSubject.setLegalGround("34534");
                controller.dataSubjectEndOfResidence(dataSubject);
    }

}
