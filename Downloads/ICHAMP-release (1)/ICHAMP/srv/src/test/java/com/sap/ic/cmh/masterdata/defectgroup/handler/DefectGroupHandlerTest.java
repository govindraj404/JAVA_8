package com.sap.ic.cmh.masterdata.defectgroup.handler;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.*;

import com.sap.cds.services.cds.CdsDeleteEventContext;
import com.sap.ic.cmh.utils.CqnAnalyzerUtil;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.cds.CdsUpdateEventContext;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.masterdata.defectcode.handler.DefectCodeHandlerTest;
import com.sap.ic.cmh.masterdata.defectgroup.service.DefectGroupService;
import com.sap.ic.cmh.masterdata.defectgroup.validation.DefectGroupValidator;
import cds.gen.masterdataservice.DefectGroups;

public class DefectGroupHandlerTest {
    @InjectMocks
    @Autowired
    private DefectGroupHandler handler;
    @Mock
    private DefectGroupValidator validator;
    @Mock
    Messages messages;
    @Mock
    CdsCreateEventContext createContextMock;
    @Mock
    private CdsUpdateEventContext updateContextMock;
    @Mock
    CqnInsert cqnInsert;
    @Mock
	CqnUpdate cqnUpdate;
    @Mock
    DefectGroupService defectGroupService;
    @Mock
    Result result;
    @Mock
    private CdsService cdsService;
    @Mock
    CdsDeleteEventContext deleteEventContext;
    @Mock
    CqnAnalyzerUtil cqnAnalyzerUtil;


    private DefectGroups group;


    public static final Logger logger = LoggerFactory.getLogger(DefectCodeHandlerTest.class);

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);

        group = Struct.create(DefectGroups.class);
        group.setCode("QM-E");
        group.setDescription("test");

        when(createContextMock.getCqn()).thenReturn(cqnInsert);
        List<Map<String, Object>> entries = new ArrayList<>();
        when(cqnInsert.entries()).thenReturn(entries);
    }

	@Test
	public void testCreateDefectGroup() {
		Optional<DefectGroups> emptyOpt = Optional.empty();
		when(defectGroupService.fetchDefectGroupCode(group.getCode())).thenReturn(group);
		handler.beforeDefectGroupCreate(createContextMock, group);
	}

	@Test
	public void testUpdateDefectGroup() {
		Optional<DefectGroups> emptyOpt = Optional.of(group);
		when(defectGroupService.fetchDefectGroupCode(group.getCode())).thenReturn(group);
		handler.beforeDefectGroupCreate(createContextMock, group);
	}
	
	 @Test
	 public void updateDefectGroupTest(){		
	  handler.updateDefectGroup(updateContextMock,group);			
	 }

	 @Test
     public void testOnDefectGroupCreate(){

        when(defectGroupService.fetchDefectGroupCode(group.getCode())).thenReturn(group);
        handler.onDefectGroupCreate(createContextMock,group);
     }

     @Test
    public void testBeforeDefectGroupDelete(){
         Map<String, Object> targetKeys = new HashMap<>();
         targetKeys.put("ID", "Defect");
        when(cqnAnalyzerUtil.provideTargetKeys(any())).thenReturn(targetKeys);
        handler.beforeDefectGroupDelete(deleteEventContext);
     }



}
