package com.sap.ic.cmh.utils;

import com.sap.cds.ql.cqn.AnalysisResult;
import com.sap.cds.ql.cqn.CqnAnalyzer;
import com.sap.cds.ql.cqn.CqnDelete;
import com.sap.cds.ql.cqn.CqnStructuredTypeRef;
import com.sap.cds.reflect.CdsModel;
import com.sap.cds.services.cds.CdsDeleteEventContext;
import com.sap.cds.services.draft.DraftCancelEventContext;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class CqnAnalyzerUtilTest {
    @InjectMocks
    CqnAnalyzerUtil util;
    @Mock
    CdsDeleteEventContext  context;
    @Mock
    CqnDelete cqnDelete;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);}

    @Mock
    CqnAnalyzer cqnAnalyzer;
    @Mock
    AnalysisResult analysisResult;
    @Mock
    CdsModel cdsModel;
    @Mock
    CqnStructuredTypeRef cqnStructuredTypeRef;
    @Mock
	DraftCancelEventContext draftCancelEventContext;
    
    @Test(expected = Exception.class)
    public void provideTargetKeys(){
        Map<String,Object> map=new HashMap<>();
        map.put("test","test");
        when(context.getCqn()).thenReturn(cqnDelete);
        when(cqnDelete.ref()).thenReturn(cqnStructuredTypeRef);
        when(cqnAnalyzer.analyze(any(CqnDelete.class))).thenReturn(analysisResult);
        when(analysisResult.targetKeys()).thenReturn(map);
        util.provideTargetKeys(context);
    }
    
    @Test(expected = Exception.class)
    public void provideTargetKeysdraftCancelEventContext(){
        Map<String,Object> map=new HashMap<>();
        map.put("test","test");
        when(draftCancelEventContext.getCqn()).thenReturn(cqnDelete);
        when(cqnDelete.ref()).thenReturn(cqnStructuredTypeRef);
        when(cqnAnalyzer.analyze(any(CqnDelete.class))).thenReturn(analysisResult);
        when(analysisResult.targetKeys()).thenReturn(map);
        util.provideTargetKeysDraft(draftCancelEventContext);
    }
}
