package com.sap.ic.cmh.utils;

import com.sap.cds.ql.cqn.CqnAnalyzer;
import com.sap.cds.services.cds.CdsDeleteEventContext;
import com.sap.cds.services.draft.DraftCancelEventContext;

import org.springframework.stereotype.Component;

import java.util.Map;

@Component
public class CqnAnalyzerUtil {
    public Map<String, Object> provideTargetKeys(CdsDeleteEventContext context) {
        return CqnAnalyzer.create(context.getModel()).analyze(context.getCqn()).targetKeys();
    }
    
    public Map<String, Object> provideTargetKeysDraft(DraftCancelEventContext context) {
        return CqnAnalyzer.create(context.getModel()).analyze(context.getCqn()).targetKeys();
    }
}
