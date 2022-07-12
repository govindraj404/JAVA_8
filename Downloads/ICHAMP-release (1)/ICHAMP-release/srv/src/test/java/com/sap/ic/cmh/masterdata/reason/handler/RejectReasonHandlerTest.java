package com.sap.ic.cmh.masterdata.reason.handler;

import cds.gen.masterdataservice.Reasons;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.cds.CdsUpdateEventContext;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.masterdata.reason.service.RejectReasonService;
import com.sap.ic.cmh.masterdata.reason.validation.RejectReasonValidator;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.mockito.Mockito.when;


public class RejectReasonHandlerTest {

    @InjectMocks
    @Autowired
    RejectReasonHandler rejectReasonHandler;
    @Mock
    RejectReasonService rejectReasonService;

    @Mock
    private RejectReasonValidator rejectReasonValidator;

    @Mock
    CdsCreateEventContext createContextMock;

    @Mock
    CdsUpdateEventContext updateContextMock;

    private Reasons reasons;
    @Mock
    CqnInsert cqnInsert;
    @Mock
    Messages messages;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        reasons = Struct.create(Reasons.class);
        reasons.setCode("code");
        reasons.setDescription("Description");
        when(createContextMock.getCqn()).thenReturn(cqnInsert);
        List<Map<String, Object>> entries = new ArrayList<>();
        when(cqnInsert.entries()).thenReturn(entries);
    }
    @Test
    public void testSubItemTypeNull () {
        when(rejectReasonService.fetchReasonBasedOnCode(reasons.getCode())).thenReturn(null);
        rejectReasonHandler.onRejectReasonOnCreate(createContextMock,reasons);
    }


    @Test
    public void testCreateRejectReasons()  {
        rejectReasonHandler.validateRejectReasonOnCreate(createContextMock, reasons);
    }

    @Test
    public void testUpdateRejectReasons()  {
        rejectReasonHandler.validateRejectReasonOnUpdate(updateContextMock, reasons);
    }
}
