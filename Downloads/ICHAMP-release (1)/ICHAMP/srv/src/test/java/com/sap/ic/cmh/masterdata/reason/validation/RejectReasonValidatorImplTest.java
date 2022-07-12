package com.sap.ic.cmh.masterdata.reason.validation;

import cds.gen.masterdataservice.Reasons;
import com.sap.cds.Struct;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.utils.datavalidation.DataValidator;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.beans.factory.annotation.Autowired;

public class RejectReasonValidatorImplTest {

    @InjectMocks
    @Autowired
    RejectReasonValidatorImpl rejectReasonValidatorImpl;

    @Spy
    private DataValidator dataValidator;

    @Mock
    private Messages messages;

    private Reasons reasons;

    @Before
    public void beforeClass() throws Exception {
        MockitoAnnotations.openMocks(this);

        reasons = Struct.create(Reasons.class);
        reasons.setCode("code");
        reasons.setDescription("description");

    }

    @Test
    public void testCheckInputsSanitized(){
        rejectReasonValidatorImpl.checkInputsSanitized(reasons);
    }
}
