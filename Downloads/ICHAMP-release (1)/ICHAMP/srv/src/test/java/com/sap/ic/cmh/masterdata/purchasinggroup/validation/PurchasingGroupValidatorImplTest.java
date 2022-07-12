package com.sap.ic.cmh.masterdata.purchasinggroup.validation;

import cds.gen.masterdataservice.PurchasingGroups;
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

public class PurchasingGroupValidatorImplTest {

    @InjectMocks
    @Autowired
    PurchasingGroupValidatorImpl purchasingGroupValidatorImpl;

    @Spy
    private DataValidator dataValidator;

    @Mock
    private Messages messages;

    private PurchasingGroups purchasingGroups;

    @Before
    public void beforeClass() throws Exception {
        MockitoAnnotations.openMocks(this);

        purchasingGroups = Struct.create(PurchasingGroups.class);
        purchasingGroups.setCode("code");
        purchasingGroups.setDescription("description");

    }

    @Test
    public void testCheckInputsSanitized(){
        purchasingGroupValidatorImpl.checkInputsSanitized(purchasingGroups);
    }
}
