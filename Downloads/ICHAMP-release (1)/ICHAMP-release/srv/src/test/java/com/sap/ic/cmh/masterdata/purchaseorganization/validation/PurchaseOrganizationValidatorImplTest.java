package com.sap.ic.cmh.masterdata.purchaseorganization.validation;

import cds.gen.masterdataservice.PurchaseOrganizations;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.utils.datavalidation.DataValidator;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.beans.factory.annotation.Autowired;

public class PurchaseOrganizationValidatorImplTest {

    @InjectMocks
    @Autowired
    PurchaseOrganizationValidatorImpl purchaseOrganizationValidatorImpl;

    @Spy
    private DataValidator dataValidator;

    @Mock
    private Messages messages;

    private PurchaseOrganizations purchaseOrganizations;

    @Before
    public void beforeClass() throws Exception {
        MockitoAnnotations.openMocks(this);
        purchaseOrganizations = PurchaseOrganizations.create();
        purchaseOrganizations.setId("100001");
        purchaseOrganizations.setPurchaseOrganization("Organization");
    }

    @Test
    public void testCheckInputsSanitized(){
        purchaseOrganizationValidatorImpl.checkInputsSanitized(purchaseOrganizations);
    }
}
