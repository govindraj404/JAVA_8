package com.sap.ic.cmh.masterdata.businesspartner.validations;

import cds.gen.masterdataservice.BusinessPartners;
import com.sap.cds.Struct;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.utils.datavalidation.DataValidator;
import org.junit.Before;
import org.junit.Test;
import org.mockito.*;
import org.springframework.beans.factory.annotation.Autowired;


public class BusinessPartnerValidatorImplTest {
    @InjectMocks
    @Autowired
    private BusinessPartnerValidatorImpl businessPartnerValidator;

    @Spy
    private DataValidator dataValidator;

    @Autowired
    private Messages messages;

    @Mock
    private BusinessPartners businessPartner;

    @Before
    public void beforeClass() throws Exception {
        MockitoAnnotations.openMocks(this);

        businessPartner = Struct.create(BusinessPartners.class);
        businessPartner.setBusinessPartnerNumber("10000001");
        businessPartner.setVendorCode("1100000001");
        businessPartner.setCustomerCode("2100000001");
        businessPartner.setCompanyCode("BP01");
        businessPartner.setBusinessPartnerName1("Auto-Werke GmbH");
        businessPartner.setBusinessPartnerName2("Robert Bosch GmbH");
        businessPartner.setAddress("10000001");
        businessPartner.setBusinessPartnerType("VC");
    }

    @Test
    public void testCheckInputsSanitized_ValidationPass() {
        businessPartnerValidator.checkInputsSanitized(businessPartner);
    }
}
