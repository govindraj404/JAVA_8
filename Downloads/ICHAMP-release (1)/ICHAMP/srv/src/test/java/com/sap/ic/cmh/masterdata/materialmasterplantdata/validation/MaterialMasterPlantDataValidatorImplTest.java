package com.sap.ic.cmh.masterdata.materialmasterplantdata.validation;

import cds.gen.masterdataservice.MaterialMasterPlantDatas;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.masterdata.common.persistency.MasterDataDao;
import com.sap.ic.cmh.utils.datavalidation.DataValidatorImpl;
import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;

import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class MaterialMasterPlantDataValidatorImplTest {
    @Mock
    private DataValidatorImpl dataValidator;
    @Mock
    private MasterDataDao dao;
    @Mock
    private Messages messages;
    @Mock
    Message msg;
    @InjectMocks
    private MaterialMasterPlantDataValidatorImpl validator;
    @Mock
    private PersistenceService mockDb;
    @Mock
    Result result;
    private MaterialMasterPlantDatas groups;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);

        groups = Struct.create(MaterialMasterPlantDatas.class);
        groups.setMaterialCode("1");
        groups.setId("1");
        groups.setPlant("test");
        groups.setMaterialCodeIDId("1");
        groups.setMrpType("test");
        groups.setPlantIDId("1");
        groups.setContainerRequirements("EN");
        groups.setPricingIndicator("test");

    }
    @Test
    public void checkInputsSanitized_ValidationPass() {
        validator.checkInputsSanitized(groups);
    }
    @Test
    public void checkInputsSanitized_Validation() {
        Assertions.assertDoesNotThrow(() -> validator.checkInputsSanitized(groups));
    }

    @Test
    public void checkInputsSanitized_ValidationFailedForAlphaNumericData() {
        groups.setMrpType("<html><head></head><body></body></html>");
        validator.checkInputsSanitized(groups);
    }

    @Test
    public void checkInputsSanitized_ValidationateData() {
        groups.setPlant("Henry-Strasse");
        validator.checkInputsSanitized(groups);
    }


    @Test
    public void checkInputsSanitized_ValidationFailedForAlphaNumericDataTest() {
        groups.setContainerRequirements("<html><head></head><body></body></html>");
        validator.checkInputsSanitized(groups);
    }

    @Test
    public void checkInputsSanitized_ValidationateDataTest() {
        groups.setMaterialCode("Henry-Strasse");
        validator.checkInputsSanitized(groups);
    }


    @Test
    public void testCheckInputsSanitized_ValidationFailedForAlphaNumericData() {
        groups.setMaterialCode("<html><head></head><body></body></html>");
        validator.checkInputsSanitized(groups);
    }

    @Test
    public void testCheckInputsSanitized_ValidationateDataTest() {
        groups.setContainerRequirements("Henry-Strasse");
        validator.checkInputsSanitized(groups);
    }

}
