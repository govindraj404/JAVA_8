package com.sap.ic.cmh.masterdata.materialmastergeneraldata.validation;

import cds.gen.masterdataservice.MaterialMasterGeneralDatas;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
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
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import static org.mockito.ArgumentMatchers.any;

public class MaterialMasterGeneralDataValidatorImplTest {
    @Mock
    private DataValidatorImpl dataValidator;
    @Mock
    private MasterDataDao dao;
    @Mock
    private Messages messages;
    @Mock
    Message msg;
    @InjectMocks
    private MaterialMasterGeneralDataValidatorImpl validator;
    @Mock
    private PersistenceService mockDb;
    @Mock
    Result result;
    private MaterialMasterGeneralDatas groups;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);

        groups = Struct.create(MaterialMasterGeneralDatas.class);
        groups.setMaterialCode("1");
        groups.setId("1");
        groups.setMaterialGroup("test");
        groups.setMaterialDescription("test");
        groups.setDivision("test");
        groups.setEANCategary("test");
        groups.setMaterialType("EN");
        groups.setBaseUnitOfMeasureCode("test");
        groups.setEanUpc("test");
        groups.setSizeDimensions("test");
        groups.setProductCompositionIndicator("test");
        groups.setBaseUnitOfMeasureCode("test");
        groups.setDivision("test");

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
        groups.setMaterialDescription("<html><head></head><body></body></html>");
        validator.checkInputsSanitized(groups);
    }

    @Test
    public void checkInputsSanitized_ValidationateData() {
        groups.setMaterialType("Henry-Strasse");
        validator.checkInputsSanitized(groups);
    }


    @Test
    public void checkInputsSanitized_ValidationFailedForAlphaNumericDataTest() {
        groups.setMaterialGroup("<html><head></head><body></body></html>");
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
        groups.setBaseUnitOfMeasureCode("Henry-Strasse");
        validator.checkInputsSanitized(groups);
    }

    @Test
    public void checkInputsSanitized_ValidationateData1Test() {
        groups.setEANCategary("Henry-Strasse");
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.checkInputsSanitized(groups);
    }

}
