package com.sap.ic.cmh.masterdata.defectgroup.validation;

import cds.gen.masterdataservice.DefectGroups;
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
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class DefectGroupValidatorImplTest {
    @Mock
    private DataValidatorImpl dataValidator;
    @Mock
    private MasterDataDao dao;
    @Mock
    private Messages messages;
    @Mock
    Message msg;
    @InjectMocks
    private DefectGroupValidatorImpl validator;
    @Mock
    private PersistenceService mockDb;

    @Mock
    Result result;

    private DefectGroups groups;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);

        groups = Struct.create(DefectGroups.class);
        groups.setCode("1");
        groups.setDescription("test");

        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);


    }
    @Test
    public void checkInputsSanitized_ValidationPass() {
        Object [] obj={"test"};
        when(dao.getDefectGroup("1")).thenReturn(result);
        when(messages.error("test",obj)).thenReturn(msg);
        when(msg.target("test")).thenReturn(msg);
        validator.checkInputsSanitized(groups);
    }

    @Test
    public void checkInputsSanitized_ValidationFailedForAlphaNumericData() {
        groups.setDescription("<html><head></head><body></body></html>");
        validator.checkInputsSanitized(groups);
    }

    @Test
    public void checkInputsSanitized_ValidationateData() {
        groups.setCode("Henry-Strasse");
        validator.checkInputsSanitized(groups);
    }



}
