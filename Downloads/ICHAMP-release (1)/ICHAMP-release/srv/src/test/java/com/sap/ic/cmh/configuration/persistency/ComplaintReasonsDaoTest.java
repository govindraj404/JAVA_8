package com.sap.ic.cmh.configuration.persistency;

import cds.gen.configurationservice.ComplaintReasons;

import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class ComplaintReasonsDaoTest {
    @InjectMocks
    ComplaintReasonsDao complaintReasonsDao;
    @Mock
    PersistenceService db;
    @Mock
    Result result;
    @Mock
    Runnable run;
    ComplaintReasons complaintReasons;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        complaintReasons = Struct.create(ComplaintReasons.class);
        complaintReasons.setCode("reasoncode");
        complaintReasons.setIdentifier(101);
    }
    @Test
    public void getAllComplaintReasonsCodeTest(){
        Optional<ComplaintReasons> opt = Optional.of(complaintReasons);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        complaintReasonsDao.getComplaintReasonCodeAndIdByCode(complaintReasons.getCode());
    }

    @Test
    public void getAllComplaintReasonsTest(){
        Optional<ComplaintReasons> opt = Optional.of(complaintReasons);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        complaintReasonsDao.getAllComplaintReasonsOrderByIdentifier();
    }

    @Test
    public void getReasonCategoryTest(){
        Optional<ComplaintReasons> opt = Optional.of(complaintReasons);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        complaintReasonsDao.getComplaintReasonBasedOnID(
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
    }
    @Test
    public void getComplaintReasonDetailsBasedOnIDTest()
    {
        Optional<ComplaintReasons> opt = Optional.of(complaintReasons);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        complaintReasonsDao.getComplaintReasonDetailsBasedOnID("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
    }
}