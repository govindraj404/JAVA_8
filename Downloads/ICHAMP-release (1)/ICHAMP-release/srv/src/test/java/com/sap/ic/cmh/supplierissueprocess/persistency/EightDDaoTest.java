package com.sap.ic.cmh.supplierissueprocess.persistency;

import cds.gen.configurationservice.ClaimStatusMappings_;
import cds.gen.managesupplierissueprocessservice.Supplier8DProcesses;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class EightDDaoTest {

    @InjectMocks
    @Autowired
    EightDDao dao;
    @Mock
    Result result;
    @Mock
    CqnInsert cqnInsert;
    @Mock
    Row row;
    @Mock
    PersistenceService db;
    private Supplier8DProcesses qn;
    private List<Supplier8DProcesses> boList = new ArrayList<>();
    @Before
    public void beforeClass(){
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void testGetEightDBasedOnId(){
        dao.getEightDBasedOnId("456");
    }


    @Test
    public void testGetEightDDetailsBasedOnComplaintId(){
        dao.getEightDDetailsBasedOnComplaintId("complaintId");
    }

    @Test
    public void testGetActiveEightD() {
        Mockito.when(db.run(any(CqnSelect.class))).thenReturn(result);
        List<Supplier8DProcesses> supplier8DProcessesList = new ArrayList<>();
        Mockito.when(result.listOf(Supplier8DProcesses.class)).thenReturn(supplier8DProcessesList);
        assertEquals(supplier8DProcessesList, dao.getActiveEightD());
    }

    @Test
    public void testGetEightD(){
        Mockito.when(db.run(any(CqnSelect.class))).thenReturn(result);
        List<Supplier8DProcesses> supplier8DProcessesList = new ArrayList<>();
        Mockito.when(result.listOf(Supplier8DProcesses.class)).thenReturn(supplier8DProcessesList);
         dao.getEightD("hhh");}


}
