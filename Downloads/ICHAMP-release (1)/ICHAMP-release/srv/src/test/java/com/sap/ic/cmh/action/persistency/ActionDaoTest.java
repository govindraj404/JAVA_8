package com.sap.ic.cmh.action.persistency;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class ActionDaoTest {

     @InjectMocks
    public ActionDao dao;
    @Mock
    public PersistenceService db;
    @Mock
    Result result;
       @Mock
    CqnSelect cqnSelect;
    private Row row;
    private Optional<Row> opt;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        row = Struct.create(Row.class);

    }
    
    @Test
    public void testGetAction(){
         when(db.run(any(CqnSelect.class))).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("code", "CRTQN");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        dao.getActions("QN");
    }

    @Test
    public void testGetActionResultNotNull(){
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        dao.getActions("QN");
    }
    
    @Test
    public void testActionPreCondition(){
     when(db.run(any(CqnSelect.class))).thenReturn(result);
     dao.getActionPreconditions("CRTQN");
    }
    
}
