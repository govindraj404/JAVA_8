// package com.sap.ic.cmh.eightD.persistency;

// import org.mockito.InjectMocks;
// import org.mockito.Mock;
// import org.mockito.MockitoAnnotations;
// import static org.mockito.ArgumentMatchers.any;
// import java.util.ArrayList;
// import java.util.List;
// import java.util.Optional;
// import org.junit.Before;
// import org.junit.Test;
// import org.mockito.Mockito;
// import org.springframework.beans.factory.annotation.Autowired;
// import com.sap.cds.Result;
// import com.sap.cds.Row;
// import com.sap.cds.Struct;
// import com.sap.cds.ql.cqn.CqnSelect;
// import com.sap.cds.services.persistence.PersistenceService;
// import cds.gen.supplierissueprocessservice.Supplier8DProcesses;
// public class EightDDaoTest {
// @InjectMocks
// @Autowired
// EightDDao eightDDao;
// @Mock
// protected PersistenceService mockDb;
// @Mock
// Row row;
// @Mock
// Result result;
// private Supplier8DProcesses eightD;
// private List<Supplier8DProcesses> boList = new ArrayList<>();
// @Before
// public void beforeClass() {
// MockitoAnnotations.openMocks(this);
// eightD = Struct.create(Supplier8DProcesses.class);
// eightD.setId("EigthDID");
// eightD.setStatusCode("INTL");
// eightD.setComplaintId("1234");
// boList.add(eightD);
// }

// @Test
// public void testGetEightDBasedOnId(){
// Mockito.when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
// Mockito.when(result.listOf(Supplier8DProcesses.class)).thenReturn(boList);
// List<Row> rowvalues = new ArrayList<>();
// Optional<Row> opt = Optional.of(row);
// row.put("ID", "EigthDID");
// rowvalues.add(row);
// Mockito.when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
// Mockito.when(result.first()).thenReturn(opt);
// eightDDao.getEightDBasedOnId(eightD.getId());
// }
// }
