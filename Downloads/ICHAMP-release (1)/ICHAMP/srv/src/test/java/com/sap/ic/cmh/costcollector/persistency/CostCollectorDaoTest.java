package com.sap.ic.cmh.costcollector.persistency;

import cds.gen.costcollectorservice.CostCollectors;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnDelete;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.costcollector.persistance.CostCollectorDao;
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
import static org.junit.Assert.assertNull;
import static org.mockito.ArgumentMatchers.any;

public class CostCollectorDaoTest {
	
	@InjectMocks @Autowired
	CostCollectorDao costColDao;
	
	@Mock
	PersistenceService db;	
	@Mock
	Result result;
	@Mock
    Row row;

	private CostCollectors costCollector;
	private List<CostCollectors> listofCostCol = new ArrayList<CostCollectors>(); 
	private Optional<Row> opt;
	private List<Row> list;
	
	@Before
	public void beforeClass() {
		
		MockitoAnnotations.openMocks(this);
		
		costCollector = Struct.create(CostCollectors.class);
		costCollector.setClaim("C123");
		costCollector.setItemTypeCode("Electronic");	
		
		listofCostCol.add(costCollector);	
		row.put("ID", "I123");
		opt = Optional.of(row);
		list = new ArrayList<>();
        list.add(row);		
	}

	@Test
	public void TestinsertCostCollector() {
		        
		Mockito.when(db.run(any(CqnInsert.class))).thenReturn(result);
		Mockito.when(result.listOf(CostCollectors.class)).thenReturn(listofCostCol);
		Mockito.when(result.first()).thenReturn(opt);
		Mockito.when(result.list()).thenReturn(list);
		Mockito.when(row.get("ID")).thenReturn("I123");

		assertEquals(costCollector, costColDao.insertCostCollector(costCollector));
	}

	@Test
	public void testinsertCostCollectorForNullValues() {
		Mockito.when(db.run(any(CqnInsert.class))).thenReturn(result);
		assertNull(costColDao.insertCostCollector(costCollector));
	}
	
	@Test
	public void TestupdateCostCollector() {
		Mockito.when(db.run(any(CqnUpdate.class))).thenReturn(result);
		Mockito.when(result.listOf(CostCollectors.class)).thenReturn(listofCostCol);
		Mockito.when(result.first()).thenReturn(opt);
		Mockito.when(result.list()).thenReturn(list);
		Mockito.when(row.get("ID")).thenReturn("I123");

		assertEquals(costCollector, costColDao.updateCostCollector(costCollector));
	}

	@Test
	public void testupdateCostCollectorForNullValues() {

		Mockito.when(db.run(any(CqnUpdate.class))).thenReturn(result);
		assertNull(costColDao.updateCostCollector(costCollector));
	}
	
	@Test
	public void TestdeleteCostCollector() {		
        
		Mockito.when(db.run(any(CqnDelete.class))).thenReturn(result);
		Mockito.when(result.listOf(CostCollectors.class)).thenReturn(listofCostCol);
		Mockito.when(result.first()).thenReturn(opt);
		Mockito.when(result.list()).thenReturn(list);
		Mockito.when(row.get("ID")).thenReturn("I123");

		assertEquals(costCollector, costColDao.deleteCostCollector(costCollector));
	}

	@Test
	public void testdeleteCostCollectorForNullValue() {

		Mockito.when(db.run(any(CqnDelete.class))).thenReturn(result);
		assertNull(costColDao.deleteCostCollector(costCollector));
	}
	
	@Test
	public void TestselectTransferToClaimCostCollector() {		
        
		Mockito.when(db.run(any(CqnSelect.class))).thenReturn(result);
		Mockito.when(result.listOf(CostCollectors.class)).thenReturn(listofCostCol);
		Mockito.when(result.first()).thenReturn(opt);
		Mockito.when(result.list()).thenReturn(list);
		Mockito.when(row.get("ID")).thenReturn("I123");

		assertEquals(result, costColDao.selectTransferToClaimCostCollector("C123"));
	}
	
	
	@Test
	public void TestselectAllCostCollector() {		
        
		Mockito.when(db.run(any(CqnSelect.class))).thenReturn(result);
		Mockito.when(result.listOf(CostCollectors.class)).thenReturn(listofCostCol);
		Mockito.when(result.first()).thenReturn(opt);
		Mockito.when(result.list()).thenReturn(list);
		Mockito.when(row.get("ID")).thenReturn("I123");

		assertEquals(result, costColDao.selectAllCostCollector("C123"));
	}

	@Test
	public void testGetCostCollector() {
		Mockito.when(db.run(any(CqnSelect.class))).thenReturn(result);
		Mockito.when(result.listOf(CostCollectors.class)).thenReturn(listofCostCol);
		Mockito.when(result.first()).thenReturn(opt);
		Mockito.when(result.list()).thenReturn(list);
		Mockito.when(row.get("ID")).thenReturn("I123");

		assertEquals(result, costColDao.getCostCollector("C123"));
	}
}