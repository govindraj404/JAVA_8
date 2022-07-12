package com.sap.ic.cmh.masterdata.division.persistency;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.sap.cds.Result;
import com.sap.cds.ql.cqn.CqnDelete;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.persistence.PersistenceService;

import cds.gen.masterdataservice.DistributionChannels;
import cds.gen.masterdataservice.Divisions;

public class DivisionRepositoryImplTest {
	
	@InjectMocks
	DivisionRepositoryImpl divisionRepositoryImpl;
	@Mock
	private PersistenceService mockDb;
	@Mock
	Result result;

	Divisions divisions;
	private List<String> divisionIdList;
	private List<Divisions> divisionsList = new ArrayList<>();
	
	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		divisionIdList = new ArrayList<>();
		divisionIdList.add("1000");

		divisions = Divisions.create();
		divisions.setId("111");
		divisions.setSalesDivision("1000");
		divisionsList.add(divisions);

		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		when(mockDb.run(any(CqnDelete.class))).thenReturn(result);
		when(mockDb.run(any(CqnUpdate.class))).thenReturn(result);
	}
	
	@Test
	public void testDeleteSalesDivision() {
		when(mockDb.run(any(CqnDelete.class))).thenReturn(result);
		divisionRepositoryImpl.deleteSalesDivision(divisionIdList);
	}

	@Test
	public void testGetDistributionChannelMap() {
		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		List<String> divisionList = new ArrayList<>();
		divisionList.add("1000");
		List<String> salesOrgList = new ArrayList<>();
		salesOrgList.add("1000");
		divisionRepositoryImpl.getDivisionMap(divisionList, salesOrgList);
	}

	@Test
	public void testGetDetailsBasedOnDistributionChannelAndSalesOrg() {
		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		divisionRepositoryImpl.getDivisionDetailsBasedOnDivisionAndSalesOrg("1000", "1000");
	}

	@Test
	public void testGetDivisionById() {
		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		divisionRepositoryImpl.getDivisionIDandSalesIDById("123");
	}

}
