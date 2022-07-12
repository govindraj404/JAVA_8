package com.sap.ic.cmh.masterdata.salesorganization.persistency;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import com.sap.cds.Result;
import com.sap.cds.ql.cqn.CqnDelete;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.persistence.PersistenceService;

import cds.gen.masterdataservice.SalesOrganizations;

public class SalesOrganizationRespositoryImplTest {
	
	@InjectMocks
	@Autowired
	SalesOrganizationRespositoryImpl salesOrganizationRespositoryImpl;
	@Mock
	private PersistenceService mockDb;
	@Mock
	Result result;
	
	SalesOrganizations salesOrganizations;
	
	private List<String> salesOrganizationsIdList;
	private List<SalesOrganizations> salesOrganizationsList= new ArrayList<>();
	
	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		salesOrganizationsIdList = new ArrayList<>();
		salesOrganizationsIdList.add("111");

		salesOrganizations = SalesOrganizations.create();
		salesOrganizations.setId("111");
		salesOrganizations.setSalesOrganization("1000");
		salesOrganizationsList.add(salesOrganizations);

		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		when(mockDb.run(any(CqnDelete.class))).thenReturn(result);
		when(mockDb.run(any(CqnUpdate.class))).thenReturn(result);
	}
	
	@Test
	public void testDeleteInactiveSalesOrganization() {
		when(mockDb.run(any(CqnDelete.class))).thenReturn(result);
		salesOrganizationRespositoryImpl.deleteInactiveSalesOrganization(salesOrganizationsIdList);
	}

	@Test
	public void testGetSalesOrganizationMap() {
		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		List<String> salesOrgList = new ArrayList<>();
		salesOrgList.add("1000");
		salesOrganizationRespositoryImpl.getSalesOrganizationMap(salesOrgList);
	}

	@Test
	public void testGetSalesOrganizationDetailsBasedOnSalesOrgCode() {
		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		salesOrganizationRespositoryImpl.getSalesOrganizationDetailsBasedOnSalesOrgCode(salesOrganizations.getSalesOrganization());
	}
	@Test
	public void testGetSalesOrganizationDetailsBasedOnCodeList() {
		List<String> salesOrgList= new ArrayList<>();
		salesOrgList.add(salesOrganizations.getSalesOrganization());
		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		salesOrganizationRespositoryImpl.getSalesOrganizationDetailsBasedOnCodeList(salesOrgList);
		
	}

    @Test
    public void testgetSalesOrganizationById(){
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        salesOrganizationRespositoryImpl.getSalesOrganizationById("111");
    }

}
