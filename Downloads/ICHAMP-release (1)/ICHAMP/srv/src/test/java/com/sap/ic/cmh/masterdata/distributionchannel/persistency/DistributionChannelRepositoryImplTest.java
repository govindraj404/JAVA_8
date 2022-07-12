package com.sap.ic.cmh.masterdata.distributionchannel.persistency;

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

import cds.gen.masterdataservice.DistributionChannels;

public class DistributionChannelRepositoryImplTest {

	@InjectMocks
	@Autowired
	DistributionChannelRepositoryImpl distributionChannelRepositoryImpl;

	@Mock
	private PersistenceService mockDb;
	@Mock
	Result result;

	DistributionChannels distributionChannels;
	private List<String> distributionChannelIdList;
	private List<DistributionChannels> distributionChannelsList = new ArrayList<>();

	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		distributionChannelIdList = new ArrayList<>();
		distributionChannelIdList.add("1000");

		distributionChannels = DistributionChannels.create();
		distributionChannels.setId("111");
		distributionChannels.setDistributionChannel("1000");
		distributionChannelsList.add(distributionChannels);

		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		when(mockDb.run(any(CqnDelete.class))).thenReturn(result);
		when(mockDb.run(any(CqnUpdate.class))).thenReturn(result);
	}

	@Test
	public void testDeleteInactiveDistributionChannel() {
		when(mockDb.run(any(CqnDelete.class))).thenReturn(result);
		distributionChannelRepositoryImpl.deleteInactiveDistributionChannel(distributionChannelIdList);
	}

	@Test
	public void testGetDistributionChannelMap() {
		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		List<String> distributionChannelList = new ArrayList<>();
		distributionChannelList.add("1000");
		List<String> salesOrgList = new ArrayList<>();
		salesOrgList.add("1000");
		distributionChannelRepositoryImpl.getDistributionChannelMap(distributionChannelList, salesOrgList);
	}

	@Test
	public void testGetDetailsBasedOnDistributionChannelAndSalesOrg() {
		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		distributionChannelRepositoryImpl.getDetailsBasedOnDistributionChannelAndSalesOrg("1000", "1000");
	}

	@Test
	public void testGetActiveComplaintsInDistributionChannels() {
		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		distributionChannelRepositoryImpl.getActiveComplaintsInDistributionChannels(distributionChannelIdList);
	}

    @Test
	public void testGetDistributionChannelById() {
		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		distributionChannelRepositoryImpl.getDistributionChannelById("123");
	}

}
