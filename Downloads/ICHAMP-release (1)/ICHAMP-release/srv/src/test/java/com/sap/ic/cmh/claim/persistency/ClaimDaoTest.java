package com.sap.ic.cmh.claim.persistency;

import cds.gen.claimservice.Claims;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnDelete;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import com.sap.cds.services.draft.DraftService;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.mockito.ArgumentMatchers.any;

public class ClaimDaoTest {
	
	 @Mock
	 DraftService draftService;
	
	@InjectMocks
	@Autowired
	ClaimDao claimCMHDao;

	@Mock
	protected PersistenceService mockDb;

	@Mock
	Result result;

	private Claims claim;
	private Claims claims;
	private Row row;
	private Optional<Row> opt;

	private List<Claims> boList = new ArrayList<>();

	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		claim = Struct.create(Claims.class);
		claim.setId("ClaimID");
		claim.setStatusCode("CLMCRTD");
		claim.setComplaintId("1234");
		claim.setCompanyId("111");
		boList.add(claim);

		claims = Struct.create(Claims.class);
		claims.setId(claim.getId());
		claims.setStatusCode(claim.getStatusCode());
		claims.setComplaintId(claim.getComplaintId());

		row = Struct.create(Row.class);

	}

	@Test
	public void testGetClaimBasedOnId() {
	Mockito.when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
	assertEquals(result, claimCMHDao.getClaimBasedOnId(claim.getId()));
	}

	@Test
	public void testGetClaim() {
	Mockito.when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
	claimCMHDao.getClaim(claims.getComplaintId());

	}
	@Test
	public void testGetActiveClaimBasedOnId() {
		Mockito.when(draftService.cancelDraft(any(CqnDelete.class))).thenReturn(result);
		claimCMHDao.getActiveClaimBasedOnId(claim.getId());
	}

	@Test
	public void testGetDraftClaimByComplaintID() {
		Mockito.when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		claimCMHDao.getDraftClaimByComplaintID(claims.getComplaintId());
	}
	@Test
	public void testCheckIfClaimExistsBasedOnNumber() {
	Mockito.when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
	claimCMHDao.checkIfClaimExistsBasedOnNumber("1234");

	}

	@Test
	public void testGetClaimStatusAndCompanyCode() {
	Mockito.when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
	assertEquals(result,
	claimCMHDao.getClaimStatusAndCompanyCode(claim.getId()));
	}
	
	@Test
	public void testDeleteDraftClaimByID() {
	Mockito.when(draftService.cancelDraft(any(CqnDelete.class))).thenReturn(result);
	claimCMHDao.deleteDraftClaimByID(claim.getId());	
	}

}
