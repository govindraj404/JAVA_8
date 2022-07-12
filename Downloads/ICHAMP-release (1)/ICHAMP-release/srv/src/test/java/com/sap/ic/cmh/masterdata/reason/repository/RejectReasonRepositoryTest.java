package com.sap.ic.cmh.masterdata.reason.repository;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;

import cds.gen.masterdataservice.Reasons;

public class RejectReasonRepositoryTest {

	@InjectMocks
	@Autowired
	private RejectReasonRepositoryImpl rejectReasonRepositoryImpl;

	@Mock
	PersistenceService db;
	@Mock
	Result result;

	private Reasons reasons;

	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		reasons = Struct.create(Reasons.class);
		reasons.setCode("code");
		reasons.setDescription("Description");
	}

	@Test
	public void testFetchReasonBasedOnCode() {
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		rejectReasonRepositoryImpl.fetchReasonBasedOnCode(reasons.getCode());

	}

}
