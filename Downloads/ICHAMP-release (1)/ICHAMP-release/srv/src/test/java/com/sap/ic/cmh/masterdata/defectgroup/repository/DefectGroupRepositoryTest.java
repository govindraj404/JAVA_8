package com.sap.ic.cmh.masterdata.defectgroup.repository;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

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
import com.sap.ic.cmh.masterdata.defectcode.repository.DefectCodeRepositoryImpl;

import cds.gen.masterdataservice.DefectCodes;
import cds.gen.masterdataservice.DefectGroups;

public class DefectGroupRepositoryTest {

	@InjectMocks
	@Autowired
	private DefectGroupRepositoryImpl defectGroupRepositoryImpl;

	@Mock
	PersistenceService db;
	@Mock
	Result result;

	private DefectGroups group;

	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);

		group = Struct.create(DefectGroups.class);
		group.setCode("QM-E");
		group.setDescription("test");
	}

	@Test
	public void testFetchDefectGroupCode() {
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		defectGroupRepositoryImpl.fetchDefectGroupCode(group.getCode());
	}

}
