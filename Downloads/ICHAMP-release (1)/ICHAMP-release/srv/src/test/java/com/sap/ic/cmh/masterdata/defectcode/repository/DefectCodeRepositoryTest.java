package com.sap.ic.cmh.masterdata.defectcode.repository;

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

import cds.gen.masterdataservice.DefectCodes;
import cds.gen.masterdataservice.DefectGroups;

public class DefectCodeRepositoryTest {

	@InjectMocks
	@Autowired
	private DefectCodeRepositoryImpl defectCodeRepositoryImpl;

	@Mock
	PersistenceService db;
	@Mock
	Result result;

	private DefectCodes codeItem;
	private DefectGroups group;

	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);

		group = Struct.create(DefectGroups.class);
		group.setCode("QM-E");
		group.setDescription("test");
		codeItem = Struct.create(DefectCodes.class);
		codeItem.setCode("1");
		codeItem.setDescription("test");
		codeItem.setDefectGroupCode(group.getCode());
	}

	 @Test
	 public void testFetchDefectCode() {
	 when(db.run(any(CqnSelect.class))).thenReturn(result);
	 defectCodeRepositoryImpl.fetchDefectCode(codeItem.getCode(),codeItem.getDefectGroupCode());

	 }

	 @Test
	 public void testFetchDefectCodeByDefectGroup(){
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		defectCodeRepositoryImpl.fetchDefectCodeByDefectGroup(codeItem.getDefectGroupCode());
	 }

}
