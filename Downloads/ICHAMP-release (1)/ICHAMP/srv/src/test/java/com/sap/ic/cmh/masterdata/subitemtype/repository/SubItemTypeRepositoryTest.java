package com.sap.ic.cmh.masterdata.subitemtype.repository;

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

import cds.gen.masterdataservice.SubItemTypes;

public class SubItemTypeRepositoryTest {

	@InjectMocks
	@Autowired
	SubItemTypeRepositoryImpl subItemTypeRepositoryImpl;
	@Mock
	PersistenceService db;
	@Mock
	Result result;
	@Mock
	CqnSelect select;

	private SubItemTypes subItemType;

	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		subItemType = Struct.create(SubItemTypes.class);

		subItemType.setCode("10000001");
		subItemType.setDescription("Henry-Strasse");
		subItemType.setItemTypeCode("70839");
	}

	@Test
	public void testFetchSubItemTypes() {
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		subItemTypeRepositoryImpl.fetchSubItemTypes(subItemType.getCode());
		
	}

}
