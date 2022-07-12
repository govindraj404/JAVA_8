package com.sap.ic.cmh.masterdata.unitofmeasure.persistency;

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

import cds.gen.masterdataservice.DefectCodes;
import cds.gen.masterdataservice.DefectGroups;
import cds.gen.masterdataservice.UnitOfMeasures;

public class UnitOfMeasureRepositoryTest {
	
	@InjectMocks
	@Autowired
	UnitOfMeasureRepositoryImpl unitOfMeasureRepositoryImpl;
	
	@Mock
	PersistenceService db;
	@Mock
	Result result;
	
	private UnitOfMeasures unitOfMeasures;
	
	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		
		unitOfMeasures = Struct.create(UnitOfMeasures.class);
		unitOfMeasures.setCode("ST");
		unitOfMeasures.setISOCode("PCE");
	}
	
	@Test
	public void testGetUnitOfMeasureDetails() {
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		unitOfMeasureRepositoryImpl.getUnitOfMeasureDetails(unitOfMeasures.getCode());
	}

}
